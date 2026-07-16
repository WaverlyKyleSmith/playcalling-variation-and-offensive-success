library(tidyverse); library(nflverse); library(entropy); library(dplyr); library(kableExtra); library(lme4); library(sandwich); library(lmtest); library(car); library(caret); 
library(modelsummary) 

pv_data <- load_pbp(seasons = 2015:2025) %>%
  filter(!startsWith(game_id, "2020_"),
         play_type %in% c("run", "pass"),
         !is.na(epa), !is.na(posteam), !is.na(drive),
         qb_kneel == 0, qb_spike == 0, two_point_attempt == 0) %>%
  mutate(
    season = as.integer(substr(game_id, 1, 4)),
    passer_player_name = gsub("\\.", ". ", passer_player_name),
    posteam = ifelse(posteam == "LV", "LVR", 
                     ifelse(posteam == "LA", "LAR", posteam))
  ) %>%
  dplyr::select(
    game_id, season, week, posteam, defteam, drive, play_id, play_type,
    down, ydstogo, yardline_100, epa, pass_location, air_yards, 
    run_location, run_gap, score_differential, half_seconds_remaining,
    wp, roof, temp, wind, passer_player_name, passer_player_id, receiver_player_id, touchdown,
    first_down, success
  )

#2020 eliminated initially due to opt outs, no fans, a generally abnormal year. May be included back in later for robustness or general testing.
#temp, wind, bad weather don't meaningfully differ between drives, so they are game level variables.

qbz <- pv_data %>%
  filter(play_type == "pass", !is.na(passer_player_id)) %>%
  group_by(season, passer_player_id, passer_player_name) %>%
  summarize(qb_epa = mean(epa, na.rm = T), .groups = "drop") %>%
  group_by(season) %>%
  mutate(qbz = scale(qb_epa)[, 1]) %>%
  ungroup() %>%
  dplyr::select(season, passer_player_id, passer_player_name, qbz)

rbz <- pv_data %>%
  filter(play_type == "run") %>%
  group_by(season, posteam) %>%
  summarize(rush_epa = mean(epa, na.rm = T), .groups = "drop") %>%
  group_by(season) %>%
  mutate(rbz = scale(rush_epa)[, 1]) %>%
  ungroup() %>%
  dplyr::select(season, posteam, rbz)

wrz <- pv_data %>%
  filter(play_type == "pass", !is.na(receiver_player_id)) %>%
  group_by(season, posteam, receiver_player_id) %>%
  summarize(
    rec_epa = mean(epa, na.rm = T),
    targets = n(),
    .groups = "drop"
  ) %>%
  group_by(season, posteam) %>%
  summarize(
    wrz_raw = weighted.mean(rec_epa, w = targets, na.rm = T),
    .groups = "drop"
  ) %>%
  group_by(season) %>%
  mutate(wrz = scale(wrz_raw)[, 1]) %>%
  ungroup() %>%
  dplyr::select(season, posteam, wrz)

defz <- pv_data %>%
  group_by(season, defteam) %>%
  summarize(def_epa = mean(epa, na.rm = T), .groups = "drop") %>%
  group_by(season) %>%
  mutate(defz = -scale(def_epa)[, 1]) %>% #higher values = better defenses
  ungroup() %>%
  dplyr::select(season, defteam, defz)


library(readxl)

blocking_grades <- read_xlsx("blocking_grades.xlsx") %>%
  pivot_longer(
    cols = -team,
    names_to = c(".value", "season"),
    names_pattern = "(rblk|pblk)_(\\d{4})"
  ) %>%
  mutate(season = as.integer(season)) %>%
  filter(season != 2020) %>%
  rename(rblk_grade = rblk, pblk_grade = pblk) %>%
  group_by(season) %>%
  mutate(rblk_grade = scale(rblk_grade)[, 1], pblk_grade = scale(pblk_grade)[, 1])

std_entropy <- function(x){
  tbl <- table(x)
  if(length(tbl) <= 1) return(0)
  H <- entropy(tbl, unit = "log2")
  H_max <- log2(length(tbl))
  H / H_max
}

drives <- pv_data %>%
  group_by(game_id, season, week, posteam, defteam, drive) %>%
  arrange(play_id, .by_group = T) %>%
  summarize(
    epa_per_play = mean(epa, na.rm = T),
    nplays = n(),
    npass = sum(play_type == "pass"),
    nrun = sum(play_type == "run"),
    pass_rate = npass / nplays,
    play_category = list({
      category <- case_when(
          play_type == "pass" & air_yards <= 7 & pass_location == "left" ~ "pass_short_left",
          play_type == "pass" & air_yards <= 7 & pass_location == "middle" ~ "pass_short_middle",
          play_type == "pass" & air_yards <= 7 & pass_location == "right" ~ "pass_short_right",
          play_type == "pass" & air_yards > 7 & air_yards <= 20 & pass_location == "left" ~ "pass_medium_left",
          play_type == "pass" & air_yards > 7 & air_yards <= 20 & pass_location == "middle" ~ "pass_medium_middle",
          play_type == "pass" & air_yards > 7 & air_yards <= 20 & pass_location == "right" ~ "pass_medium_right",
          play_type == "pass" & air_yards > 20 & pass_location == "left" ~ "pass_deep_left",
          play_type == "pass" & air_yards > 20 & pass_location == "middle" ~ "pass_deep_middle",
          play_type == "pass" & air_yards > 20 & pass_location == "right" ~ "pass_deep_right",
          
          play_type == "run" & run_location == "left" & run_gap == "end" ~ "run_left_d",
          play_type == "run" & run_location == "left" & run_gap == "tackle" ~ "run_left_c",
          play_type == "run" & run_location == "left" & run_gap == "guard" ~ "run_left_b",
          play_type == "run" & run_location == "right" & run_gap == "end" ~ "run_right_d",
          play_type == "run" & run_location == "right" & run_gap == "tackle" ~ "run_right_c",
          play_type == "run" & run_location == "right" & run_gap == "guard" ~ "run_right_b",
          play_type == "run" & run_location == "middle" ~ "run_a",
          T ~ NA_character_
        )
      category[!is.na(category)]
    }),
    variation = std_entropy(unlist(play_category)),
    score_diff = first(score_differential),
    one_possession = as.integer(abs(first(score_differential)) <= 8),
    short_time = as.integer(first(half_seconds_remaining) <= 300),
    clutch = one_possession * short_time,
    start_field_position = first(yardline_100), #yardline is distance from opponent endzoe. So 80 is own 20.
    temp = first(temp),
    wind = first(wind),
    bad_weather = as.integer(
      (!is.na(first(wind)) & first(wind) >= 20) | (!is.na(first(temp)) & first(temp) <= 32)
    ),
    passer_id = first(na.omit(passer_player_id)),
    passer_player = first(na.omit(passer_player_name)),
    .groups = "drop"
  ) %>%
  filter(nplays >= 2) %>% #one play drives can't have intra-drive variation
  dplyr::select(-play_category)

drives <- drives %>%
  left_join(qbz, by = c("season", "passer_id" = "passer_player_id")) %>%
  left_join(rbz, by = c("season", "posteam")) %>%
  left_join(defz, by = c("season", "defteam")) %>%
  left_join(wrz , by = c("season", "posteam")) %>%
  left_join(blocking_grades, by = c("season", "posteam" = "team")) %>%
  mutate(
    team_season = paste(posteam, season, sep = "_"),
    season = factor(season)
  )

pct_pass_only <- sum(drives$nrun == 0) / nrow(drives)
pct_rush_only <- sum(drives$npass == 0) / nrow(drives)
pct_mixed <- sum(drives$npass > 0 & drives$nrun > 0)/nrow(drives)
pct_short_drives <- mean(drives$nplays <= 4)

## ---- eda-density-early ----
ggplot(drives, aes(x = epa_per_play)) +
  geom_density(fill = "#c0392b", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of EPA per Play", x = "EPA per play", y = "Density")

tibble(
  Category = c("Mixed", "Pass-only", "Rush-only"),
  `% of Drives` = scales::percent(c(pct_mixed, pct_pass_only, pct_rush_only), accuracy = 0.1)
) %>%
  knitr::kable(caption = "Drive composition across the sample") %>%
  kable_styling()

## ---- OLS ----


scaled_drives <- drives %>%
  mutate(
    variation = variation - mean(variation, na.rm = T),
    pass_rate = scale(pass_rate)[, 1],
    score_diff = scale(score_diff)[, 1],
    start_field_pos = scale(start_field_position)[, 1],
    temp = scale(temp)[, 1],
    wind = scale(wind)[, 1],
    season = factor(season)
  ) %>%
  dplyr::select(-start_field_position)


m1 <- lm(
  epa_per_play ~
    variation + I(variation^2) + pass_rate + nplays + I(nplays^2) +
    one_possession + short_time + start_field_pos +
    qbz + rbz + wrz + defz + rblk_grade + pblk_grade +
    bad_weather  + variation:one_possession + 
    variation:short_time + variation:qbz + 
    variation:rbz + variation:wrz + variation:defz +
    variation:rblk_grade +
    season,
  data = scaled_drives
  )

## ---- model-diagnostics-hidden ----

bp <- bptest(m1)
dw <- dwtest(m1)
vif_tbl <- tibble(term = names(vif(m1)), VIF = round(vif(m1), 2))

## ---- robust-se ----
robust_se <- coeftest(m1, vcov = vcovHC(m1, type = "HC3"))

set.seed(42)
cv <- trainControl(method = "cv", number = 50)
m1_cv <- train(
  epa_per_play ~
    variation + I(variation^2) + pass_rate + nplays + I(nplays^2) +
    one_possession + short_time + start_field_pos +
    qbz + rbz + wrz + defz + rblk_grade + pblk_grade +
    bad_weather  + variation:one_possession + 
    variation:short_time + variation:qbz + 
    variation:rbz + variation:wrz + variation:defz +
    variation:rblk_grade +
    season,
  data = na.omit(scaled_drives),
  method = "lm",
  trControl = cv
)

in_sample_r2 <- summary(m1)$r.squared
out_sample_rmse <- m1_cv$results$RMSE
out_sample_r2 <- m1_cv$results$Rsquared
resid_se <- summary(m1)$sigma

## ---- permutation-test ----
nperm <- 5000
model_data <- na.omit(scaled_drives)
observed_coef <- coef(m1)["I(variation^2)"]
perm_coefs <- numeric(nperm)

set.seed(525)
for (i in 1:nperm){
  permuted_data <- model_data %>%
    group_by(team_season) %>%
    mutate(variation = sample(variation)) %>%
    ungroup()
  
  perm_fit <- lm(
    epa_per_play ~
      variation + I(variation^2) + pass_rate + nplays + I(nplays^2) +
      one_possession + short_time + start_field_pos +
      qbz + rbz + wrz + defz + rblk_grade + pblk_grade +
      bad_weather  + variation:one_possession + 
      variation:short_time + variation:qbz + 
      variation:rbz + variation:wrz + variation:defz +
      variation:rblk_grade +
      season,
    data = permuted_data
  )
  
  perm_coefs[i] <- coef(perm_fit)["I(variation^2)"]
}

p_value <- mean(abs(perm_coefs) >= abs(observed_coef))
n_exceeding <- sum(abs(perm_coefs) >= abs(observed_coef))

## ---- permutation-plot ----
ggplot(data.frame(perm_coefs = perm_coefs), aes(x = perm_coefs)) +
  geom_histogram(bins = 60, fill = "red", alpha = 0.7) +
  geom_vline(xintercept = observed_coef, color = "#c0392b") +
  theme_minimal() +
  labs(
    title = "Permutation Test: Squared Variation Coefficient",
    x = "Coefficient on Variation (shuffled data)",
    y = "Count"
  )


variation_coef <- coef(m1)["variation"]

modelsummary(
  list("Full Model" = m1),
  vcov = vcovHC(m1, type = "HC3"),
  stars = c('*' = .05, '**' = .01, '***' = .001),
  estimate = "{estimate} ({std.error}){stars}",
  statistic = NULL,
  align = "ll",
  coef_omit = "season",
  coef_rename = c(
    "variation" = "Variation",
    "I(variation^2)" = "Squared Variation",
    "pass_rate" = "Pass Rate",
    "nplays" = "Drive Length",
    "I(nplays^2)" = "Squared Drive Length",
    "one_possession" = "One Possession (OP)",
    "short_time" = "Short Time (ST)",
    "start_field_pos" = "Starting Field Position",
    "qbz" = "Quarterback Talent (QBZ)",
    "rbz" = "Rushing Talent (RBZ)",
    "wrz" = "Receiving Talent (WRZ)",
    "defz" = "Defensive Talent (DFZ)",
    "rblk_grade" = "Run Blocking (RBLK)",
    "pblk_grade" = "Pass Blocking (PBLK)",
    "bad_weather" = "Bad Weather",
    "variation:one_possession" = "Variation x OP",
    "variation:short_time" = "Variation x ST",
    "variation:qbz" = "Variation x QBZ",
    "variation:rbz" = "Variation x RBZ",
    "variation:wrz" = "Variation x WRZ",
    "variation:defz" = "Variation x DFZ",
    "variation:rblk_grade" = "Variation x RBLK"
  ),
  gof_omit = "IC|Log|Adj|Errors",
  output = "kableExtra",
  title = "Full Model Results (Robust HC3 Standard Errors)"
) %>%
  add_footnote("Season fixed effects included but omitted from the table for brevity.", notation = "none") %>%
  kable_styling(latex_options = "hold_position")

## ---- subgroup-models ----
pass_heavy <- model_data %>% filter(pass_rate > 0.70, nplays >= 3)
run_heavy <- model_data %>% filter(pass_rate < 0.30, nplays >= 3)


m_pass <- lm(
  epa_per_play ~ variation + I(variation^2) + pass_rate + nplays + I(nplays^2) +
    one_possession + short_time + start_field_pos +
    qbz + rbz + wrz + defz + rblk_grade + pblk_grade +
    bad_weather + variation:one_possession +
    variation:short_time + variation:qbz +
    variation:rbz + variation:wrz + variation:defz +
    variation:rblk_grade + season,
  data = pass_heavy
)

m_rush <- lm(
  epa_per_play ~ variation + I(variation^2) + pass_rate + nplays + I(nplays^2) +
    one_possession + short_time + start_field_pos +
    qbz + rbz + wrz + defz + rblk_grade + pblk_grade +
    bad_weather + variation:one_possession +
    variation:short_time + variation:qbz +
    variation:rbz + variation:wrz + variation:defz +
    variation:rblk_grade + season,
  data = run_heavy
)

early <- model_data %>% filter(season %in% c(2015:2019))
late <- model_data %>% filter(season %in% c(2021:2025))

m_early <- lm(
  epa_per_play ~ variation + I(variation^2) + pass_rate + nplays + I(nplays^2) +
    one_possession + short_time + start_field_pos +
    qbz + rbz + wrz + defz + rblk_grade + pblk_grade +
    bad_weather + variation:one_possession +
    variation:short_time + variation:qbz +
    variation:rbz + variation:wrz + variation:defz +
    variation:rblk_grade + season,
  data = early
)

m_late <- lm(
  epa_per_play ~ variation + I(variation^2) + pass_rate + nplays + I(nplays^2) +
    one_possession + short_time + start_field_pos +
    qbz + rbz + wrz + defz + rblk_grade + pblk_grade +
    bad_weather + variation:one_possession +
    variation:short_time + variation:qbz +
    variation:rbz + variation:wrz + variation:defz +
    variation:rblk_grade + season,
  data = late
)

pass_ct <- coeftest(m_pass, vcov = vcovHC(m_pass, type = "HC3"))
rush_ct <- coeftest(m_rush, vcov = vcovHC(m_rush, type = "HC3"))
early_ct <- coeftest(m_early, vcov = vcovHC(m_early, type = "HC3"))
late_ct <- coeftest(m_late, vcov = vcovHC(m_late, type = "HC3"))

sig_stars <- function(p){
  case_when(
    p < .001 ~ "***",
    p < .01 ~ "**",
    p < .05 ~ "*",
    T ~ ""
  )
}

fmt_cell <- function(ct, term){
  sprintf("%.3f (%.3f)%s", ct[term, "Estimate"], ct[term, "Std. Error"],
          sig_stars(ct[term, "Pr(>|t|)"]))
}

subgroup_summary <- data.frame(
  Model = c("Pass-heavy drives (>70% pass)", "Run-heavy drives (<30% pass)",
            "Early era (2015-2019)", "Late era (2021-2025)"),
  N = c(nrow(pass_heavy), nrow(run_heavy), nrow(early), nrow(late)),
  `Variation` = c(fmt_cell(pass_ct, "variation"), fmt_cell(rush_ct, "variation"),
                  fmt_cell(early_ct, "variation"), fmt_cell(late_ct, "variation")),
  `Variation Squared` = c(fmt_cell(pass_ct, "I(variation^2)"), fmt_cell(rush_ct, "I(variation^2)"),
                           fmt_cell(early_ct, "I(variation^2)"), fmt_cell(late_ct, "I(variation^2)"))
)

subgroup_summary %>%
  kable(
    caption = "Variation and Variation² Across Subgroups",
    align = c("l", "r", "r", "r")
  ) %>%
  kable_styling() %>%
  footnote(general = "*p<.05, **p<.01, ***p<.001. Estimate (Robust SE) shown for each term.",
           general_title = "")

## ---- marginal-effect-full ----
variation_range <- seq(min(scaled_drives$variation, na.rm = T), max(scaled_drives$variation, na.rm = T), length.out = 100)

pred_grid <- data.frame(
  variation = variation_range,
  pass_rate = 0, nplays = mean(scaled_drives$nplays, na.rm = T),
  one_possession = mean(scaled_drives$one_possession, na.rm = T),
  short_time = mean(scaled_drives$short_time, na.rm = T),
  start_field_pos = 0,
  qbz = 0, rbz = 0, wrz = 0, defz = 0, rblk_grade = 0, pblk_grade = 0,
  bad_weather = mean(scaled_drives$bad_weather, na.rm = T),
  season = "2025"
)

preds <- predict(m1, newdata = pred_grid, interval = "confidence")
pred_grid <- cbind(pred_grid, preds)

ggplot(pred_grid, aes(x = variation, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "#c0392b", alpha = 0.2) +
  geom_line(color = "#c0392b", linewidth = 1) +
  theme_minimal() +
  labs(title = "Predicted EPA/Play vs. Variation", x = "Variation (centered)", y = "Predicted EPA per play")

## ---- marginal-effect-subgroup-passrun ----
pred_grid_pr <- rbind(
  transform(pred_grid[, !(names(pred_grid) %in% c("fit","lwr","upr"))], group = "Pass-heavy"),
  transform(pred_grid[, !(names(pred_grid) %in% c("fit","lwr","upr"))], group = "Run-heavy")
)

pred_grid_pr$fit <- c(predict(m_pass, newdata = pred_grid), predict(m_rush, newdata = pred_grid))

ggplot(pred_grid_pr, aes(x = variation, y = fit, color = group)) + 
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("Pass-heavy" = "#c0392b", "Run-heavy" = "grey40")) +
  theme_minimal() +
  labs(title = "Predicted EPA/Play vs. Variation, by Drive Type", x = "Variation (centered)", y = "Predicted EPA per play", color = NULL)

## ---- marginal-effect-subgroup-era ----
make_pred_grid <- function(season_value){
  data.frame(
    variation = variation_range,
    pass_rate = 0, nplays = mean(scaled_drives$nplays, na.rm = T),
    one_possession = mean(scaled_drives$one_possession, na.rm = T),
    short_time = mean(scaled_drives$short_time, na.rm = T),
    start_field_pos = 0,
    qbz = 0, rbz = 0, wrz = 0, defz = 0, rblk_grade = 0, pblk_grade = 0,
    bad_weather = mean(scaled_drives$bad_weather, na.rm = T),
    season = season_value
  )
}

pred_grid_early <- make_pred_grid("2017")
pred_grid_late  <- make_pred_grid("2023")

pred_grid_era <- rbind(
  transform(pred_grid_early, group = "2015-2019"),
  transform(pred_grid_late, group = "2021-2025")
)

pred_grid_era$fit <- c(predict(m_early, newdata = pred_grid_early), predict(m_late, newdata = pred_grid_late))

ggplot(pred_grid_era, aes(x = variation, y = fit, color = group)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("2015-2019" = "grey40", "2021-2025" = "#c0392b")) +
  theme_minimal() +
  labs(title = "Predicted EPA/Play vs. Variation, by Era", x = "Variation (centered)", y = "Predicted EPA per play", color = NULL)

