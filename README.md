# Keeping Defenses Guessing: Does Unpredictable Play-Calling Improve NFL Offenses?

A drive-level analysis of play variation and EPA per play using nflfastR and PFF data, with a concave (diminishing-returns) result validated via permutation testing.

**[`Read the full write-up`](PlayVariation.pdf)**

## Summary

Play-calling variation has a concave relationship with offensive success: efficiency rises with variation up to a point, then declines. This holds across pass-heavy/run-heavy drive splits and across the early (2015-19) and late (2021-25) era drive splits, and survives a 5,000-iteration permutation test designed to rule out team-quality confounding as an alternative explanation.

## Repository Contents

- **`PlayVariation.Rmd`** - full source: data construction, modeling, diagnostics, and the write-up itself. This is the canonical version; knit to reproduce `PlayVariation.pdf`
- **`PlayVariation.R`** - a code-only extract of the above for a quick skim of the analysis without the prose. This file is derived, not hand-maintained - regenerate it after any change to the Rmd with:

```r
  knitr::purl("PlayVariation.Rmd", output = "PlayVariation.R", documentation = 0)
```
- **`PlayVariation.pdf`** - the knitted final report.

## Data Access
Play-by-play data is pulled live via the open-source `nflfastR`/`nflverse` API - no manual download needed for that portion, and it's fully redistributable/reproducible as-is.

Run- and pass-blocking grades come from Pro Football Focus (PFF), which is proprietary and not redistributable under its license terms. This repository does not include that data. To reproduce the blocking-grade portion of the model, you'll need your own PFF data export, formatted as columns `rblk_<year>` / `pblk_<year>` per team.

## Requirements
See [install_packages.R](install_packages.R)

## Citation
If referencing this analysis:
> Smith, K. (2026). *Keeping Defenses Guessing: Does Unpredictable Play-Calling Improve NFL Offenses?*

Sources cited within the report:
> Carl S, Baldwin B (2026). nflfastR: Functions to Efficiently Access NFL Play by Play Data. R package version 5.2.0.9012. https://nflfastr.com/

> Szabó, A., & Perez Ruix, F. (2021). Does home advantage without crowd exist in American football?

## Limitations
See the Limitations section of the report for caveats on clustering, the drive-length variation mechanical relationship, talent measured at the room level, and other modeling decisions.
