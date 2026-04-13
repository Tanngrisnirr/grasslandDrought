# grasslandDrought

Streamlined analysis of grassland drought effects using mixed models.

## Quick Start (RStudio)

1. Clone or download this repository
2. Place your Excel file (`.xlsx`) in the `data/` folder
3. Open `grasslandDrought.Rproj` in RStudio
4. The data loads automatically!

```r
# Your data is already in 'data' variable
results <- analyze_treatment(data, "RootGrowth")

# With JSON export
results <- analyze_treatment(data, "RootGrowth", export_json = "output/results.json")
```

**Demo data included:** `data/picon_cochard_2021.xlsx` from [Zenodo](https://zenodo.org/records/4034903) (CC-BY-4.0)

## Overview

This package wraps the `nlme::lme()` workflow for analyzing grassland experiments with nested random effects (`block/ID`), as used in [Picon-Cochard et al. (2021)](https://doi.org/10.24072/pcjournal.54).

**Features:**
- Auto-detect column names (treatment, block, date, ID)
- Run treatment × date mixed models with `nlme::lme()`
- Post-hoc comparisons with `emmeans`
- Diagnostic plots
- Export results to JSON

## Data Format

Your Excel file should have columns for:
- **treatment**: experimental treatment (e.g., grazing intensity)
- **block**: experimental block
- **date**: sampling date
- **ID**: plot/subplot identifier
- **response variable**: e.g., RootGrowth, ANPP, BNPP

Column names are auto-detected (case-insensitive, common aliases supported).

## Functions

| Function | Description |
|----------|-------------|
| `import_urep()` | Import UREP Excel datasets |
| `calc_rswc()` | Calculate Relative Soil Water Content |
| `calc_ppet()` | Calculate P-PET water balance |
| `analyze_treatment()` | Run mixed model analysis |
| `export_json()` | Export results to JSON |

## Reference

Based on methodology from:

> Picon-Cochard C, Vassal N, Martin R, Herfurth D, Note P, Louault F (2021). Intra and inter-annual climatic conditions have stronger effect than grazing intensity on root growth of permanent grasslands. *Peer Community Journal*, 1:e43. [doi:10.24072/pcjournal.54](https://doi.org/10.24072/pcjournal.54)

## License

MIT
