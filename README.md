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

## Overview

This package wraps the `nlme::lme()` workflow for analyzing grassland experiments with nested random effects (`block/ID`), as used in [Picon-Cochard et al. (2021)](https://doi.org/10.24072/pcjournal.54).

## Demo Data

This package includes demo data from the UREP grassland experiment:

**Source:** Picon-Cochard C, Vassal N, Martin R, Herfurth D, Note P, Louault F (2020).
Dataset for: Intra and inter-annual climatic conditions have stronger effect than
grazing intensity on root growth of permanent grasslands [Data set]. Zenodo.
https://doi.org/10.5281/zenodo.4034903

**License:** [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/) — You are free
to share and adapt this data with attribution.

**Original Study:** Picon-Cochard C, Vassal N, Martin R, Herfurth D, Note P, Louault F
(2021). Intra and inter-annual climatic conditions have stronger effect than grazing
intensity on root growth of permanent grasslands. *Peer Community Journal*, 1:e43.
[doi:10.24072/pcjournal.54](https://doi.org/10.24072/pcjournal.54)

The demo file `data/picon_cochard_2021.xlsx` is redistributed under the terms of the
CC-BY-4.0 license. We thank the authors for making their data openly available.

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

## Exploratory PCA Analysis

For Principal Component Analysis as performed in Picon-Cochard et al. (2021), use the [FactoMineR](https://cran.r-project.org/package=FactoMineR) package:

```r
library(FactoMineR)

# Select variables for PCA (adapt to your dataset)
pca_vars <- c("RSWC", "Tsoil", "Diam", "SRA", "RootMass", "BNPP", "Height", "LDMC", "ANPP")
pca_data <- data[, pca_vars]

# Run PCA with scaling
res.pca <- PCA(pca_data, scale.unit = TRUE, graph = TRUE)

# View results
summary(res.pca)
```

## Reference

Based on methodology from:

> Picon-Cochard C, Vassal N, Martin R, Herfurth D, Note P, Louault F (2021). Intra and inter-annual climatic conditions have stronger effect than grazing intensity on root growth of permanent grasslands. *Peer Community Journal*, 1:e43. [doi:10.24072/pcjournal.54](https://doi.org/10.24072/pcjournal.54)

## License

MIT
