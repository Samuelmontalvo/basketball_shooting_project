# Basketball Shooting Force Plate Analysis

Analysis of ground reaction forces (GRF) during basketball shooting using force plate data. This project examines biomechanical differences between 2-point and 3-point shots and predictors of shooting success.

## Project Overview

**Objective:** Quantify biomechanical differences between 2PT and 3PT basketball shots and identify predictors of shooting accuracy using force plate measurements.

**Design:** Within-subject repeated measures (23 players × 20 trials each = 460 observations)

## Key Findings

### Biomechanical Differences (3PT vs 2PT)

| Outcome | Difference | 95% CI | p-value |
|---------|-----------|--------|---------|
| Jump Height | +5.44 cm | [4.98, 5.90] | <0.001 |
| Peak Propulsive Force | +181.8 N | [161.1, 202.6] | <0.001 |
| Peak Propulsive Power | +618.8 W | [525.9, 711.7] | <0.001 |
| Peak Braking Force | +152.7 N | [123.6, 181.9] | <0.001 |
| Peak Braking Power | +98.2 W | [58.2, 138.1] | <0.001 |

Players generate significantly higher force, power, and jump height for 3PT shots compared to 2PT shots.

### Shot Success

- **Overall make rate:** 64.1%
- **2PT make rate:** Higher than 3PT
- **3PT vs 2PT OR:** 0.40 [0.28, 0.58], p < 0.001

Three-point shots have ~60% lower odds of success compared to two-point shots after accounting for player clustering.

## Data

- **Source:** Force plate measurements during basketball shooting trials
- **Sample:** 23 collegiate basketball players
- **Trials:** 10 two-point shots + 10 three-point shots per player
- **Variables:** 18 columns including force, power, duration, and anthropometric measures

### Key Variables

| Variable | Description |
|----------|-------------|
| `player_id` | Player identifier (1-23) |
| `shot_type` | 2PT or 3PT |
| `made` | Shot outcome (0 = miss, 1 = made) |
| `jump_height` | Jump height (cm) |
| `peak_propulsive_force` | Maximum propulsive GRF (N) |
| `peak_propulsive_power` | Maximum propulsive power (W) |
| `peak_braking_force` | Maximum braking GRF (N) |
| `peak_braking_power` | Maximum braking power (W) |
| `position` | Playing position |
| `height` | Player height (cm) |
| `body_mass` | Player body mass (kg) |

## Repository Structure

```
basketball_shooting_project/
├── data/
│   ├── GRF Shooting SM[40].csv    # Raw data
│   └── grf_shooting_clean.csv     # Cleaned data
├── scripts/
│   ├── 01_clean_grf_data.Rmd      # Data cleaning
│   └── 02_eda_grf_shooting.Rmd    # Exploratory data analysis
├── analysis/
│   └── 02_stats_models.Rmd        # Statistical modeling (LMM, GEE)
├── results/
│   ├── lmm_results.rds            # Linear mixed model results
│   ├── success_model_results.rds  # Logistic model results
│   └── full_vs_qc_comparison.rds  # QC sensitivity analysis
└── figures/
    └── eda/                       # EDA visualizations
```

## Methods

### Data Cleaning
- Column standardization with `janitor::clean_names()`
- Player-level covariate propagation (height, mass, position)
- Quality control filtering (duration outliers, displacement artifacts)

### Statistical Models

**Continuous Outcomes (LMM):**
```r
lmer(outcome ~ shot_type + (1|player_id))
```
- Random intercepts for players to account for repeated measures
- Sensitivity analyses with random slopes and adjusted covariates

**Binary Outcome (GEE/GLMM):**
```r
geeglm(made ~ shot_type + jump_z + power_z + position,
       id = player_id, family = binomial, corstr = "exchangeable")
```
- Marginal models (GEE) with exchangeable correlation
- Subject-specific models (GLMM) for sensitivity

### Quality Control
- Excluded trials with braking/propulsive duration > 0.8s
- Excluded displacement depth outliers (IQR rule + within-player z > 3)
- Full vs QC dataset comparison for robustness

## Requirements

### R Packages
```r
# Core
tidyverse, janitor, here

# Mixed Models
lme4, lmerTest, broom.mixed

# GEE
geepack, sandwich

# Visualization & Tables
ggplot2, plotly, DT, knitr

# Model Diagnostics
performance, emmeans
```

## Usage

1. Clone the repository
2. Open `basketball_shooting_project.Rproj` in RStudio
3. Run scripts in order:
   - `scripts/01_clean_grf_data.Rmd` - Data cleaning
   - `scripts/02_eda_grf_shooting.Rmd` - EDA
   - `analysis/02_stats_models.Rmd` - Statistical models

## Author

Samuel Montalvo

## License

This project is for academic/research purposes.
