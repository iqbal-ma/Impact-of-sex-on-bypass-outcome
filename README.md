# Sex differences in outcomes after STA–MCA bypass surgery for atherosclerotic cerebrovascular disease

**R analysis code for:**  
*Impact of biological sex on clinical outcomes following superficial temporal artery – middle cerebral artery bypass surgery in atherosclerotic cerebrovascular disease*

Iqbal et al., Charité – Universitätsmedizin Berlin, Germany

---

## Overview

This repository contains the statistical analysis code for a retrospective cohort study (n = 140) examining biological sex as an independent predictor of functional outcome after STA–MCA bypass surgery at Charité – Universitätsmedizin Berlin (2012–2025). The primary outcome was functional status at latest follow-up, assessed by the modified Rankin Scale (mRS) and analyzed using proportional odds regression.

Ethics approval: EA2/139/12 and EA2/178/18, Ethics Committee of Charité – Universitätsmedizin Berlin.

---

## Repository contents

| File | Description |
|------|-------------|
| `bypass_sex_analysis.R` | Complete analysis script (descriptive statistics, proportional odds regression, sensitivity analyses, CVRC subgroup) |

---

## Requirements

**R version:** ≥ 4.2.0  
**Required packages** (installed automatically by the script):

- `readxl` — data import  
- `MASS` — proportional odds regression (`polr`)  
- `nparcomp` — nonparametric comparisons  
- `ggplot2`, `ggalluvial` — visualization  
- `corrplot` — correlation matrix  
- `car`, `performance` — collinearity diagnostics (VIF)  
- `glmulti` — AICc-based variable selection  
- `brant` — proportional odds assumption test  

---

## Data availability

Patient data cannot be shared publicly due to data protection regulations and ethics committee requirements. Data are available from the corresponding author on reasonable request, subject to institutional data sharing agreements.

The script expects an Excel file at `data/bypass_data.xlsx`. Readers wishing to apply this code to their own data should update this path accordingly.

---

## How to run

1. Clone or download this repository  
2. Place the dataset at `data/bypass_data.xlsx`  
3. Open R (or RStudio) and run:

```r
source("bypass_sex_analysis.R")
```

Outputs (forest plot data, sensitivity analysis results, session info) are written to the `outputs/` folder, which is created automatically.

---

## Citation

If you use this code, please cite the associated publication (citation to be updated upon acceptance).
