#### === # === # === # === # === # === # === # === # === 
# Impact of biological sex on clinical outcomes following STA-MCA bypass in atherosclerotic cerebrovascular disease
# Analysis code
# Author: Mohammad Asif Iqbal
#### === # === # === # === # === # === # === # === # === 

set.seed(2026)

# Required packages
required_packages = c(
  "readxl",
  "MASS",
  "nparcomp",
  "ggplot2",
  "ggalluvial",
  "corrplot",
  "car",
  "performance",
  "glmulti",
  "brant"
)

# Installing missing packages automatically
installed_packages = rownames(installed.packages())

for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Loading packages
lapply(required_packages, library, character.only = TRUE)

# Creating output directory if it does not exist
if (!dir.exists("outputs")) {
  dir.create("outputs")
}

# Importing data
data_raw = read_excel("data/bypass_data.xlsx")
str(data_raw)

# Data cleaning -----------------------------------------------------------

# Creating another dataset from raw dataset for desc statistics
data_desc = data_raw[data_raw$diagnosis == "ACVD", ]

# Checking column names to see the different variable names and types
colnames(data_desc)
nrow(data_desc)

# Defining different variable groups/types
con_var = c("age", "bmi", "hospital_stay_day", "fu_time")

cat_var = c("diagnosis", "inhouse_fu", "affected_vessel", "onset_symp", 
            "affected_vessel_stat", "affected_hemi", "bypass_side",
            "htn", "dm", "hyperlipid", "smoking", "copd",
            "hyperuricemia", "cad", "pad", "af", "ckd",
            "antiplatelet", "statine", "anticoag", 
            "perf_ct", "perf_ct_type", "perf_status",
            "fu_symp_type", "bypass_status", "fu_symp", "imaging_type", "referrals")

ord_var = c("preop_mrs", "mrs_discharge", "fu_mrs")


# Converting variables to correct type
data_desc[con_var] = lapply(data_desc[con_var], as.numeric)
data_desc[cat_var] = lapply(data_desc[cat_var], as.factor)
data_desc[ord_var] = lapply(data_desc[ord_var], function(x) factor(x, levels = 0:6, ordered = TRUE))

# Separating patients with and without follow-up
# Analysis of patients without follow-up at the end of the code
data_fu_desc = data_desc[data_desc$inhouse_fu == "yes", ]
data_no_fu_desc = data_desc[data_desc$inhouse_fu == "no", ]

# Checking number of patients with and without follow-up
nrow(data_fu_desc)
nrow(data_no_fu_desc)

# Checking number of domestic and international referrals for patients without follow-up
nrow(data_no_fu_desc[data_no_fu_desc$referrals == "domestic" & !is.na(data_no_fu_desc$referrals),])
nrow(data_no_fu_desc[data_no_fu_desc$referrals == "international" & !is.na(data_no_fu_desc$referrals),])


# Descriptive statistics --------------------------------------------------

#### === # === # === # === # === # === # === # === # === 
# DESCRIPTIVE STATISTICS
#### === # === # === # === # === # === # === # === # === 

# Calculating mean, IQR, min & max for previously defined continuous variables stratified by sex
lapply(list(mean = mean, median = median, sd = sd,
            IQR = IQR,
            Q1  = function(x, na.rm) quantile(x, 0.25, na.rm = TRUE),
            Q3  = function(x, na.rm) quantile(x, 0.75, na.rm = TRUE),
            min = min,
            max = max), function(f) {
              sapply(con_var, function(var)
                tapply(data_fu_desc[[var]], data_fu_desc$sex, f, na.rm = TRUE))
            })


# Calculating frequencies and % for categorical variables stratified by sex
lapply(setNames(cat_var, cat_var), function(var) {
  tbl = table(data_fu_desc[[var]], data_fu_desc$sex)
  pct = prop.table(tbl, margin = 2) * 100
  matrix(paste0(tbl, " (", round(pct, 1), "%)"),
         nrow = nrow(tbl), dimnames = dimnames(tbl))
})

# Ordinal variables
lapply(setNames(ord_var, ord_var), function(var) {
  tbl = table(data_fu_desc[[var]], data_fu_desc$sex)
  pct = prop.table(tbl, margin = 2) * 100
  matrix(paste0(tbl, " (", round(pct, 1), "%)"),
         nrow = nrow(tbl), dimnames = dimnames(tbl))
})


#### === # === # === # === # === # === # === # === # === 
# VISUALISATION/PLOTS
#### === # === # === # === # === # === # === # === # === 

# Visualizing continous variables
ggplot(data_fu_desc, aes(x = sex, y = age)) +
  geom_boxplot()

ggplot(data_fu_desc, aes(x = age, fill = sex)) +
  geom_density(alpha = 0.4)

ggplot(data_fu_desc, aes(x = sex, y = bmi)) +
  geom_boxplot()

ggplot(data_fu_desc, aes(x = bmi, fill = sex)) +
  geom_density(alpha = 0.4)

ggplot(data_fu_desc, aes(x = sex, y = hospital_stay_day)) +
  geom_boxplot()

ggplot(data_fu_desc, aes(x = sex, y = fu_time)) +
  geom_boxplot()


# Visualizing the mRS scores and changes in mRS after discharge and follow up
ggplot(data_fu_desc, aes(x = preop_mrs, fill = sex)) +
  geom_bar(position = "fill") +
  ylab("Proportion")

ggplot(data_fu_desc, aes(x = mrs_discharge, fill = sex)) +
  geom_bar(position = "fill") +
  ylab("Proportion")

ggplot(data_fu_desc, aes(x = fu_mrs, fill = sex)) +
  geom_bar(position = "fill") +
  ylab("Proportion")

# Creating an alluvial plot to visualise the flow of mRS
# Creating a new variable
mrs_long = data.frame( 
  ID = rep(1:nrow(data_fu_desc), 
           times = 3), 
  sex = rep(data_fu_desc$sex, times = 3), 
  time = factor(rep(c("A","B","C"), 
                    each = nrow(data_fu_desc)), 
                levels = c("A","B","C")), 
  mrs = factor(c(data_fu_desc$preop_mrs, 
                 data_fu_desc$mrs_discharge, 
                 data_fu_desc$fu_mrs), 
               levels = 0:6, 
               ordered = TRUE) 
) 

# Creating an aggregate of all the counts
mrs_long_agg = as.data.frame(table(mrs_long$ID, mrs_long$time, mrs_long$mrs, mrs_long$sex)) 
colnames(mrs_long_agg) = c("ID","time","mrs","sex","n") 

# Removing zero rows 
mrs_long_agg = mrs_long_agg[mrs_long_agg$n > 0, ] 

# Converting to proportions
mrs_long_agg = mrs_long_agg[order(mrs_long_agg$sex, mrs_long_agg$time), ] 
mrs_long_agg$prop = ave(mrs_long_agg$n, mrs_long_agg$sex, mrs_long_agg$time, FUN = function(x) x / sum(x))

ggplot(mrs_long_agg,
       aes(x = time,
           y = prop, 
           stratum = mrs, 
           alluvium = ID)) + 
  geom_flow(aes(fill = mrs), 
            stat = "alluvium", 
            aes.flow = "forward") + 
  geom_stratum(aes(fill = mrs), 
               alpha = 0.5) + 
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum))) + 
  labs(title = "Alluvial plot of mRS", 
       y = NULL) + 
  scale_x_discrete("", 
                   labels = c("A" = "Baseline", "B" = "Discharge", "C" = "Follow-up")) + 
  scale_y_continuous(labels = NULL, breaks = NULL) + 
  scale_fill_brewer(palette = "Set1") + 
  facet_wrap(~sex) + theme_classic() + 
  theme( legend.position = "none", 
         axis.line = element_blank(), 
         axis.ticks.y = element_blank(), 
         axis.text.y = element_blank() 
  )

# Visualizing categorical variables
ggplot(data_fu_desc, aes(x = affected_vessel_stat, fill = sex)) +
  geom_bar(position = "fill") 

ggplot(data_fu_desc, aes(x = perf_status, fill = sex)) +
  geom_bar(position = "fill") 

ggplot(data_fu_desc, aes(x = onset_symp, fill = sex)) +
  geom_bar(position = "fill")

ggplot(data_fu_desc, aes(x = fu_symp, fill = sex)) +
  geom_bar(position = "fill") 



# Logistic regression -----------------------------------------------------

#### === # === # === # === # === # === # === # === # === 
# DATA SETUP
#### === # === # === # === # === # === # === # === # === 

# Creating new dataset for regression models 
df_ordinal = data_raw[!is.na(data_raw$inhouse_fu) & data_raw$inhouse_fu == "yes", ]

# Converting variables to correct type for new dataset
df_ordinal[con_var] = lapply(df_ordinal[con_var], as.numeric)
df_ordinal[cat_var] = lapply(df_ordinal[cat_var], as.factor)

# Converting outcome variable
df_ordinal$fu_mrs = factor(df_ordinal$fu_mrs, levels = 0:6, ordered = TRUE)
df_ordinal$fu_mrs = droplevels(df_ordinal$fu_mrs)

# Converting preop_mrs to numeric for correlation analysis and linearity diagnostics
df_ordinal$preop_mrs = as.numeric(as.character(df_ordinal$preop_mrs))

# Reference levels: Male for sex 
df_ordinal$sex = factor(df_ordinal$sex, levels = c("M", "F"))
df_ordinal$antiplatelet = factor(df_ordinal$antiplatelet, levels = c("no", "yes", "double"))


#### === # === # === # === # === # === # === # === # === 
# REGRESSION HELPER FUNCTION
#### === # === # === # === # === # === # === # === # === 

# Function to extract OR, CI, and p-values from polr model
polr_p = function(m) {
  ct = coef(summary(m))
  p  = pnorm(abs(ct[, "t value"]), lower.tail = FALSE) * 2
  ci = suppressMessages(exp(confint(m)))
  if (is.null(dim(ci))) ci = matrix(ci, nrow = 1, dimnames = list(names(coef(m)), c("2.5 %", "97.5 %")))
  n  = length(coef(m))
  data.frame(OR = round(exp(coef(m)[1:n]), 3),
             CI_low = round(ci[1:n, 1], 3),
             CI_high = round(ci[1:n, 2], 3),
             p = round(p[1:n], 4))
}

#### === # === # === # === # === # === # === # === # === 
# UNIVARIABLE ANALYSIS
#### === # === # === # === # === # === # === # === # === 

# Screening all candidate variables for association with fu_mrs
uni_vars = c("sex", "age", "bmi", "preop_mrs", "htn", "dm", "hyperlipid",
             "smoking", "copd", "cad", "pad", "af", "ckd",
             "antiplatelet", "statine", "anticoag")

cat("\n### UNIVARIABLE ANALYSIS ###\n")
for (v in uni_vars) {
  m = tryCatch(polr(as.formula(paste("fu_mrs ~", v)), data = df_ordinal, Hess = TRUE),
               error = function(e) NULL)
  if (!is.null(m)) { cat("---", v, "---\n"); print(polr_p(m)); cat("\n") }
}


#### === # === # === # === # === # === # === # === # === 
# PREREGRESSION CHECKS
#### === # === # === # === # === # === # === # === # === 

# Linearity check
cat("\n--- LINEARITY CHECK (Continuous Predictors vs Outcome) ---\n")

# Continuous variables to assess
cont_vars_check = c("age", "preop_mrs")

# Converting outcome to numeric for plotting (ordinal scale)
df_ordinal$fu_mrs_num = as.numeric(as.character(df_ordinal$fu_mrs))

# Plottung each variable against outcome with LOESS smoothing
for (v in cont_vars_check) {
  
  p = ggplot(df_ordinal, aes(x = .data[[v]], y = fu_mrs_num)) +
    geom_jitter(width = 0.2, height = 0.2, alpha = 0.4, color = "darkgray") +
    geom_smooth(method = "loess", color = "blue", se = TRUE) +
    labs(title = paste("Linearity Check:", v, "vs fu_mrs"),
         x = v,
         y = "Outcome (fu_mrs as numeric)") +
    theme_minimal()
  
  print(p)
}

df_ordinal$age_q = cut(df_ordinal$age,
                       breaks = quantile(df_ordinal$age, probs = seq(0, 1, 0.25), na.rm = TRUE),
                       include.lowest = TRUE)

m_age_cat = polr(fu_mrs ~ age_q, data = df_ordinal, Hess = TRUE)

cat("\nAge (quartiles) vs outcome:\n")
print(polr_p(m_age_cat))


cat("\n### PRE-REGRESSION CHECKS (Collinearity & Assumptions) ###\n")

# Candidates selected at p < 0.30 from univariable analysis
# Final model selected via AICc minimization
candidates = c("sex", "age", "preop_mrs", "dm", "pad", "ckd", "antiplatelet", "anticoag", "cad", "htn", "af")

# Creating a correlation matrix for continuous variables
cat("\n--- Correlation Matrix (age, preop_mrs) ---\n")
cor_vars = c("age", "preop_mrs")
df_cor = df_ordinal[, cor_vars]
df_cor$preop_mrs = as.numeric(df_ordinal$preop_mrs)
cor_matrix = cor(df_cor, use = "complete.obs")
print(cor_matrix)
corrplot(cor_matrix, method = "circle", type = "upper", diag = FALSE, addCoef.col = "black")

# VIF check for multicollinearity (using linear model as proxy)
cat("\n--- Variance Inflation Factor (Candidate Variables) ---\n")
lm_candidates = lm(as.numeric(fu_mrs) ~ sex + age + preop_mrs + dm + pad + ckd + antiplatelet + anticoag + cad + htn + af,
                   data = df_ordinal)
print(vif(lm_candidates))

# Using Performance package diagnostics
cat("\n--- Performance Package Collinearity Check ---\n")
check_collinearity(lm_candidates)

# Performing Chi-squared test for categorical variable associations
cat("\n--- Chi-Squared Associations Among Categorical Predictors (p < 0.05) ---\n")
cat_candidates = c("sex", "dm", "pad", "ckd", "antiplatelet", "anticoag", "cad", "htn", "af")
chi_results = data.frame()
for (i in 1:(length(cat_candidates)-1)) {
  for (j in (i+1):length(cat_candidates)) {
    v1 = cat_candidates[i]
    v2 = cat_candidates[j]
    tbl = table(df_ordinal[[v1]], df_ordinal[[v2]])
    chi_test = chisq.test(tbl)
    chi_results = rbind(chi_results, 
                        data.frame(var1 = v1, var2 = v2, 
                                   chisq = round(chi_test$statistic, 2),
                                   p_value = round(chi_test$p.value, 4)))
  }
}
print(chi_results[chi_results$p_value < 0.05, ])

#### === # === # === # === # === # === # === # === # === 
# VARIABLE SELECTION: DATA DRIVEN (glmulti)
#### === # === # === # === # === # === # === # === # === 

cat("\n### VARIABLE SELECTION: DATA-DRIVEN APPROACH (glmulti) ###\n")

# glmulti exhaustive search using AICc criterion
glmulti_result = glmulti(
  fu_mrs ~ sex + age + preop_mrs + dm + pad + ckd + antiplatelet + anticoag + cad + htn + af,
  data = df_ordinal,
  fitfunction = polr,
  crit = "aicc",
  level = 1,        
  method = "h",     
  plotty = FALSE,
  confsetsize = 100
)

cat("Best model from glmulti:\n")
print(glmulti_result$bestmodel)
plot(glmulti_result, type = "s")



#### === # === # === # === # === # === # === # === # === 
# MULTIVARIABLE MODELS (MAIN COHORT)
#### === # === # === # === # === # === # === # === # === 

cat("\n### MULTIVARIABLE MODELS ###\n")

# Model 1: Theory-driven (clinical reasoning)
# Variables selected based on clinical reasoning and prior knowledge
m_ordinal = polr(fu_mrs ~ sex + age + preop_mrs + htn + dm + cad + af + ckd + pad,
                 data = df_ordinal, Hess = TRUE)

cat("\n--- Model 1: (m_ordinal) ---\n")
print(polr_p(m_ordinal))

# Model 2: Data-driven model selection (from glmulti)
# Selected variables based on AICc
m_final = polr(fu_mrs ~ sex + age + preop_mrs + pad, 
               data = df_ordinal, Hess = TRUE)

cat("\n--- Model 2: Data-Driven (m_final) ---\n")
print(polr_p(m_final))


#### === # === # === # === # === # === # === # === # === 
# POST-REGRESSION DIAGNOSTICS
#### === # === # === # === # === # === # === # === # === 

cat("\n### POST-REGRESSION DIAGNOSTICS ###\n")

# Helper function to extract diagnostics for both models
get_diagnostics = function(m, model_name) {
  cat("\n--- MODEL:", model_name, "---\n")
  
  # 1. Model fit statistics
  cat("Log-likelihood:", round(logLik(m), 2), "\n")
  cat("AIC:", round(AIC(m), 2), "\n")
  cat("BIC:", round(BIC(m), 2), "\n")
  
  # 2. Brant test (proportional odds assumption)
  cat("\nBrant Test (Proportional Odds Assumption):\n")
  print(brant(m))
  
  # 3. VIF in final model
  cat("\nVIF (Final Model):\n")
  model_vars = all.vars(formula(m))[-1]  # Extract predictor names
  lm_vif = lm(as.numeric(fu_mrs) ~ ., data = df_ordinal[, c("fu_mrs", model_vars)])
  print(vif(lm_vif))
}

# Running diagnostics for both models
get_diagnostics(m_ordinal, "Inferential model (theory-driven) (m_ordinal)")
get_diagnostics(m_final, "Data-Driven (m_final)")

# Interaction tests: sex interactions

cat("\n### INTERACTION TESTS: SEX × PREDICTORS ###\n")
cat("Testing whether sex effect differs by other predictors\n")

# Testing each main model predictor for interaction with sex
interaction_tests = list()

# Sex × Age
m_int_sex_age = polr(fu_mrs ~ sex * age + preop_mrs + pad + ckd + htn,
                     data = df_ordinal, Hess = TRUE)
interaction_tests$"Sex × Age" = polr_p(m_int_sex_age)

# Sex × Preop_mrs
m_int_sex_preop = polr(fu_mrs ~ sex * preop_mrs + age + pad + ckd + htn,
                       data = df_ordinal, Hess = TRUE)
interaction_tests$"Sex × Preop_mrs" = polr_p(m_int_sex_preop)

# Sex × PAD
m_int_sex_pad = polr(fu_mrs ~ sex * pad + age + preop_mrs + ckd + htn,
                     data = df_ordinal, Hess = TRUE)
interaction_tests$"Sex × PAD" = polr_p(m_int_sex_pad)

# Sex × CKD
m_int_sex_ckd = polr(fu_mrs ~ sex * ckd + age + preop_mrs + pad + htn,
                     data = df_ordinal, Hess = TRUE)
interaction_tests$"Sex × CKD" = polr_p(m_int_sex_ckd)

# Sex × HTN
m_int_sex_htn = polr(fu_mrs ~ sex * htn + age + preop_mrs + pad + ckd,
                     data = df_ordinal, Hess = TRUE)
interaction_tests$"Sex × HTN" = polr_p(m_int_sex_htn)

# Printing interaction p-values only 
for (int_name in names(interaction_tests)) {
  int_df = interaction_tests[[int_name]]
  interaction_row = int_df[grepl(":", rownames(int_df)), ]
  if (nrow(interaction_row) > 0) {
    cat("\n", int_name, ":\n")
    print(interaction_row)
  }
}

# Influence diagnostics for theory driven model
cat("\n--- INFLUENCE DIAGNOSTICS ---\n")

# Approximating influence using linear model proxy
model_vars = all.vars(formula(m_ordinal))[-1]
lm_influence = lm(as.numeric(fu_mrs) ~ .,
                  data = df_ordinal[, c("fu_mrs", model_vars)])

# Cook's distance
cooks_d = cooks.distance(lm_influence)

# Plotting Cook's distance
p = ggplot(data.frame(cooks_d), aes(x = seq_along(cooks_d), y = cooks_d)) +
  geom_bar(stat = "identity", fill = "darkgray") +
  geom_hline(yintercept = 4/length(cooks_d), color = "red", linetype = "dashed") +
  labs(title = "Cook's Distance",
       x = "Observation",
       y = "Cook's distance") +
  theme_minimal()

print(p)

# Identifying influential observations
threshold = 4 / length(cooks_d)
influential = which(cooks_d > threshold)

cat("\nNumber of influential observations:", length(influential), "\n")

if (length(influential) > 0) {
  cat("Influential observation indices:\n")
  print(influential)
}

df_ordinal[influential, ]

df_no_inf = df_ordinal[-influential, ]
m_no_inf = polr(fu_mrs ~ sex + age + preop_mrs + htn + dm + cad + af + ckd + pad,
                data = df_no_inf, Hess = TRUE)

cat("\n--- Model without influential observations ---\n")
print(polr_p(m_no_inf))

#### === # === # === # === # === # === # === # === # === 
# SENSITIVITY ANALYSIS
#### === # === # === # === # === # === # === # === # === 

cat("\n### SENSITIVITY ANALYSIS: FEMALE OR (Leave-One-Out) ###\n")
cat("Assessing robustness of sex effect by excluding one variable at a time\n")

# Helper function for sensitivity analysis
sensitivity_analysis = function(m, model_name) {
  model_vars = all.vars(formula(m))[-1]  # Extract predictor names
  
  sensitivity_results = data.frame()
  
  # Leave-one-out: fit model excluding each variable in turn
  for (v in model_vars[model_vars != "sex"]) {
    vars_subset = setdiff(model_vars, v)
    formula_str = paste("fu_mrs ~", paste(vars_subset, collapse = " + "))
    
    m_reduced = polr(as.formula(formula_str), data = df_ordinal, Hess = TRUE)
    r = polr_p(m_reduced)
    
    r$variable = rownames(r)
    out = r[r$variable == "sexF", ]
    out$excluded_var = v
    
    sensitivity_results = rbind(sensitivity_results, out)
  }
  
  # Add full model result
  full_r = polr_p(m)
  full_r$variable = rownames(full_r)
  full_r = full_r[full_r$variable == "sexF", ]
  full_r$excluded_var = "None (Full Model)"
  
  sensitivity_results = rbind(full_r, sensitivity_results)
  
  cat("\n---", model_name, "---\n")
  print(sensitivity_results[, c("OR", "CI_low", "CI_high", "p", "excluded_var")])
  
  return(sensitivity_results)
}

# Run sensitivity analysis for both models
sens_ordinal = sensitivity_analysis(m_ordinal, "Inferential model (theory-driven) (m_ordinal)")
sens_final = sensitivity_analysis(m_final, "Data-Driven (m_final)")


#### === # === # === # === # === # === # === # === # === 
# SENSITIVITY ANALYSIS DATA EXPORT
#### === # === # === # === # === # === # === # === # === 

# Update sensitivity function to include model name and export
sens_combined = rbind(
  cbind(sens_ordinal, model_type = "Literature-Based"),
  cbind(sens_final, model_type = "Data-Driven")
)
write.csv(sens_combined, "outputs/sensitivity.csv", row.names = FALSE)


#### === # === # === # === # === # === # === # === # === 
# FOREST PLOT DATA EXPORT
#### === # === # === # === # === # === # === # === # === 

# Univariable results for forest plot
uni_results = list()
for (v in uni_vars) {
  m = tryCatch(polr(as.formula(paste("fu_mrs ~", v)), data = df_ordinal, Hess = TRUE),
               error = function(e) NULL)
  if (!is.null(m)) {
    r = polr_p(m)
    r$variable = rownames(r)
    r$model = "Univariable"
    uni_results[[v]] = r
  }
}
uni_df = do.call(rbind, uni_results)

# Multivariable results for both models
multi_df_ordinal = polr_p(m_ordinal)
multi_df_ordinal$variable = rownames(multi_df_ordinal)
multi_df_ordinal$model = "Multivariable (Literature-Based)"

multi_df_final = polr_p(m_final)
multi_df_final$variable = rownames(multi_df_final)
multi_df_final$model = "Multivariable (Data-Driven)"

# Combine all for forest plot
forest_df = rbind(uni_df, multi_df_ordinal, multi_df_final)
write.csv(forest_df, "outputs/forest_plot.csv", row.names = FALSE)


#### === # === # === # === # === # === # === # === # === 
# VISUALIZATION: FOREST PLOTS
#### === # === # === # === # === # === # === # === # === 

cat("\n### FOREST PLOTS ###\n")

# Function to create forest plot from model
forest_plot = function(m, model_name) {
  coef_data = polr_p(m)
  coef_data$variable = rownames(coef_data)
  
  p = ggplot(coef_data, aes(x = reorder(variable, OR), y = OR)) +
    geom_point(size = 4, color = "darkblue") +
    geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2, color = "darkblue") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
    coord_flip() +
    labs(title = paste("Forest Plot:", model_name),
         x = "Variable", y = "Odds Ratio (log scale)") +
    scale_y_log10() +
    theme_minimal() +
    theme(axis.text = element_text(size = 11),
          plot.title = element_text(size = 13, face = "bold"))
  
  return(p)
}

plot(forest_plot(m_ordinal, "Inferential model (theory-driven)"))
plot(forest_plot(m_final, "Data-Driven Model"))


# Create forest plots showing both multivariable models vs univariable
# Filter to show only sex (primary exposure) for focus

sex_forest = forest_df[forest_df$variable %in% c("sexF"), ]

ggplot(sex_forest, aes(x = reorder(model, OR), y = OR, color = model)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.8) +
  coord_flip() +
  labs(title = "Female vs Male: Univariable and Multivariable Models",
       x = "Model Type", y = "Odds Ratio (95% CI)") +
  scale_y_log10() +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        plot.title = element_text(size = 13, face = "bold"),
        legend.position = "none")

# Complete forest plot (all variables in multivariable models only)
multi_only = forest_df[forest_df$model != "Univariable", ]

ggplot(multi_only, aes(x = reorder(variable, OR), y = OR, color = model)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.3, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.8) +
  coord_flip() +
  labs(title = "Multivariable Models: Inferential model (theory-driven) vs Data-Driven",
       x = "Variable", y = "Odds Ratio (95% CI)", color = "Model") +
  scale_y_log10() +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        plot.title = element_text(size = 13, face = "bold"))


#### === # === # === # === # === # === # === # === # === 
# SUBGROUP ANALYSIS: CEREBROVASCULAR RESERVE CAPACITY (CVRC)
#### === # === # === # === # === # === # === # === # === 

cat("\n### SUBGROUP ANALYSIS: CVRC ###\n")

# Creating a subset of only patients with CVRC available
df_cvrc = df_ordinal[df_ordinal$perf_status != "not assessed" & !is.na(df_ordinal$perf_status), ]

cat("\nSample size with CVRC data:", nrow(df_cvrc), "\n")
cat("Distribution by sex:\n")
print(table(df_cvrc$perf_status, df_cvrc$sex))

# Collapsing levels to avoid instability
df_cvrc$perf_status = factor(df_cvrc$perf_status,
                             levels = c("Normal", "Slightly reduced", "Reduced", "Exhausted"),
                             labels = c("Normal", "Mild", "Reduced", "Exhausted"))

# Create binary CVRC variable (Preserved vs Impaired)
df_cvrc$perf_status_bin = ifelse(df_cvrc$perf_status %in% c("Reduced", "Exhausted"),
                                 "Impaired", "Preserved")
df_cvrc$perf_status_bin = factor(df_cvrc$perf_status_bin)

# UNIVARIABLE ANALYSIS IN CVRC SUBGROUP
cat("\n--- Univariable Analysis in CVRC Subgroup ---\n")

uni_vars_perf = c("sex", "age", "bmi", "preop_mrs", "htn", "dm", "hyperlipid",
                  "smoking", "copd", "cad", "pad", "af", "ckd",
                  "antiplatelet", "statine", "anticoag", "perf_status")

for (v in uni_vars_perf) {
  m = tryCatch(polr(as.formula(paste("fu_mrs ~", v)), data = df_cvrc, Hess = TRUE),
               error = function(e) NULL)
  if (!is.null(m)) {
    cat("---", v, "---\n")
    print(polr_p(m))
    cat("\n")
  }
}


# MULTIVARIABLE MODELS IN CVRC SUBGROUP

# Model 1: Theory-driven (matching main analysis structure)
m_cvrc_ordinal = polr(fu_mrs ~ sex + age + preop_mrs + htn + dm + cad + af + ckd + pad + perf_status_bin,
                      data = df_cvrc, Hess = TRUE)

cat("\n--- Model 1: Theory-Driven (CVRC Subgroup) ---\n")
print(polr_p(m_cvrc_ordinal))

# Model 2: Data-driven (matching m_final structure, with perf_status_bin added)
m_cvrc_final = polr(fu_mrs ~ sex + age + preop_mrs + pad + perf_status_bin,
                    data = df_cvrc, Hess = TRUE)

cat("\n--- Model 2: Data-Driven (CVRC Subgroup) ---\n")
print(polr_p(m_cvrc_final))


# POST-REGRESSION DIAGNOSTICS FOR CVRC MODELS

cat("\n### POST-REGRESSION DIAGNOSTICS (CVRC SUBGROUP) ###\n")

# Helper function adapted for CVRC dataset
get_diagnostics_cvrc = function(m, model_name) {
  cat("\n--- MODEL:", model_name, "---\n")
  
  # 1. Model fit statistics
  cat("Log-likelihood:", round(logLik(m), 2), "\n")
  cat("AIC:", round(AIC(m), 2), "\n")
  cat("BIC:", round(BIC(m), 2), "\n")
  
  # 2. Brant test (proportional odds assumption)
  cat("\nBrant Test (Proportional Odds Assumption):\n")
  print(brant(m))
  
  # 3. VIF in final model
  cat("\nVIF (Final Model):\n")
  model_vars = all.vars(formula(m))[-1]
  lm_vif = lm(as.numeric(fu_mrs) ~ ., data = df_cvrc[, c("fu_mrs", model_vars)])
  print(vif(lm_vif))
}

# Run diagnostics for CVRC models
get_diagnostics_cvrc(m_cvrc_ordinal, "Theory-Driven (CVRC Subgroup)")
get_diagnostics_cvrc(m_cvrc_final, "Data-Driven (CVRC Subgroup)")


# INTERACTION TESTS IN CVRC SUBGROUP

cat("\n### INTERACTION TESTS: SEX × PREDICTORS (CVRC Subgroup) ###\n")
cat("Testing whether sex effect differs by other predictors in CVRC subgroup\n")

interaction_tests_cvrc = list()

# Sex × Age
m_int_sex_age_cvrc = polr(fu_mrs ~ sex * age + preop_mrs + perf_status_bin,
                          data = df_cvrc, Hess = TRUE)
interaction_tests_cvrc$"Sex × Age" = polr_p(m_int_sex_age_cvrc)

# Sex × Preop_mrs
m_int_sex_preop_cvrc = polr(fu_mrs ~ sex * preop_mrs + age + perf_status_bin,
                            data = df_cvrc, Hess = TRUE)
interaction_tests_cvrc$"Sex × Preop_mrs" = polr_p(m_int_sex_preop_cvrc)

# Sex × CVRC
m_int_sex_cvrc = polr(fu_mrs ~ sex * perf_status_bin + age + preop_mrs,
                      data = df_cvrc, Hess = TRUE)
interaction_tests_cvrc$"Sex × CVRC" = polr_p(m_int_sex_cvrc)

# Print interaction results
for (int_name in names(interaction_tests_cvrc)) {
  int_df = interaction_tests_cvrc[[int_name]]
  interaction_row = int_df[grepl(":", rownames(int_df)), ]
  if (nrow(interaction_row) > 0) {
    cat("\n", int_name, ":\n")
    print(interaction_row)
  }
}


# INFLUENCE DIAGNOSTICS IN CVRC SUBGROUP

cat("\n--- INFLUENCE DIAGNOSTICS (CVRC Subgroup) ---\n")

# Cook's distance for data-driven CVRC model
model_vars_cvrc = all.vars(formula(m_cvrc_final))[-1]
lm_influence_cvrc = lm(as.numeric(fu_mrs) ~ ., 
                       data = df_cvrc[, c("fu_mrs", model_vars_cvrc)])

cooks_d_cvrc = cooks.distance(lm_influence_cvrc)

# Plot Cook's distance
p_cvrc = ggplot(data.frame(cooks_d_cvrc), aes(x = seq_along(cooks_d_cvrc), y = cooks_d_cvrc)) +
  geom_bar(stat = "identity", fill = "darkgray") +
  geom_hline(yintercept = 4/length(cooks_d_cvrc), color = "red", linetype = "dashed") +
  labs(title = "Cook's Distance (CVRC Subgroup)",
       x = "Observation",
       y = "Cook's distance") +
  theme_minimal()

print(p_cvrc)

# Identify influential observations
threshold_cvrc = 4 / length(cooks_d_cvrc)
influential_cvrc = which(cooks_d_cvrc > threshold_cvrc)

cat("\nNumber of influential observations:", length(influential_cvrc), "\n")

if (length(influential_cvrc) > 0) {
  cat("Influential observation indices:\n")
  print(influential_cvrc)
  
  # Fit model without influential observations
  df_cvrc_no_inf = df_cvrc[-influential_cvrc, ]
  m_cvrc_final_no_inf = polr(fu_mrs ~ sex + age + preop_mrs + perf_status_bin,
                             data = df_cvrc_no_inf, Hess = TRUE)
  
  cat("\n--- Data-Driven Model without influential observations (CVRC) ---\n")
  print(polr_p(m_cvrc_final_no_inf))
}


# SENSITIVITY ANALYSIS IN CVRC SUBGROUP

cat("\n### SENSITIVITY ANALYSIS: FEMALE OR (CVRC Subgroup) ###\n")
cat("Assessing robustness of sex effect by excluding one variable at a time\n")

# Helper function for CVRC sensitivity analysis
sensitivity_analysis_cvrc = function(m, model_name) {
  model_vars = all.vars(formula(m))[-1]
  
  sensitivity_results = data.frame()
  
  # Leave-one-out: fit model excluding each variable in turn
  for (v in model_vars[model_vars != "sex"]) {
    vars_subset = setdiff(model_vars, v)
    formula_str = paste("fu_mrs ~", paste(vars_subset, collapse = " + "))
    
    m_reduced = polr(as.formula(formula_str), data = df_cvrc, Hess = TRUE)
    r = polr_p(m_reduced)
    
    r$variable = rownames(r)
    out = r[r$variable == "sexF", ]
    out$excluded_var = v
    
    sensitivity_results = rbind(sensitivity_results, out)
  }
  
  # Add full model result
  full_r = polr_p(m)
  full_r$variable = rownames(full_r)
  full_r = full_r[full_r$variable == "sexF", ]
  full_r$excluded_var = "None (Full Model)"
  
  sensitivity_results = rbind(full_r, sensitivity_results)
  
  cat("\n---", model_name, "---\n")
  print(sensitivity_results[, c("OR", "CI_low", "CI_high", "p", "excluded_var")])
  
  return(sensitivity_results)
}

# Run sensitivity analysis for CVRC models
sens_cvrc_ordinal = sensitivity_analysis_cvrc(m_cvrc_ordinal, "Theory-Driven (CVRC Subgroup)")
sens_cvrc_final = sensitivity_analysis_cvrc(m_cvrc_final, "Data-Driven (CVRC Subgroup)")


# FOREST PLOT DATA EXPORT FOR CVRC

cat("\n### FOREST PLOT DATA (CVRC Subgroup) ###\n")

# Univariable results for CVRC subgroup
uni_results_cvrc = list()
for (v in uni_vars_perf) {
  m = tryCatch(polr(as.formula(paste("fu_mrs ~", v)), data = df_cvrc, Hess = TRUE),
               error = function(e) NULL)
  if (!is.null(m)) {
    r = polr_p(m)
    r$variable = rownames(r)
    r$model = "Univariable"
    uni_results_cvrc[[v]] = r
  }
}
uni_df_cvrc = do.call(rbind, uni_results_cvrc)

# Multivariable results for CVRC models
multi_df_cvrc_ordinal = polr_p(m_cvrc_ordinal)
multi_df_cvrc_ordinal$variable = rownames(multi_df_cvrc_ordinal)
multi_df_cvrc_ordinal$model = "Multivariable (Theory-Driven)"

multi_df_cvrc_final = polr_p(m_cvrc_final)
multi_df_cvrc_final$variable = rownames(multi_df_cvrc_final)
multi_df_cvrc_final$model = "Multivariable (Data-Driven)"

# Combine for forest plot
forest_df_cvrc = rbind(uni_df_cvrc, multi_df_cvrc_ordinal, multi_df_cvrc_final)
write.csv(forest_df_cvrc, "outputs/forest_plot_cvrc.csv", row.names = FALSE)

cat("CVRC forest plot data exported to: forest_plot_cvrc.csv\n")


# SENSITIVITY ANALYSIS DATA EXPORT FOR CVRC

sens_cvrc_combined = rbind(
  cbind(sens_cvrc_ordinal, model_type = "Theory-Driven"),
  cbind(sens_cvrc_final, model_type = "Data-Driven")
)
write.csv(sens_cvrc_combined, "outputs/sensitivity_analysis_cvrc.csv", row.names = FALSE)

cat("CVRC sensitivity analysis exported to: sensitivity_analysis_cvrc.csv\n")


#### === # === # === # === # === # === # === # === # === 
# SESSION INFORMATION
#### === # === # === # === # === # === # === # === # === 

# Creating output directory if it does not exist
if (!dir.exists("outputs")) {
  dir.create("outputs")
}

# Saving session information
writeLines(capture.output(sessionInfo()),
           "outputs/session_info.txt")

cat("Session information exported to: outputs/session_info.txt\n")

