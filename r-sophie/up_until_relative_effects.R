library(lme4)
library(haven)
library(margins)

rm(list = ls())

############################################

FolderDirect <- getwd()  # Path of the folder "Replication_Conflict"
out <- sprintf('%s/out', FolderDirect)    # Path of output folder
conflict_replication1 <- read_dta("conflict_replication1.dta")

# Create Output folder
if (dir.exists(out)) {
  unlink(out, recursive = TRUE)
}
dir.create(out)

# Variable lists (defining character vectors to store variable names)
covariates <- c("age", "male", "econstudent", "bachelor", "comp1", "comp2", "likestowin", 
                "compulsive", "trust", "trustworth", "lossavers", "no_cointosses", 
                "riskaverse", "auszahlung", "Altruist", "Selfish", "Maximin")

reg_cov <- c("male", "age", "comp1", "comp2", "likestowin", "compulsive", "trust", 
             "trustworth", "riskaverse", "lossaversion_ordinal", "Altruist", 
             "Selfish", "Maximin")

Outcomes_beliefs <- c("belief_arming", "belief_attack", "belief_UP")

Out_beliefs_bin <- c("belief_attack", "belief_UP")
Out_beliefs_cont <- c("belief_arming")

list_associations <- c("attack", "arming", "choose_UP")

reg_out_bin <- c("attack", "choose_UP", "choose_AP", "choose_AC")
reg_out_cont <- c("arming", "arm_att", "arm_def", "rel_arming")

reg_out_bin_f <- c("attack", "choose_UP")
reg_out_cont_f <- c("arming")

############################################


FolderDirect <- getwd()  # Path of the folder "Replication_Conflict"
out <- sprintf('%s/out', FolderDirect)    # Path of output folder

# Define a helper function to fit the models, compute margins, and store results
fit_model <- function(output, formula_basic, formula_full, data, model_type = "binary") {
  
  if (model_type == "binary") {
    model_basic <- glmer(formula_basic, data = data, family = binomial(link = "probit"))
    model_full  <- glmer(formula_full, data = data, family = binomial(link = "probit"))
  } else {
    model_basic <- lmer(formula_basic, data = data)
    model_full  <- lmer(formula_full, data = data)
  }
  
  # Marginal effects
  margins_r <- margins(model_basic, variables = "treatment")
  margins_a <- margins(model_full, variables = "treatment", atmeans = TRUE)
  
  # Marginal effects over treatment
  margins_rr <- margins(model_basic, variables = "treatment", over = "treatment")
  margins_aa <- margins(model_full, variables = "treatment", over = "treatment", atmeans = TRUE)
  
  # Store coefficients and marginal effects
  coef_r <- summary(margins_r)$AME[1]
  coef_a <- summary(margins_a)$AME[1]
  
  list(
    model_basic = model_basic,
    model_full = model_full,
    margins_r = margins_r,
    margins_a = margins_a,
    margins_rr = margins_rr,
    margins_aa = margins_aa,
    coef_r = coef_r,
    coef_a = coef_a
  )
}

# Prepare the basic and full formulas for binary and continuous outcomes
prepare_formulas <- function(output, reg_cov) {
  formula_basic <- as.formula(paste(output, "~ treatment + period + (1|indep_obs) + (1|id)"))
  formula_full  <- as.formula(paste(output, "~ treatment + period +", paste(reg_cov, collapse = "+"),
                                    "+ (1|indep_obs) + (1|id)"))
  list(formula_basic = formula_basic, formula_full = formula_full)
}

# Apply the models for binary outcomes
binary_results <- lapply(reg_out_bin, function(output) {
  formulas <- prepare_formulas(output, reg_cov)
  fit_model(output, formulas$formula_basic, formulas$formula_full, conflict_replication1, model_type = "binary")
})

# Apply the models for continuous outcomes
continuous_results <- lapply(reg_out_cont, function(output) {
  formulas <- prepare_formulas(output, reg_cov)
  fit_model(output, formulas$formula_basic, formulas$formula_full, conflict_replication1, model_type = "continuous")
})

# The results are stored in lists, with each element corresponding to a specific output variable
# You can access them like this:
binary_results[[1]]$model_basic    # Basic model for the first binary outcome
binary_results[[1]]$coef_r         # Marginal effect coefficient for the first binary outcome
continuous_results[[1]]$margins_a  # Margins for the first continuous outcome


#############################################################


# Calculate standard deviations for various subgroups
SDarming_UE_def <-
  sd(
    conflict_replication1$arming[conflict_replication1$treatment == 0 & conflict_replication1$attack == 0],
    na.rm = TRUE)
SDarming_UE <- sd(conflict_replication1$arming[conflict_replication1$treatment == 0], na.rm = TRUE)
SDatt_UE <- sd(conflict_replication1$attack[conflict_replication1$treatment == 0], na.rm = TRUE)
SDarming_UE_agg <- sd(conflict_replication1$arming[conflict_replication1$treatment == 0 & conflict_replication1$attack == 1], na.rm = TRUE)

# Calculate effect sizes for attack and arming
effect_att_no_control <- -1 * binary_results[[which(reg_out_bin == "attack")]]$coef_r / SDatt_UE
cat("Effect Size Attack: (no control)", round(effect_att_no_control, 4), "\n")

effect_att_with_control <- -1 * binary_results[[which(reg_out_bin == "attack")]]$coef_a / SDatt_UE
cat("Effect Size Attack: (with controls)", round(effect_att_with_control, 4), "\n")

effect_arming_no_control <- -1 * continuous_results[[which(reg_out_cont == "arming")]]$coef_r / SDarming_UE
cat("Effect Size Arming (no control):", round(effect_arming_no_control, 4), "\n")

effect_arming_with_control <- -1 * continuous_results[[which(reg_out_cont == "arming")]]$coef_a / SDarming_UE
cat("Effect Size Arming (with controls):", round(effect_arming_with_control, 4), "\n")

effect_att_def_no_control <- -1 * continuous_results[[which(reg_out_cont == "arm_def")]]$coef_r / SDarming_UE_def
cat("Effect Size Defensive Arming: (no control)", round(effect_att_def_no_control, 4), "\n")

effect_att_def_with_control <- -1 * continuous_results[[which(reg_out_cont == "arm_def")]]$coef_a / SDarming_UE_def
cat("Effect Size Defensive Arming: (control)", round(effect_att_def_with_control, 4), "\n")

effect_att_agg_no_control <- -1 * continuous_results[[which(reg_out_cont == "arm_att")]]$coef_r / SDarming_UE_agg
cat("Effect Size Aggressive Arming: (no control)", round(effect_att_agg_no_control, 4), "\n")

effect_att_agg_with_control <- -1 * continuous_results[[which(reg_out_cont == "arm_att")]]$coef_a / SDarming_UE_agg
cat("Effect Size Aggressive Arming: (control)", round(effect_att_agg_with_control, 4), "\n")

##

install.packages('texreg')
install.packages('broom')
library(texreg)
library(broom)

# Generate tables for average treatment effects without and with controls
texreg(
  l = list(binary_results[[which(reg_out_bin == "attack")]]$margins_r, 
           binary_results[[which(reg_out_bin == "attack")]]$margins_a,
           continuous_results[[which(reg_out_cont == "arming")]]$margins_r,
           continuous_results[[which(reg_out_cont == "arming")]]$margins_a),
  file = "AverageEffects.tex",
  custom.model.names = c("Attack (no control)", "Attack (with controls)", 
                         "Arming (no control)", "Arming (with controls)"),
  label = "Table 2: Average Treatment Effects - Control Means",
  stars = c(0.05, 0.01, 0.001),
  digits = 3
)

# Append more results for different choices and additional regressions
texreg(
  l = list(binary_results[[which(reg_out_bin == "choose_UP")]]$margins_r,
           binary_results[[which(reg_out_bin == "choose_UP")]]$margins_a),
  file = "AverageEffects2.tex",
  custom.model.names = c("Choose UP (no control)", "Choose UP (with controls)"),
  append = TRUE,
  label = "Table S5: Average Treatment Effects",
  stars = c(0.05, 0.01, 0.001),
  digits = 3
)

