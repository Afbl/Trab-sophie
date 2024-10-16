library(lme4)
library(haven)
library(margins)

rm(list = ls())

############################################

FolderDirect <- "D:/Users/B435076/Documents/GitHub/Trab-sophie/r-sophie"  # Path of the folder "Replication_Conflict"
out <- "D:/Users/B435076/Documents/GitHub/Trab-sophie/r-sophie/out"    # Path of output folder
source("D:/Users/B435076/Documents/GitHub/Trab-sophie/r-sophie/Teste 14-10 modificado.R")
conflict_replication1 <- read.csv("D:/Users/B435076/Documents/GitHub/Trab-sophie/r-sophie/out/partial_data3.csv")

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




#mdes calculado somente multiplicando desvio-padrao por 2.8



####AVERAGE ENDOWMENT 

# Attacking models
model_r_attack <- glm(attack ~ endowment, family = binomial(link = "probit"), data = conflict_replication1, subset = treatment == 0)
margins_r_attack <- margins(model_r_attack, variables = "endowment")
coef_attack_r <- summary(margins_r_attack)$AME[1]

formula3 <- as.formula(paste("attack ~ endowment +", paste(reg_cov, collapse = " + ")))

# Fit the probit model with covariates
model_a_attack <- glm(formula3, family = binomial(link = "probit"), data = conflict_replication1, subset = treatment == 0)
margins_a_attack <- margins(model_a_attack, variables = "endowment", atmeans = TRUE)
coef_attack_a <- summary(margins_a_attack)$AME[1]

# Arming & Relative Arming (handling foreach equivalent using a loop)
reg_out_cont <- c("arming", "arm_def", "arm_att")  # Example outputs for arming and relative arming

results <- list()

# Loop over your outcome variables
for (output in reg_out_cont) {
  
  # Build the formula without controls
  formula_r <- as.formula(paste(output, "~ endowment + (1|id)"))
  
  # Without controls
  model_r_output <- lmer(formula_r, data = conflict_replication1, subset = treatment == 0)
  margins_r_output <- margins(model_r_output, variables = "endowment")
  coef_r_output <- summary(margins_r_output)$AME[1]
  
  # Build the formula with controls using reg_cov
  formula_a <- as.formula(paste(output, "~ endowment +", paste(reg_cov, collapse = " + "), "+ (1|id)"))
  
  # With controls
  model_a_output <- lmer(formula_a, data = conflict_replication1, subset = treatment == 0)
  margins_a_output <- margins(model_a_output, variables = "endowment", atmeans = TRUE)
  coef_a_output <- summary(margins_a_output)$AME[1]
  
  # Store results
  results[[output]] <- list(coef_r = coef_r_output, coef_a = coef_a_output)
}

##### Compute standard deviations
sd_arming_120 <- sd(conflict_replication1$arming[conflict_replication1$endowment == 120], na.rm = TRUE)
sd_attack_120 <- sd(conflict_replication1$attack[conflict_replication1$endowment == 120], na.rm = TRUE)
sd_arming_120_agg <- sd(conflict_replication1$arming[conflict_replication1$endowment == 120 & conflict_replication1$attack == 1], na.rm = TRUE)
sd_arming_120_def <- sd(conflict_replication1$arming[conflict_replication1$endowment == 120 & conflict_replication1$attack == 0], na.rm = TRUE)

# Relative effect sizes
effect_attack_r <- -1 * coef_attack_r / sd_attack_120
effect_attack_a <- -1 * coef_attack_a / sd_attack_120
effect_arming_r <- -1 * results[["arming"]][["coef_r"]] / sd_arming_120
effect_arming_a <- -1 * results[["arming"]][["coef_a"]] / sd_arming_120
effect_att_def_r <- -1 * results[["arm_def"]][["coef_r"]] / sd_arming_120_def
effect_att_def_a <- -1 * results[["arm_def"]][["coef_a"]] / sd_arming_120_def
effect_att_agg_r <- -1 * results[["arm_att"]][["coef_r"]] / sd_arming_120_agg
effect_att_agg_a <- -1 * results[["arm_att"]][["coef_a"]] / sd_arming_120_agg

# Display results
cat("Effect Size Attack (no control):", round(effect_attack_r, 4), "\n")
cat("Effect Size Attack (with controls):", round(effect_attack_a, 4), "\n")
cat("Effect Size Arming (no control):", round(effect_arming_r, 4), "\n")
cat("Effect Size Arming (with controls):", round(effect_arming_a, 4), "\n")
cat("Effect Size Defensive Arming (no control):", round(effect_att_def_r, 4), "\n")
cat("Effect Size Defensive Arming (control):", round(effect_att_def_a, 4), "\n")
cat("Effect Size Aggressive Arming (no control):", round(effect_att_agg_r, 4), "\n")
cat("Effect Size Aggressive Arming (control):", round(effect_att_agg_a, 4), "\n")


# Exporting regression tables
screenreg(list(model_r_attack, model_a_attack, model_r_output, model_a_output), 
          custom.model.names = c("Attack (No Control)", "Attack (With Controls)", "Arming (No Control)", "Arming (With Controls)"),
          stars = c(0.001, 0.01, 0.05))

# Save to .tex (LaTeX) file
texreg(list(model_r_attack, model_a_attack, model_r_output, model_a_output), 
       file = "AverageEffects.tex", 
       custom.model.names = c("Attack (No Control)", "Attack (With Controls)", "Arming (No Control)", "Arming (With Controls)"),
       stars = c(0.001, 0.01, 0.05))
