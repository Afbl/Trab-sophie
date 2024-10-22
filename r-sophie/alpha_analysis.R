library(lme4)
library(haven)
library(margins)
library(dplyr)

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

########################################### Beliefs

# # # Define variable sets for binary indicators
# # Out_beliefs_bin <- c("belief_attack", "belief_UP")
# # # Continuous Outcomes (Linear Models and Marginal Effects)
# # Out_beliefs_cont <- c("belief_arming")
# ### Took this part out bc its already defined on lines 30-31
#
# binary_beliefs_results <- lapply(Out_beliefs_bin, function(output) {
#   formulas <- prepare_formulas(output, reg_cov)
#   fit_model(output, formulas$formula_basic, formulas$formula_full, conflict_replication1, model_type = "binary")
#  })
#
# # Out_beliefs_bin
# #
# # conflict_replication1$belief_attack
# #
# # conflict_replication1 %>% select(all_of(Out_beliefs_bin))
#
# no_id <- conflict_replication1 %>% select(-id)
#
# length(conflict_replication1$id)
#
# conflict_replication1$id <- as.factor(conflict_replication1$id)
#
# conflict_replication1$id
#
# duplicates <- conflict_replication1[duplicated(conflict_replication1), ]
# duplicates
#
# conflict_replication1 %>%
#   group_by(id) %>%
#   filter(n() > 1) %>%
#   ungroup()
#
#
# continuous_beliefs_results <- lapply(Out_beliefs_cont, function(output) {
#   formulas <- prepare_formulas(output, reg_cov)
#   fit_model(output, formulas$formula_basic, formulas$formula_full,
#             conflict_replication1, model_type = "continuous")
# })
#
# ####
#
# # Create a list to store results
# results <- list()
#
# # Iterate over each output variable
# for (output in Out_beliefs_bin) {
#
#   # Probit regression without covariates
#   r1_model <- glm(as.formula(paste(output, "~ treatment")), family = binomial(link = "probit"), data = conflict_replication1)
#   results[[paste0("r1_", output)]] <- summary(r1_model)
#
#   # Marginal effects without covariates
#   r_margins <- margins(r1_model)
#   results[[paste0("r_", output)]] <- summary(r_margins)
#
#   # Probit regression with covariates
#   a1_model <- glm(as.formula(paste(output, "~ treatment +", paste(reg_cov, collapse = "+"))),
#                   family = binomial(link = "probit"), data = conflict_replication1)
#   results[[paste0("a1_", output)]] <- summary(a1_model)
#   #
#   # # prepare_formulas <- function(output, reg_cov) {
#   # #   formula_basic <- as.formula(paste(output, "~ treatment + period + (1|indep_obs) + (1|id)"))
#   # #   formula_full  <- as.formula(paste(output, "~ treatment + period +", paste(reg_cov, collapse = "+"),
#   # #                                     "+ (1|indep_obs) + (1|id)"))
#   # #   list(formula_basic = formula_basic, formula_full = formula_full)
#   # # }
#   #
#   #
#   # Marginal effects with covariates
#   a_margins <- margins(a1_model, at = list(treatment = mean(conflict_replication1$treatment, na.rm = TRUE)))
#   results[[paste0("a_", output)]] <- summary(a_margins)
#
#   # # Probit regression with robust standard errors
#   # rr_model <- glm(as.formula(paste(output, "~ treatment")), family = binomial(link = "probit"), data = conflict_replication1)
#   # results[[paste0("rr_", output)]] <- summary(rr_model)
#
#   # # Marginal effects with robust standard errors
#   # rr_margins <- margins(rr_model, over = "treatment")
#   # results[[paste0("rr_", output)]] <- summary(rr_margins)
#   #
#   # # Probit regression with covariates and robust standard errors
#   # aa_model <- glm(as.formula(paste(output, "~ treatment +", paste(reg_cov, collapse = "+"))),
#   #                 family = binomial(link = "probit"), data = conflict_replication1)
#   # results[[paste0("aa_", output)]] <- summary(aa_model)
#   #
#   # # Marginal effects with covariates and robust standard errors
#   # aa_margins <- margins(aa_model, at = list(treatment = mean(conflict_replication1$treatment, na.rm = TRUE)), over = "treatment")
#   # results[[paste0("aa_", output)]] <- summary(aa_margins)
# }
#
# # Check the results
# print(results)
#
#
# ##### new
#
# # Define a helper function to fit the models, compute margins, and store results
# notme_fit_model <- function(output, formula_basic, formula_full, data, model_type = "binary") {
#
#   if (model_type == "binary") {
#     model_basic <- glm(formula_basic, data = data, family = binomial(link = "probit"))
#     model_full  <- glm(formula_full, data = data, family = binomial(link = "probit"))
#   } else {
#     model_basic <- glm(formula_basic, data = data)
#     model_full  <- glm(formula_full, data = data)
#   }
#
#   # Marginal effects
#   margins_r <- margins(model_basic, variables = "treatment")
#   margins_a <- margins(model_full, variables = "treatment", atmeans = TRUE)
#
#   # Marginal effects over treatment
#   margins_rr <- margins(model_basic, variables = "treatment", over = "treatment")
#   margins_aa <- margins(model_full, variables = "treatment", over = "treatment", atmeans = TRUE)
#
#   # Store coefficients and marginal effects
#   coef_r <- summary(margins_r)$AME[1]
#   coef_a <- summary(margins_a)$AME[1]
#
#   list(
#     model_basic = model_basic,
#     model_full = model_full,
#     margins_r = margins_r,
#     margins_a = margins_a,
#     margins_rr = margins_rr,
#     margins_aa = margins_aa,
#     coef_r = coef_r,
#     coef_a = coef_a
#   )
# }
#
# # Prepare the basic and full formulas for binary and continuous outcomes
# notme_prepare_formulas <- function(output, reg_cov) {
#   formula_basic <- as.formula(paste(output, "~ treatment"))
#   formula_full  <- as.formula(paste(output, "~ treatment + ", paste(reg_cov, collapse = "+")))
#   list(formula_basic = formula_basic, formula_full = formula_full)
# }
#
# # Apply the models for binary outcomes
# belief_binary_results <- lapply(reg_out_bin, function(output) {
#   formulas <- notme_prepare_formulas(output, reg_cov)
#   notme_fit_model(output, formulas$formula_basic, formulas$formula_full, conflict_replication1, model_type = "binary")
# })
#
# # Apply the models for continuous outcomes
# belief_continuous_results <- lapply(reg_out_cont, function(output) {
#   formulas <- notme_prepare_formulas(output, reg_cov)
#   notme_fit_model(output, formulas$formula_basic, formulas$formula_full, conflict_replication1, model_type = "continuous")
# })
#
# # The results are stored in lists, with each element corresponding to a specific output variable
# # You can access them like this:
# belief_binary_results[[1]]$model_basic    # Basic model for the first binary outcome
# belief_binary_results[[1]]$coef_r         # Marginal effect coefficient for the first binary outcome
# belief_continuous_results[[1]]$margins_a  # Margins for the first continuous outcome

############################################# Multinomial Regression

install.packages('nnet')
install.packages('marginaleffects')
library(nnet)    # for multinom
library(margins) # for marginal effects
library(dplyr)   # for data manipulation
library(marginaleffects)

# Clear previous estimates
# (In R, there's no need for an explicit command)

# Preserve the dataset (useful in Stata but not needed in R, can just filter)
mlogit_data <- conflict_replication1 %>% filter(Strategies3 != 3)
# subset function yields the same
# madiamda <- conflict_replication1 %>% subset(Strategies3 != 3)

# Multinomial regression with trust, riskaverse, and lossavers
mlogit_model <- multinom(Strategies3 ~ trust + riskaverse + lossavers,
                         data = mlogit_data)

# marginal_effects(model_trust)
# # Marginal effects for 'trust' at specified values, holding other variables at means
# margins_trust <- margins(model_trust, at = list(trust = seq(0, 16, by = 2)))
margins_trust <-
  slopes(mlogit_model,
         newdata = datagrid(trust = seq(0, 16, by = 2)),
         type = "probs")
avg_margins_trust <-
  avg_slopes(mlogit_model,
         newdata = datagrid(trust = seq(0, 16, by = 2)),
         type = "probs")

# # Save results for trust to CSV
# write.csv(summary(margins_trust), file = "Figure_5_data.csv", append = TRUE)

# # Marginal effects for 'trust' at specified values (0, 2, 4, ..., 16), holding other variables constant
# newdata_trust <- data.frame(trust = seq(0, 16, by = 2), riskaverse = mean(data$riskaverse, na.rm = TRUE), lossavers = mean(data$lossavers, na.rm = TRUE))
# margins_trust <- marginal_effects(model_trust, data = newdata_trust)

# Save results for trust to CSV
write.csv(margins_trust, file = "Figure_5_data_trust.csv")
write.csv(avg_margins_trust, file = "avg_Figure_5_data_trust.csv")

########### loss

# Marginal effects for 'riskaverse' at specified values, holding other variables at means
margins_riskaverse <-
  slopes(mlogit_model,
         newdata = datagrid(riskaverse = seq(0, 1, by = 0.1)),
         type = "probs")
avg_margins_riskaverse <-
  avg_slopes(mlogit_model,
         newdata = datagrid(riskaverse = seq(0, 1, by = 0.1)),
         type = "probs")

# Append results for riskaverse to CSV
write.csv(margins_riskaverse, file = "Figure_5_data_risk.csv")
write.csv(avg_margins_riskaverse, file = "avg_Figure_5_data_risk.csv")

###########

# Marginal effects for 'lossavers' at specified values, holding other variables at means
margins_lossavers <-
  slopes(mlogit_model,
         newdata = datagrid(lossavers = seq(0, 1, by = 0.1)),
         type = "probs")
avg_margins_lossavers <-
  avg_slopes(mlogit_model,
         newdata = datagrid(lossavers = seq(0, 1, by = 0.1)),
         type = "probs")

# Append results for lossavers to CSV
write.csv(margins_lossavers, file = "Figure_5_data_loss.csv")
write.csv(avg_margins_lossavers, file = "avg_Figure_5_data_loss.csv")

# ########## binder
#
# thin_trust <- margins_trust %>% select('group','term','trust','std.error')
# thin_risk <- margins_riskaverse %>% select('group','term','trust','std.error')
# thin_loss <- margins_lossavers %>% select('group','term','trust','std.error')
#
# thin_loss
# write.csv(margins_lossavers, file = "Figure_5_data_loss.csv")

#
# # Final multinomial regression without exporting the result
# final_model <- multinom(Strategies3 ~ trust + riskaverse + lossavers, data = data)
# final_model_rrr <- exp(coef(final_model))  # Relative risk ratios (equivalent to rrr in Stata)
# View(summary(final_model))

########################################## Figure 1

data <- conflict_replication1

# List of variables to loop over
vars <- c("attack", "arming", "arm_att", "arm_def")

# Function to calculate mean and confidence intervals
calculate_summary <- function(data, var, treatment_value) {
  filtered_data <- data %>% filter(treatment == treatment_value)

  mean_val <- mean(filtered_data[[var]], na.rm = TRUE)
  sd_val <- sd(filtered_data[[var]], na.rm = TRUE)
  n_val <- sum(!is.na(filtered_data[[var]]))  # Number of non-NA observations

  lower_limit <- mean_val - 1.96 * (sd_val / sqrt(n_val))
  upper_limit <- mean_val + 1.96 * (sd_val / sqrt(n_val))

  return(list(mean = mean_val, lower = lower_limit, upper = upper_limit))
}

# Loop over each variable and calculate statistics for both treatment groups
for (var in vars) {
  # For treatment == 0 (UNEQUAL)
  summary_treatment_0 <- calculate_summary(data, var, treatment_value = 0)
  cat(paste0(var, " - Mean (UNEQUAL): ", summary_treatment_0$mean, "\n"))
  cat(paste0(var, " - Lower Limit (UNEQUAL): ", summary_treatment_0$lower, "\n"))
  cat(paste0(var, " - Upper Limit (UNEQUAL): ", summary_treatment_0$upper, "\n"))

  # For treatment == 1 (EQUAL)
  summary_treatment_1 <- calculate_summary(data, var, treatment_value = 1)
  cat(paste0(var, " - Mean (EQUAL): ", summary_treatment_1$mean, "\n"))
  cat(paste0(var, " - Lower Limit (EQUAL): ", summary_treatment_1$lower, "\n"))
  cat(paste0(var, " - Upper Limit (EQUAL): ", summary_treatment_1$upper, "\n"))
}


########################################## Figure 2 - choices

# For treatment == 1
data_treatment_1 <- data %>% filter(treatment == 1) %>% select(period, id, Strategies3, treatment)
View(data_treatment_1)

# For treatment == 0
data_treatment_0 <- data %>% filter(treatment == 0) %>% select(period, id, Strategies3, treatment)
View(data_treatment_0)

########################################## Figure 3 - development of outcomes

# Collapse (mean) by treatment and period
collapsed_data <- data %>%
  group_by(treatment, period) %>%
  summarize(
    unarmed_peace = mean(unarmed_peace, na.rm = TRUE),
    armed_peace = mean(armed_peace, na.rm = TRUE),
    armed_conflict = mean(armed_conflict, na.rm = TRUE)
  )

# Replace armed_peace and armed_conflict based on the described operations
collapsed_data <- collapsed_data %>%
  mutate(
    armed_peace = armed_peace + unarmed_peace,
    armed_conflict = armed_conflict + armed_peace
  )

# View the updated collapsed data
View(collapsed_data)


########################################## Figure 4 - Frequency of attacking and investments in arms.


# List of variables to loop over
vars <- c("attack", "arming", "arm_att", "arm_def")

# Function to calculate mean and confidence intervals
calculate_summary <- function(data, var, endowment_value) {
  filtered_data <- data %>% filter(endowment == endowment_value)

  mean_val <- mean(filtered_data[[var]], na.rm = TRUE)
  sd_val <- sd(filtered_data[[var]], na.rm = TRUE)
  n_val <- sum(!is.na(filtered_data[[var]]))  # Number of non-NA observations

  lower_limit <- mean_val - 1.96 * (sd_val / sqrt(n_val))
  upper_limit <- mean_val + 1.96 * (sd_val / sqrt(n_val))

  return(list(mean = mean_val, lower = lower_limit, upper = upper_limit))
}

# Loop over each variable and calculate statistics for both endowment groups
for (var in vars) {
  # For endowment == 120
  summary_endowment_120 <- calculate_summary(data, var, endowment_value = 120)
  cat(paste0(var, " - Mean (120): ", summary_endowment_120$mean, "\n"))
  cat(paste0(var, " - Lower Limit (120): ", summary_endowment_120$lower, "\n"))
  cat(paste0(var, " - Upper Limit (120): ", summary_endowment_120$upper, "\n"))

  # For endowment == 80
  summary_endowment_80 <- calculate_summary(data, var, endowment_value = 80)
  cat(paste0(var, " - Mean (80): ", summary_endowment_80$mean, "\n"))
  cat(paste0(var, " - Lower Limit (80): ", summary_endowment_80$lower, "\n"))
  cat(paste0(var, " - Upper Limit (80): ", summary_endowment_80$upper, "\n"))
}

