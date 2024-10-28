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


############################################ Average Treatment Effects


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
  margins_rr <- margins(model_basic, variables = "treatment")
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
continuous_results[[which(reg_out_cont == "arming")]]$margins_a

#
attack_margins <- lapply(c("margins_r", "margins_a"), function(margin) {
  binary_results[[which(reg_out_bin == 'attack')]][[margin]] %>%
    summary() %>%
    select(AME, SE)
})
# attack_intercept <-
#   fixef(binary_results[[which(reg_out_bin == 'attack')]]$model_basic)["(Intercept)"]
# attack_df <- as.data.frame(attack_margins, attack_intercept)

###
# lapply(reg_out_cont, function(out){
#   continuous_results[[which(reg_out_cont == out)]]$margins_a
# })
# lapply(reg_out_cont, function(out){
#   continuous_results[[which(reg_out_cont == out)]]$margins_r
# })
# ignores 'rel_arming'

attack_margins <- lapply(c("margins_r", "margins_a"), function(margin) {
  binary_results[[which(reg_out_bin == 'attack')]][[margin]] %>%
    summary() %>%
    select(AME, SE)
})
#
reg_out_cont_thin <- c("arming", "arm_att", "arm_def")
continuous_margins <- lapply(c("margins_r", "margins_a"), function(margin) {
  lapply(reg_out_cont_thin, function(out) {
    continuous_results[[which(reg_out_cont == out)]][[margin]] %>%
      summary() %>%
      select(AME, SE)
  })
})

###

bin_intercept <-
  fixef(binary_results[[which(reg_out_bin == 'attack')]]$model_basic)["(Intercept)"]
# bin_intercept_rep <- data.frame(raw=unlist(bin_intercept_no_rep))
# bin_intercept_rep <-
#   bin_intercept_rep[rep(1:nrow(bin_intercept_rep),each=2),]
cont_intercepts <-
  lapply(reg_out_cont_thin, function(out){
    fixef(continuous_results[[which(reg_out_cont == out)]]$model_basic)["(Intercept)"]
  })

nobs_bin <- nobs(binary_results[[which(reg_out_bin == 'attack')]]$model_basic)
nobs_cont <- lapply(reg_out_cont_thin, function(out){
  nobs(continuous_results[[which(reg_out_cont == out)]]$model_basic)
})

data.frame(nobs_bin, nobs_cont)

# cont_intercepts_rep <- data.frame(raw=unlist(cont_intercepts_no_rep))
# cont_intercepts_rep <-
#   cont_intercepts_rep[rep(1:nrow(cont_intercepts_rep),each=2),]


# # (dated) Control mean para bin variable: split
# binary_results[[which(reg_out_bin == "attack")]]$margins_r %>%
#   split(., .$treatment)
# Control mean para continuous vb: intercept de fixed effects


# gets SEs for cont vbs (values inside parameters)
# most/all are larger than the stata values
# code (mess) below '#####' is an attempt to get stata values (which use HC1)
# problem: cov values in vcov matrix for treatment seem to be too low
# (and for the other vbs too large)
# lapply(reg_out_cont,
#        function(out){
#          continuous_results[[which(reg_out_cont == out)]]$margins_a %>% summary
#        })
# lapply(reg_out_cont,
#        function(out){
#          continuous_results[[which(reg_out_cont == out)]]$margins_r %>% summary
#        })

#############################################################

# install.packages('lmtest','sandwich')
# library(lmtest)
# library(sandwich)
# 
# install.packages("modelsummary", type = "source")
# 
# install.packages("clubSandwich")
# library(clubSandwich)
# 
# test_cov <- continuous_results[[which(reg_out_cont == "arming")]]$model_basic
# vcovCR(test_cov, cluster = test_cov@frame$treatment, type = "CR1")
# 
# vcov(test_cov, type='HC1')
# 
# coef(summary(test_cov, vcov=vcov(test_cov, type='HC1')))
# 
# library(modelsummary)
# 
# modelsummary(
#   test_cov,
#   vcov = list(varcov1, varcov2))


# # Calculate standard deviations for various subgroups
# SDarming_UE_def <-
#   sd(
#     conflict_replication1$arming[conflict_replication1$treatment == 0 & conflict_replication1$attack == 0],
#     na.rm = TRUE)
# SDarming_UE <- sd(conflict_replication1$arming[conflict_replication1$treatment == 0], na.rm = TRUE)
# SDatt_UE <- sd(conflict_replication1$attack[conflict_replication1$treatment == 0], na.rm = TRUE)
# SDarming_UE_agg <- sd(conflict_replication1$arming[conflict_replication1$treatment == 0 & conflict_replication1$attack == 1], na.rm = TRUE)
# 
# # Calculate effect sizes for attack and arming
# effect_att_no_control <- -1 * binary_results[[which(reg_out_bin == "attack")]]$coef_r / SDatt_UE
# cat("Effect Size Attack: (no control)", round(effect_att_no_control, 4), "\n")
# 
# effect_att_with_control <- -1 * binary_results[[which(reg_out_bin == "attack")]]$coef_a / SDatt_UE
# cat("Effect Size Attack: (with controls)", round(effect_att_with_control, 4), "\n")
# 
# effect_arming_no_control <- -1 * continuous_results[[which(reg_out_cont == "arming")]]$coef_r / SDarming_UE
# cat("Effect Size Arming (no control):", round(effect_arming_no_control, 4), "\n")
# 
# effect_arming_with_control <- -1 * continuous_results[[which(reg_out_cont == "arming")]]$coef_a / SDarming_UE
# cat("Effect Size Arming (with controls):", round(effect_arming_with_control, 4), "\n")
# 
# effect_att_def_no_control <- -1 * continuous_results[[which(reg_out_cont == "arm_def")]]$coef_r / SDarming_UE_def
# cat("Effect Size Defensive Arming: (no control)", round(effect_att_def_no_control, 4), "\n")
# 
# effect_att_def_with_control <- -1 * continuous_results[[which(reg_out_cont == "arm_def")]]$coef_a / SDarming_UE_def
# cat("Effect Size Defensive Arming: (control)", round(effect_att_def_with_control, 4), "\n")
# 
# effect_att_agg_no_control <- -1 * continuous_results[[which(reg_out_cont == "arm_att")]]$coef_r / SDarming_UE_agg
# cat("Effect Size Aggressive Arming: (no control)", round(effect_att_agg_no_control, 4), "\n")
# 
# effect_att_agg_with_control <- -1 * continuous_results[[which(reg_out_cont == "arm_att")]]$coef_a / SDarming_UE_agg
# cat("Effect Size Aggressive Arming: (control)", round(effect_att_agg_with_control, 4), "\n")


# Calculate standard deviations for various subgroups
SDarming_UE_def <-
  sd(
    conflict_replication1$arming[conflict_replication1$treatment == 0 & conflict_replication1$attack == 0],
    na.rm = TRUE)
SDarming_UE <- sd(conflict_replication1$arming[conflict_replication1$treatment == 0], na.rm = TRUE)
SDatt_UE <- sd(conflict_replication1$attack[conflict_replication1$treatment == 0], na.rm = TRUE)
SDarming_UE_agg <- sd(conflict_replication1$arming[conflict_replication1$treatment == 0 & conflict_replication1$attack == 1], na.rm = TRUE)

# Create a list to store effect sizes
effect_sizes <- list()

# Calculate effect sizes for attack and arming and store them in the list
effect_sizes$Effect_Size_Attack_No_Control <- round(-1 * binary_results[[which(reg_out_bin == "attack")]]$coef_r / SDatt_UE, 4)
effect_sizes$Effect_Size_Attack_With_Controls <- round(-1 * binary_results[[which(reg_out_bin == "attack")]]$coef_a / SDatt_UE, 4)
effect_sizes$Effect_Size_Arming_No_Control <- round(-1 * continuous_results[[which(reg_out_cont == "arming")]]$coef_r / SDarming_UE, 4)
effect_sizes$Effect_Size_Arming_With_Controls <- round(-1 * continuous_results[[which(reg_out_cont == "arming")]]$coef_a / SDarming_UE, 4)
effect_sizes$Effect_Size_Defensive_Arming_No_Control <- round(-1 * continuous_results[[which(reg_out_cont == "arm_def")]]$coef_r / SDarming_UE_def, 4)
effect_sizes$Effect_Size_Defensive_Arming_With_Controls <- round(-1 * continuous_results[[which(reg_out_cont == "arm_def")]]$coef_a / SDarming_UE_def, 4)
effect_sizes$Effect_Size_Aggressive_Arming_No_Control <- round(-1 * continuous_results[[which(reg_out_cont == "arm_att")]]$coef_r / SDarming_UE_agg, 4)
effect_sizes$Effect_Size_Aggressive_Arming_With_Controls <- round(-1 * continuous_results[[which(reg_out_cont == "arm_att")]]$coef_a / SDarming_UE_agg, 4)

# Print the effect sizes list
View(data.frame(effect_sizes=unlist(effect_sizes)))


##

# install.packages('texreg')
# install.packages('broom')
# library(texreg)
# library(broom)
# 
# # Generate tables for average treatment effects without and with controls
# texreg(
#   l = list(binary_results[[which(reg_out_bin == "attack")]]$margins_r,
#            binary_results[[which(reg_out_bin == "attack")]]$margins_a,
#            continuous_results[[which(reg_out_cont == "arming")]]$margins_r,
#            continuous_results[[which(reg_out_cont == "arming")]]$margins_a),
#   file = "AverageEffects.tex",
#   custom.model.names = c("Attack (no control)", "Attack (with controls)",
#                          "Arming (no control)", "Arming (with controls)"),
#   label = "Table 2: Average Treatment Effects - Control Means",
#   stars = c(0.05, 0.01, 0.001),
#   digits = 3
# )
# 
# # Append more results for different choices and additional regressions
# texreg(
#   l = list(binary_results[[which(reg_out_bin == "choose_UP")]]$margins_r,
#            binary_results[[which(reg_out_bin == "choose_UP")]]$margins_a),
#   file = "AverageEffects2.tex",
#   custom.model.names = c("Choose UP (no control)", "Choose UP (with controls)"),
#   append = TRUE,
#   label = "Table S5: Average Treatment Effects",
#   stars = c(0.05, 0.01, 0.001),
#   digits = 3
# )

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

############################################# Multinomial Regression ~ Figure 5


#### !!!!!!!!!!!!!!!!!! TODO
###### EXTRACT VALUES FOR PLOT
###### PLOT

#install.packages('nnet')
#install.packages('marginaleffects')
library(nnet)    # for multinom
#library(margins) # for marginal effects
#library(dplyr)   # for data manipulation
library(marginaleffects)

# Clear previous estimates
# (In R, there's no need for an explicit command)

# Preserve the dataset (useful in Stata but not needed in R, can just filter)
mlogit_data <- conflict_replication1 %>% filter(Strategies3 != 3)
# subset function yields the same
# madiamda <- conflict_replication1 %>% subset(Strategies3 != 3)

mlogit_data$Strategies3 <- relevel(as.factor(mlogit_data$Strategies3), ref = 2)

# Multinomial regression with trust, riskaverse, and lossavers
mlogit_model <- multinom(Strategies3 ~ trust + riskaverse + lossavers,
                         data = mlogit_data,
                         model=TRUE)

# marginal_effects(model_trust)
# # Marginal effects for 'trust' at specified values, holding other variables at means
# margins_trust <- margins(model_trust, at = list(trust = seq(0, 16, by = 2)))
margins_trust <-
  slopes(mlogit_model,
         newdata = datagrid(trust = seq(0, 16, by = 2)))

# N_trust <- length(plot_data_trust$predicted)
# lo_trust <-
#   plot_data_trust$predicted - 1.96 * (plot_data_trust$std.error / sqrt(N_trust))
# hi_trust <-
#   plot_data_trust$predicted + 1.96 * (plot_data_trust$std.error / sqrt(N_trust))

lo_trust <-
  plot_data_trust$predicted - abs(plot_data_trust$conf.low)
hi_trust <-
  plot_data_trust$predicted + abs(plot_data_trust$conf.high)

plot_data_trust <- margins_trust %>% subset(term=='trust')
ggplot(plot_data_trust, aes(x = trust, y = predicted, color=group)) +
  geom_line() +
  geom_ribbon(aes(ymin=lo_trust, ymax=hi_trust), alpha = 0.2, fill = "lightblue") +
  labs(
    x = "Amount sent in trust game",
    y = "Probability",
    color = "Term",
    title = "Trust"
  ) +
  theme_minimal()

##

margins_risk <-
  slopes(mlogit_model,
         newdata = datagrid(riskaverse = seq(0., 1., by = 0.1)))

plot_data_risk <- margins_risk %>% subset(term=='riskaverse')
ggplot(plot_data_risk, aes(x = riskaverse, y = predicted, color=group)) +
  geom_ribbon(aes(ymin=predicted_lo, ymax=predicted_hi), alpha = 0.2, fill = "lightblue") +
  labs(
    x = "Relative risk aversion",
    y = "Probability",
    color = "Term",
    title = "Risk aversion"
  ) +
  theme_minimal()

##

margins_loss <-
  slopes(mlogit_model,
         newdata = datagrid(lossavers = seq(0., 1., by = 0.1)))

plot_data_loss <- margins_loss %>% subset(term=='lossavers')
ggplot(plot_data_loss, aes(x = lossavers, y = predicted, color=group)) +
  geom_ribbon(aes(ymin=predicted_lo, ymax=predicted_hi), alpha = 0.2, fill = "lightblue") +
  labs(
    x = "Relative Loss aversion",
    y = "Probability",
    color = "Term",
    title = "Loss aversion"
  ) +
  theme_minimal()

# # Save results for trust to CSV
# write.csv(margins_trust, file = "Figure_5_data_trust.csv")
# # write.csv(avg_margins_trust, file = "avg_Figure_5_data_trust.csv")

########### loss

# # Marginal effects for 'riskaverse' at specified values, holding other variables at means
# margins_riskaverse <-
#   slopes(mlogit_model,
#          newdata = datagrid(riskaverse = seq(0, 1, by = 0.1)),
#          type = "probs")
# # avg_margins_riskaverse <-
# #   avg_slopes(mlogit_model,
# #          newdata = datagrid(riskaverse = seq(0, 1, by = 0.1)),
# #          type = "probs")
# 
# # Append results for riskaverse to CSV
# write.csv(margins_riskaverse, file = "Figure_5_data_risk.csv")
# # write.csv(avg_margins_riskaverse, file = "avg_Figure_5_data_risk.csv")
# 
# ###########
# 
# # Marginal effects for 'lossavers' at specified values, holding other variables at means
# margins_lossavers <-
#   slopes(mlogit_model,
#          newdata = datagrid(lossavers = seq(0, 1, by = 0.1)),
#          type = "probs")
# # avg_margins_lossavers <-
# #   avg_slopes(mlogit_model,
# #          newdata = datagrid(lossavers = seq(0, 1, by = 0.1)),
# #          type = "probs")
# 
# # Append results for lossavers to CSV
# write.csv(margins_lossavers, file = "Figure_5_data_loss.csv")
# # write.csv(avg_margins_lossavers, file = "avg_Figure_5_data_loss.csv")
# 
# # ########## binder
# #
# # thin_trust <- margins_trust %>% select('group','term','trust','std.error')
# # thin_risk <- margins_riskaverse %>% select('group','term','trust','std.error')
# # thin_loss <- margins_lossavers %>% select('group','term','trust','std.error')
# #
# # thin_loss
# # write.csv(margins_lossavers, file = "Figure_5_data_loss.csv")
# 
# #
# # # Final multinomial regression without exporting the result
# # final_model <- multinom(Strategies3 ~ trust + riskaverse + lossavers, data = data)
# # final_model_rrr <- exp(coef(final_model))  # Relative risk ratios (equivalent to rrr in Stata)
# # View(summary(final_model))

########################################## Figure 1

# data <- conflict_replication1
# 
# # List of variables to loop over
# vars <- c("attack", "arming", "arm_att", "arm_def")
# 
# # Function to calculate mean and confidence intervals
# calculate_summary <- function(data, var, treatment_value) {
#   filtered_data <- data %>% filter(treatment == treatment_value)
# 
#   mean_val <- mean(filtered_data[[var]], na.rm = TRUE)
#   sd_val <- sd(filtered_data[[var]], na.rm = TRUE)
#   n_val <- sum(!is.na(filtered_data[[var]]))  # Number of non-NA observations
# 
#   lower_limit <- mean_val - 1.96 * (sd_val / sqrt(n_val))
#   upper_limit <- mean_val + 1.96 * (sd_val / sqrt(n_val))
# 
#   return(list(mean = mean_val, lower = lower_limit, upper = upper_limit))
# }
# 
# # Loop over each variable and calculate statistics for both treatment groups
# for (var in vars) {
#   # For treatment == 0 (UNEQUAL)
#   summary_treatment_0 <- calculate_summary(data, var, treatment_value = 0)
#   cat(paste0(var, " - Mean (UNEQUAL): ", summary_treatment_0$mean, "\n"))
#   cat(paste0(var, " - Lower Limit (UNEQUAL): ", summary_treatment_0$lower, "\n"))
#   cat(paste0(var, " - Upper Limit (UNEQUAL): ", summary_treatment_0$upper, "\n"))
# 
#   # For treatment == 1 (EQUAL)
#   summary_treatment_1 <- calculate_summary(data, var, treatment_value = 1)
#   cat(paste0(var, " - Mean (EQUAL): ", summary_treatment_1$mean, "\n"))
#   cat(paste0(var, " - Lower Limit (EQUAL): ", summary_treatment_1$lower, "\n"))
#   cat(paste0(var, " - Upper Limit (EQUAL): ", summary_treatment_1$upper, "\n"))
# }


##### try2

library(dplyr)

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
  
  return(list(mean = mean_val, lower = lower_limit, upper = upper_limit, n = n_val))
}

# Initialize an empty list to store results
results_list <- list()

# Loop over each variable and calculate statistics for both treatment groups
for (var in vars) {
  # For treatment == 0 (UNEQUAL)
  summary_treatment_0 <- calculate_summary(conflict_replication1, var, treatment_value = 0)
  
  # For treatment == 1 (EQUAL)
  summary_treatment_1 <- calculate_summary(conflict_replication1, var, treatment_value = 1)
  
  # Organize results into a dataframe
  results_list[[var]] <- data.frame(
    Variable = var,
    Treatment = c("UNEQUAL", "EQUAL"),
    Mean = c(summary_treatment_0$mean, summary_treatment_1$mean),
    Lower_Limit = c(summary_treatment_0$lower, summary_treatment_1$lower),
    Upper_Limit = c(summary_treatment_0$upper, summary_treatment_1$upper),
    N = c(summary_treatment_0$n, summary_treatment_1$n)
  )
}

# Combine all results into a single dataframe
final_results <- do.call(rbind, results_list)

# Print the final dataframe
attack_fig1 <- final_results %>% subset(Variable=='attack')
invest_fig1 <- final_results %>% subset(Variable!='attack')

ggplot(invest_fig1, aes(x = Variable, y = Mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +  # Bar plot with dodged positions for treatments
  geom_errorbar(aes(ymin = Lower_Limit, ymax = Upper_Limit), 
                position = position_dodge(0.7), width = 0.2) +  # Error bars with dodged positions
  labs(title = "Mean Values with Confidence Intervals by Variable and Treatment",
       x = "Variable",
       y = "Mean Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

library(ggpattern)
# Create the subset
attack_fig1 <- final_results %>% subset(Variable == 'attack')

# Create the plot
ggplot(attack_fig1, aes(x = Treatment, y = Mean, linetype = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.6, alpha = 0.7) +  # Bar plot for attack
  geom_col_pattern(aes(pattern=Treatment)) +
  scale_pattern_manual(values=c('crosshatch', 'wave')) +
  geom_errorbar(aes(ymin = Lower_Limit, ymax = Upper_Limit), 
                position = position_dodge(0.9), width = 0.2) +  # Error bars
  labs(title = "Mean Attack Values with Confidence Intervals by Treatment",
       x = "Treatment",
       y = "Mean Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))  # Adjust x-axis labels as needed

########################################## Figure 2 - choices

# install.packages('waffle')
library(waffle)

# Fig2 unequal
data_unequal_fig2 <-
  conflict_replication1 %>%
  filter(treatment == 1) %>%
  count(Strategies3)

data_unequal_fig2$n <- data_unequal_fig2$n * 100 / sum(data_unequal_fig2$n)

waffle(data_unequal_fig2, rows = 10) +
labs(title = "Fig 2 - Unequal")

#
## Fig2 equal
data_equal_fig2 <-
  conflict_replication1 %>%
  filter(treatment == 0) %>%
  count(Strategies3)

data_equal_fig2$n <- data_equal_fig2$n * 100 / sum(data_equal_fig2$n)

waffle(data_equal_fig2, rows = 10) +
  labs(title = "Fig 2 - Equal")

########################################## Figure 3 - development of outcomes

# Collapse (mean) by treatment and period
collapsed_data <- conflict_replication1 %>%
  group_by(treatment, period) %>%
  summarize(
    unarmed_peace = mean(unarmed_peace, na.rm = TRUE),
    armed_peace = mean(armed_peace, na.rm = TRUE),
    armed_conflict = mean(armed_conflict, na.rm = TRUE)
  )

# Replace armed_peace and armed_conflict based on the described operations
collapsed_data <-
  collapsed_data %>%
  mutate(
    armed_peace = armed_peace + unarmed_peace,
    armed_conflict = armed_conflict + armed_peace
  ) %>%
  tidyr::pivot_longer(cols = c("unarmed_peace", "armed_peace", "armed_conflict"),
               names_to = "state",
               values_to = "value")


ggplot(collapsed_data, aes(x = period, y = value, fill = state)) +
  geom_area() +
  facet_wrap(~ treatment) +
  scale_fill_manual(values = c("unarmed_peace" = "lightblue",
                               "armed_peace" = "orange",
                               "armed_conflict" = "midnightblue")) +
  labs(title = "Peace and Conflict States Over Periods",
       x = "Period",
       y = "Proportion",
       fill = "State") +
  coord_cartesian(ylim = c(0, 1))


########################################## Figure 4 - Frequency of attacking and investments in arms.


# List of variables to loop over
vars <- c("attack", "arming", "arm_att", "arm_def")

# Initialize an empty dataframe to store the results
summary_df <- data.frame(
  variable = character(),
  endowment = numeric(),
  mean = numeric(),
  lower = numeric(),
  upper = numeric(),
  stringsAsFactors = FALSE
)

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
  summary_endowment_120 <- calculate_summary(conflict_replication1, var, endowment_value = 120)
  summary_df <- rbind(summary_df, data.frame(
    variable = var,
    endowment = 120,
    mean = summary_endowment_120$mean,
    lower = summary_endowment_120$lower,
    upper = summary_endowment_120$upper
  ))
  
  # For endowment == 80
  summary_endowment_80 <- calculate_summary(conflict_replication1, var, endowment_value = 80)
  summary_df <- rbind(summary_df, data.frame(
    variable = var,
    endowment = 80,
    mean = summary_endowment_80$mean,
    lower = summary_endowment_80$lower,
    upper = summary_endowment_80$upper
  ))
}

summary_df <-
  summary_df %>%
  mutate(endowment = ifelse(endowment == 120, "advantaged", "disadvantaged"))

fig4_data_attack <-
  summary_df %>%
  subset(variable=='attack')
fig4_data_inv <-
  summary_df %>%
  subset(variable!='attack')

# Create the plot
ggplot(fig4_data_attack, aes(x = endowment, y = mean)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.6, alpha = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(0.9), width = 0.2) +  # Error bars
  labs(title = "Fig 4 - Attack",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))  # Adjust x-axis labels as needed


ggplot(fig4_data_inv, aes(x = variable, y = mean, fill = endowment)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +  # Bar plot with dodged positions for treatments
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(0.7), width = 0.2) +  # Error bars with dodged positions
  labs(title = "Fig 4 - Inv",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

################################################# Avg endowment effects - bin

# install.packages(c("lme4", "margins", "broom.mixed"))

library(broom.mixed)

untreated_folk <- conflict_replication1 %>% subset(treatment==0)

# your_data_zero$endowment_scaled <- scale(your_data_zero$endowment)
# your_data_zero <- your_data_zero %>% mutate(across(all_of(reg_cov), scale))
# new120 <- your_data_zero$endowment_scaled %>% unique %>% max

untreated_folk$endowment <-
  relevel(as.factor(untreated_folk$endowment), ref = '120')

# Attacking - without covariates
mod_r_attack <- glmer(attack ~ endowment + (1 | indep_obs) + (1 | id), 
                      data = untreated_folk, family = binomial(link = "probit"))
marg_r_attack <- margins(mod_r_attack, variables = "endowment")
coef_attack_r <- summary(marg_r_attack)$AME

# parece ter sido sorte dar 0.487etc? pq pra dar isso usa mod_a_attack
# mas mod_a_attack é com reg_cov
# e a control mean não usa covariates
# parece que para bin vbs realmente o MEAN LEVEL é o intercept do model
# apesar de aqui (endowment, attack, tabela 3) ter dado até sinal diferente
# try1 <- margins(mod_r_attack, variables='endowment')
# try1_split <- split(try1, try1$endowment) # split margins between 120 and 80

# Create a formula by pasting the covariates into the model formula
form_attack <- as.formula(paste("attack ~ endowment +",
                                paste(reg_cov, collapse = " + "),
                                "+ (1 | indep_obs) + (1 | id)"))

# # Standardize endowment and covariates (if necessary)

# # If reg_cov contains other covariates, scale them too

# # Refit the model with the scaled variables
# form_attack_scaled <- as.formula(paste("attack ~ endowment_scaled +",
#                                        paste(reg_cov, collapse = " + "),
#                                        "+ (1 | indep_obs) + (1 | id)"))
# mod_a_attack_scaled <- glmer(form_attack_scaled,
#                              data = your_data,
#                              family = binomial(link = "probit"),
#                              subset = (treatment == 0),
#                              control = glmerControl(optimizer = "bobyqa",
#                                                     optCtrl = list(maxfun = 100000)))

# Fit the model with the formula
mod_a_attack <- glmer(form_attack,
                      data = untreated_folk,
                      family = binomial(link = "probit"),
                      control = glmerControl(optimizer = "bobyqa",
                                                     optCtrl = list(maxfun = 100000)))
marg_a_attack <- margins(mod_a_attack, variables = "endowment", atmeans=TRUE)
coef_attack_a <- summary(marg_a_attack)$AME

# margins_r <- margins(model_basic, variables = "treatment")
# coef_r <- summary(margins_r)$AME[1]
# continuous_results[[which(reg_out_cont == "arming")]]$coef_r

# Margins by levels of endowment
# marg_rr_attack <- margins(mod_r_attack, at = list(endowment = unique(your_data_zero$endowment)))
# coef_attack_rr <- summary(marg_rr_attack)$AME
# 
# marg_aa_attack <- margins(mod_a_attack,
#                           at = list(endowment = unique(your_data_zero$endowment)), atmeans=TRUE)

# ACTUAL marg/coef_aa/rr_attack ;; seems like `80` is not reported (cell below
# check mark on table is empty)

################################################# Avg endowment effects - cont

# install.packages('purrr')
library(lme4)
library(margins)
library(purrr)
library(dplyr)

cont_endowment_results <- map(reg_out_cont, function(output) {
  # Without covariates
  mod_r_output <- lmer(as.formula(paste0(output,
                                         " ~ endowment + (1 | indep_obs) + (1 | id)")), 
                       data = untreated_folk,
                       REML = FALSE)
  marg_r_output <- margins(mod_r_output, variables = "endowment")
  summary_r_output <- summary(marg_r_output)
  coefSE_r_output <- summary_r_output %>% select(AME, SE)
  
  # With covariates
  form_output <- as.formula(paste(output, "~ endowment +",
                                  paste(reg_cov, collapse = " + "),
                                  "+ (1 | indep_obs) + (1 | id)"))
  mod_a_output <- lmer(form_output,
                       data = untreated_folk,
                       REML = FALSE)
  marg_a_output <- margins(mod_a_output, variables = "endowment", atmeans=TRUE)
  summary_a_output <- summary(marg_a_output)
  coefSE_a_output <- summary_a_output %>% select(AME, SE)
  
  # Margins by levels of endowment
  # previously marg_aa_output etc
  control_means <-
    margins(mod_a_output, variables='endowment', atmeans=TRUE) %>%
    split(., .$endowment)
  
  # Store results in a list
  list(coefSE_r_output = coefSE_r_output, 
       coefSE_a_output = coefSE_a_output, 
       marg_a_output = marg_a_output,
       marg_r_output = marg_r_output,
       summary_a_output = summary_a_output,
       summary_r_output = summary_r_output,
       mod_a_output = mod_a_output,
       mod_r_output = mod_r_output)
})

# coef_arming_r <- results[[1]]$coef_r_output
# coef_relative_arming_r <- results[[2]]$coef_r_output
# 
# ######### prroof test
# results[[which(reg_out_cont == "arm_def")]]$coef_a_output

cont_endowment_nobs <-
  lapply(reg_out_cont_thin,function(out){
  nobs(cont_endowment_results[[which(reg_out_cont == out)]]$mod_r_output)
})
cont_endowment_intercepts <-
  lapply(reg_out_cont_thin,function(out){
    fixef(cont_endowment_results[[which(reg_out_cont == out)]]$mod_r_output)["(Intercept)"]
  })
cont_endowment_df <-
  data.frame(intercepts=unlist(cont_endowment_intercepts),
             nobs=unlist(cont_endowment_nobs))

cont_endowment_r_coefSE <-
  lapply(reg_out_cont_thin,function(out){
    cont_endowment_results[[which(reg_out_cont == out)]]$coefSE_r_output
  })
AME_SE_r_cont <- lapply(c(1:3), function(e){
  cont_endowment_r_coefSE[[e]] %>%
    mutate(AME_SE_r=paste(AME, SE, sep = " / ")) %>%
    select(AME_SE_r)
})

cont_endowment_a_coefSE <-
  lapply(reg_out_cont_thin,function(out){
    cont_endowment_results[[which(reg_out_cont == out)]]$coefSE_a_output
  })
AME_SE_a_cont <- lapply(c(1:3), function(e){
  cont_endowment_a_coefSE[[e]] %>%
    mutate(AME_SE_a=paste(AME, SE, sep = " / ")) %>%
    select(AME_SE_a)
})

cont_endowment_df$AME_SE_r <- AME_SE_r_cont
cont_endowment_df$AME_SE_a <- AME_SE_a_cont