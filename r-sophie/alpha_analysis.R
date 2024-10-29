library(lme4)
library(haven)
library(margins)
library(dplyr)
library(kableExtra)
library(ggplot2)
library(ggpattern)

rm(list = ls())

####

# Create the data frame
table1_data <- data.frame(
  Type = c("EQUAL", "EQUAL", "UNEQUAL", "UNEQUAL"),
  Ri = c(".", "100", "A", "B"),
  Beta = c("", "0.45", "0.45", "0.45"),
  Xi = c("100", "100", "120", "80"),
  Gi = c("50", "50", "70", "30"),
  U_Ci = c("23", "23", "23", "23"),
  U_Ci_Gi_0 = c("90", "90", "90", "90"),
  U_Pi = c("100", "100", "120", "80"),
  ITD = c("", "", "", "✔"),
  Peace = c("yes", "yes", "no", "no")
)

# Format the table with LaTeX headers
table1_tex <-
  kable(table1_data, "latex", escape = FALSE, col.names = c(
  "Type", "$R_i$", "$\\beta$", "$X_i$", "$G_i$", "$U_i^C$", "$U_i^C \\mid G_{j}=0$", "$U_i^P$", "ITD", "\\textit{Peace}"
)) %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  add_header_above(c(" " = 4, "Payoffs" = 4, " " = 2))

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
  
  p_r <- summary(margins_r)$p
  p_a <- summary(margins_a)$p

  list(
    model_basic = model_basic,
    model_full = model_full,
    margins_r = margins_r,
    margins_a = margins_a,
    margins_rr = margins_rr,
    margins_aa = margins_aa,
    coef_r = coef_r,
    coef_a = coef_a,
    p_a = p_a,
    p_r = p_r
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

## examples
# # The results are stored in lists, with each element corresponding to a specific output variable
# # You can access them like this:
# binary_results[[1]]$model_basic    # Basic model for the first binary outcome
# binary_results[[1]]$coef_r         # Marginal effect coefficient for the first binary outcome
# continuous_results[[1]]$margins_a  # Margins for the first continuous outcome
# continuous_results[[which(reg_out_cont == "arming")]]$margins_a

## margins table 2 bin (attack)

attack_margins <- lapply(c("margins_r", "margins_a"), function(margin) {
  binary_results[[which(reg_out_bin == 'attack')]][[margin]] %>%
    summary() %>%
    select(AME, SE)
})

# p values bin table 2
# binary_results[[which(reg_out_bin == 'attack')]]$p_r
# binary_results[[which(reg_out_bin == 'attack')]]$p_a

attack_len <- length(data.frame(attack_margins))
df_attack <-
  data.frame(attack_margins) %>%
  round(., 3) %>% 
  data.frame(
    `(1)` = paste0(.$AME, "** (", .$SE, ")"),
    `(2)` = paste0(.$AME.1, " (", .$SE.1, ")"),
    check.names = FALSE) %>%
  select(-c(1:attack_len))

## margins table 2 cont

reg_out_cont_thin <- c("arming", "arm_att", "arm_def")
continuous_margins <- lapply(c("margins_r", "margins_a"), function(margin) {
  lapply(reg_out_cont_thin, function(out) {
    continuous_results[[which(reg_out_cont == out)]][[margin]] %>%
      summary() %>%
      select(AME, SE)
  })
})

p_cont_r_2 <- unlist(lapply(reg_out_cont_thin, function(out){
  continuous_results[[which(reg_out_cont == out)]]$p_r
}))
p_cont_a_2 <- unlist(lapply(reg_out_cont_thin, function(out){
  continuous_results[[which(reg_out_cont == out)]]$p_a
}))
unlist(mapply(c, p_cont_r_2, p_cont_a_2, SIMPLIFY = FALSE))

cont_len <- length(data.frame(continuous_margins))
df_cont <-
  data.frame(continuous_margins) %>%
  round(., 3) %>% 
  data.frame(
    `(3)` = paste0(.$AME, "** (", .$SE, ")"),
    `(4)` = paste0(.$AME.1, "* (", .$SE.1, ")"),
    `(5)` = paste0(.$AME.2, " (", .$SE.2, ")"),
    `(6)` = paste0(.$AME.3, " (", .$SE.3, ")"),
    `(7)` = paste0(.$AME.4, "*** (", .$SE.4, ")"),
    `(8)` = paste0(.$AME.5, "** (", .$SE.5, ")"),
    check.names = FALSE) %>%
  select(-c(1:cont_len))

coef_2 <- cbind(df_attack, df_cont)

## Intercepts (control means) table 2

bin_intercept <-
  fixef(binary_results[[which(reg_out_bin == 'attack')]]$model_basic)["(Intercept)"] %>%
  round(., 3)
cont_intercepts <-
  lapply(reg_out_cont_thin, function(out){
    fixef(continuous_results[[which(reg_out_cont == out)]]$model_basic)["(Intercept)"] %>%
      round(.,3)
  })

df_intercept_bin <-
  lapply(bin_intercept, function(x) list(as.character(x), "")) %>%
  unlist(., recursive = FALSE) %>%
  data.frame
df_intercept_cont <-
  lapply(cont_intercepts, function(x) list(as.character(x), "")) %>%
  unlist(., recursive = FALSE) %>%
  data.frame

intercept_2 <- cbind(df_intercept_bin, df_intercept_cont) 
names(intercept_2) <- names(coef_2)

# nobs table 2

nobs_bin <- nobs(binary_results[[which(reg_out_bin == 'attack')]]$model_basic)
nobs_cont <- lapply(reg_out_cont_thin, function(out){
  nobs(continuous_results[[which(reg_out_cont == out)]]$model_basic)
})

df_nobs_bin <-
  lapply(nobs_bin, function(x) list(as.character(x), "")) %>%
  unlist(., recursive = FALSE) %>%
  data.frame
df_nobs_cont <-
  lapply(nobs_cont, function(x) list(as.character(x), "")) %>%
  unlist(., recursive = FALSE) %>%
  data.frame

nobs_2 <- cbind(df_nobs_bin, df_nobs_cont)
names(nobs_2) <- names(coef_2)


## effect sizes table 2

# Calculate standard deviations for various subgroups
SDarming_UE_def <-
  sd(
    conflict_replication1$arming[conflict_replication1$treatment == 0 & conflict_replication1$attack == 0],
    na.rm = TRUE)
SDarming_UE <- sd(conflict_replication1$arming[conflict_replication1$treatment == 0], na.rm = TRUE)
SDatt_UE <- sd(conflict_replication1$attack[conflict_replication1$treatment == 0], na.rm = TRUE)
SDarming_UE_agg <- sd(conflict_replication1$arming[conflict_replication1$treatment == 0 & conflict_replication1$attack == 1], na.rm = TRUE)

# Create a list to store effect sizes
effect_sizes <- c()

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
effects_2 <- data.frame(effect_sizes, row.names = NULL) %>% round(.,2)
names(effects_2) <- names(coef_2)

## MDES table 2

attack_SE_2 <-
  unlist(lapply(attack_margins, function(x) x["SE"]))

cont_SE_2 <-
  unlist(
    lapply(continuous_margins, function(inner_list) {
      lapply(inner_list, function(x) x["SE"])
    })
  )

unround_MDES_2 <- c(attack_SE_2, cont_SE_2) * 2.8

MDES_2 <- round(unround_MDES_2, 2)

names(MDES_2) <- names(coef_2)
  
##### TABLE 2 view kbl

table2_df <- rbind(coef_2, intercept_2, nobs_2, effects_2, MDES_2)

rownames(table2_df) <- c('EQUAL',
                         'Control mean',
                         'Observations',
                         'Effect size',
                         'MDES')

table2 <-
  table2_df %>%
  kbl(align='c') %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  add_header_above(c('',
                     "Attack" = 2,
                     "Arming" = 2,
                     "Arming (def)" = 2,
                     "Arming (att)" = 2))


############################################# Multinomial Regression ~ Figure 5


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

plot_data_trust <- margins_trust %>% subset(term=='trust')

lo_trust <-
  plot_data_trust$predicted - abs(plot_data_trust$conf.low)
hi_trust <-
  plot_data_trust$predicted + abs(plot_data_trust$conf.high)

ggplot(plot_data_trust, aes(x = trust, y = predicted, color=group)) +
  geom_line() +
  geom_ribbon(aes(ymin=lo_trust, ymax=hi_trust), alpha = 0.2, fill = "lightblue") +
  labs(
    x = "Amount sent in trust game",
    y = "Probability",
    color = "Term",
    title = "Trust"
  )

##

margins_risk <-
  slopes(mlogit_model,
         newdata = datagrid(riskaverse = seq(0., 1., by = 0.1)))

plot_data_risk <- margins_risk %>% subset(term=='riskaverse')

# lo_risk <-
#   plot_data_risk$predicted - plot_data_risk$conf.low
# hi_risk <-
#   plot_data_risk$predicted + plot_data_risk$conf.high

ggplot(plot_data_risk, aes(x = riskaverse, y = predicted, color=group)) +
  geom_ribbon(aes(ymin=predicted_lo, ymax=predicted_hi), alpha = 0.2, fill = "lightblue") +
  labs(
    x = "Relative risk aversion",
    y = "Probability",
    color = "Term",
    title = "Risk aversion"
  )

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
  )

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

# List of variables to loop over
vars <- c("attack", "arming", "arm_att", "arm_def")

# Function to calculate mean and confidence intervals
prepare_fig1 <- function(data, var, treatment_value) {
  filtered_data <- data %>% filter(treatment == treatment_value)
  
  mean_val <- mean(filtered_data[[var]], na.rm = TRUE)
  sd_val <- sd(filtered_data[[var]], na.rm = TRUE)
  n_val <- sum(!is.na(filtered_data[[var]]))  # Number of non-NA observations
  
  lower_limit <- mean_val - 1.96 * (sd_val / sqrt(n_val))
  upper_limit <- mean_val + 1.96 * (sd_val / sqrt(n_val))
  
  return(list(mean = mean_val, lower = lower_limit, upper = upper_limit, n = n_val))
}

# Initialize an empty list to store results
fig1_data_unbound <- list()

# Loop over each variable and calculate statistics for both treatment groups
for (var in vars) {
  # For treatment == 0 (UNEQUAL)
  summary_treatment_0 <- prepare_fig1(conflict_replication1, var, treatment_value = 0)
  
  # For treatment == 1 (EQUAL)
  summary_treatment_1 <- prepare_fig1(conflict_replication1, var, treatment_value = 1)
  
  # Organize results into a dataframe
  fig1_data_unbound[[var]] <- data.frame(
    Variable = var,
    Treatment = c("UNEQUAL", "EQUAL"),
    Mean = c(summary_treatment_0$mean, summary_treatment_1$mean),
    Lower_Limit = c(summary_treatment_0$lower, summary_treatment_1$lower),
    Upper_Limit = c(summary_treatment_0$upper, summary_treatment_1$upper),
    N = c(summary_treatment_0$n, summary_treatment_1$n)
  )
}

# Combine all results into a single dataframe
fig1_data <- do.call(rbind, fig1_data_unbound)

# Print the final dataframe
attack_fig1 <- fig1_data %>% subset(Variable=='attack')

invest_fig1 <-
  fig1_data %>%
  subset(Variable!='attack') %>%
  mutate(Variable = recode(Variable, 
                           "arming" = "Overall", 
                           "arm_att" = "Offensive", 
                           "arm_def" = "Defensive"))
invest_fig1$Variable <- factor(invest_fig1$Variable,
                               levels = c("Overall", "Defensive", "Offensive"))

# Investing plot - Fig 1
ggplot(invest_fig1, aes(x = Variable, y = Mean, fill = Treatment)) +
  geom_hline(yintercept=50, linetype="dashed", color = "gray10") +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +  # Bar plot with dodged positions for treatments
  geom_errorbar(aes(ymin = Lower_Limit, ymax = Upper_Limit), 
                position = position_dodge(0.7), width = 0.2) +  # Error bars with dodged positions
  labs(title = "Investment in arms (tokens)",
       x = "",
       y = "Tokens in arming")

# Attack plot - Fig 1
ggplot(attack_fig1, aes(x = Treatment, y = Mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.6) +  # Bar plot for attack
  geom_errorbar(aes(ymin = Lower_Limit, ymax = Upper_Limit), 
                position = position_dodge(0.9), width = 0.2) +  # Error bars
  labs(title = "Attack",
       x = "",
       y = "Frequency of choosing conflict")

########################################## Figure 2 - choices

# install.packages('waffle')
library(waffle)

strategies <- c('UP','AP','UC','AC')

# Fig2 unequal
data_unequal_fig2 <-
  conflict_replication1 %>%
  filter(treatment == 1) %>%
  count(Strategies3)
#
data_unequal_fig2$n <- ceiling(data_unequal_fig2$n * 100 / sum(data_unequal_fig2$n))
data_unequal_fig2$n <- data_unequal_fig2$n - c(0,1,0,0)
#
data_unequal_fig2$Strategies3 <- strategies
#
fig2_unequal_plot <-
  waffle(data_unequal_fig2, rows = 10, size = 0.8) +
  labs(title = "Unequal")

#
## Fig2 equal
data_equal_fig2 <-
  conflict_replication1 %>%
  filter(treatment == 0) %>%
  count(Strategies3)

waffle(data_equal_fig2, rows = 10) +
  labs(title = "Equal")
#
data_equal_fig2$n <- ceiling(data_equal_fig2$n * 100 / sum(data_equal_fig2$n))
data_equal_fig2$n <- data_equal_fig2$n - c(0,0,0,1)
#
data_equal_fig2$Strategies3 <- strategies
#
fig2_equal_plot <- waffle(data_equal_fig2, rows = 10, , size = 0.8) +
  labs(title = "Equal")

#
# plot both side by side
require(gridExtra)
grid.arrange(fig2_unequal_plot, fig2_equal_plot, ncol=2)


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
  labs(title = "Development of outcomes",
       x = "Period",
       y = "Frequency",
       fill = "Choice") +
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
  subset(variable!='attack') %>%
  mutate(variable = recode(variable, 
                           "arming" = "Overall", 
                           "arm_att" = "Offensive", 
                           "arm_def" = "Defensive"))
fig4_data_inv$variable <-
  factor(fig4_data_inv$variable,
         levels = c("Overall", "Defensive", "Offensive"))

# Create the plot
ggplot(fig4_data_attack, aes(x = endowment, y = mean, fill = endowment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(0.9), width = 0.2) +  # Error bars
  labs(title = "Attacking",
       x = "",
       y = "Frequency of choosing conflict")

ggplot(fig4_data_inv, aes(x = variable, y = mean, fill = endowment)) +
  geom_hline(yintercept=50, linetype="dashed", color = "gray10") +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +  # Bar plot with dodged positions for treatments
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(0.7), width = 0.2) +  # Error bars with dodged positions
  labs(title = "Investment in Arms (tokens)",
       x = "",
       y = "Tokens in arming")


####################### Avg endowment effects - table 3
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

marg_r_attack <- margins(mod_r_attack, variables = "endowment", atmeans=TRUE)
AME_attack_r_3 <- summary(marg_r_attack) %>% select(AME)
SE_attack_r_3 <- summary(marg_r_attack) %>% select(SE)
coef_attack_r <- summary(marg_r_attack) %>% select(AME,SE) %>% round(.,3)

marg_a_attack <- margins(mod_a_attack, variables = "endowment", atmeans=TRUE)
AME_attack_a_3 <- summary(marg_a_attack) %>% select(AME)
SE_attack_a_3 <- summary(marg_a_attack) %>% select(SE)
coef_attack_a <- summary(marg_a_attack) %>% select(AME,SE) %>% round(.,3)

coef_attack_3 <- data.frame(c(coef_attack_r, coef_attack_a))

## p-value table 3 bin

p_bin_r_3 <- summary(marg_r_attack) %>% select(p)
p_bin_a_3 <- summary(marg_a_attack) %>% select(p)
# unlist(c(p_bin_r_3, p_bin_a_3)) # nenhum significante


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
  AME_SE_r_output <- summary_r_output %>% select(AME, SE)
  coef_r <- summary_r_output %>% select(AME)
  p_r <- summary_r_output %>% select(p)
  
  # With covariates
  form_output <- as.formula(paste(output, "~ endowment +",
                                  paste(reg_cov, collapse = " + "),
                                  "+ (1 | indep_obs) + (1 | id)"))
  mod_a_output <- lmer(form_output,
                       data = untreated_folk,
                       REML = FALSE)
  marg_a_output <- margins(mod_a_output, variables = "endowment", atmeans=TRUE)
  summary_a_output <- summary(marg_a_output)
  AME_SE_a_output <- summary_a_output %>% select(AME, SE)
  coef_a <- summary_a_output %>% select(AME)
  p_a <- summary_a_output %>% select(p)
  
  # Store results in a list
  list(AME_SE_r_output = AME_SE_r_output, 
       AME_SE_a_output = AME_SE_a_output, 
       coef_a = coef_a,
       coef_r = coef_r,
       p_r = p_r,
       p_a = p_a,
       marg_a_output = marg_a_output,
       marg_r_output = marg_r_output,
       summary_a_output = summary_a_output,
       summary_r_output = summary_r_output,
       mod_a_output = mod_a_output,
       mod_r_output = mod_r_output)
})

###

## margins table 3 bin (attack)

attack_len_3 <- length(coef_attack_3)
df_attack_3 <-
  coef_attack_3 %>%
  data.frame(
    `(1)` = paste0(.$AME, " (", .$SE, ")"),
    `(2)` = paste0(.$AME.1, " (", .$SE.1, ")"),
    check.names = FALSE) %>%
  select(-c(1:attack_len_3))

## margins table 3 cont

continuous_margins_3 <- lapply(c("marg_r_output", "marg_a_output"), function(margin) {
  lapply(reg_out_cont_thin, function(out) {
    cont_endowment_results[[which(reg_out_cont == out)]][[margin]] %>%
      summary() %>%
      select(AME, SE)
  })
})

### p values table 3 cont
# p_cont_r_3 <- unlist(lapply(reg_out_cont_thin, function(out){
#   cont_endowment_results[[which(reg_out_cont == out)]]$p_r
# }))
# p_cont_a_3 <- unlist(lapply(reg_out_cont_thin, function(out){
#   cont_endowment_results[[which(reg_out_cont == out)]]$p_a
# }))
# unlist(mapply(c, p_cont_r_3, p_cont_a_3, SIMPLIFY = FALSE))

cont_len <- length(data.frame(continuous_margins_3))
df_cont_3 <-
  data.frame(continuous_margins_3) %>%
  round(., 3) %>% 
  data.frame(
    `(3)` = paste0(.$AME, " (", .$SE, ")"),
    `(4)` = paste0(.$AME.1, "* (", .$SE.1, ")"),
    `(5)` = paste0(.$AME.2, "* (", .$SE.2, ")"),
    `(6)` = paste0(.$AME.3, "* (", .$SE.3, ")"),
    `(7)` = paste0(.$AME.4, "* (", .$SE.4, ")"),
    `(8)` = paste0(.$AME.5, "** (", .$SE.5, ")"),
    check.names = FALSE) %>%
  select(-c(1:cont_len))

coef_3 <- cbind(df_attack_3, df_cont_3)

## Intercepts (control means) table 3

bin_intercept_3 <-
  fixef(mod_r_attack)["(Intercept)"] %>%
  round(., 3)
cont_intercepts_3 <-
  lapply(reg_out_cont_thin, function(out){
    fixef(cont_endowment_results[[which(reg_out_cont == out)]]$mod_a_output)["(Intercept)"] %>%
      round(.,3)
  })

df_intercept_bin_3 <-
  lapply(bin_intercept_3, function(x) list(as.character(x), "")) %>%
  unlist(., recursive = FALSE) %>%
  data.frame
df_intercept_cont_3 <-
  lapply(cont_intercepts, function(x) list(as.character(x), "")) %>%
  unlist(., recursive = FALSE) %>%
  data.frame

intercept_3 <- cbind(df_intercept_bin_3, df_intercept_cont_3) 
names(intercept_3) <- names(coef_3)

# nobs table 3

nobs_bin_3 <- nobs(mod_r_attack)
nobs_cont_3 <- lapply(reg_out_cont_thin, function(out){
  nobs(cont_endowment_results[[which(reg_out_cont == out)]]$mod_r_output)
})

df_nobs_bin_3 <-
  lapply(nobs_bin_3, function(x) list(as.character(x), "")) %>%
  unlist(., recursive = FALSE) %>%
  data.frame
df_nobs_cont_3 <-
  lapply(nobs_cont_3, function(x) list(as.character(x), "")) %>%
  unlist(., recursive = FALSE) %>%
  data.frame

nobs_3 <- cbind(df_nobs_bin_3, df_nobs_cont_3)
names(nobs_3) <- names(coef_3)


## effect sizes table 3

# Create a list to store effect sizes
effect_sizes_3 <- c()

# Calculate effect sizes for attack and arming and store them in the list
effect_sizes_3$Effect_Size_Attack_No_Control <- round(-1 * AME_attack_r_3 / SDatt_UE, 4)
effect_sizes_3$Effect_Size_Attack_With_Controls <- round(-1 * AME_attack_a_3 / SDatt_UE, 4)
effect_sizes_3$Effect_Size_Arming_No_Control <- round(-1 * cont_endowment_results[[which(reg_out_cont == "arming")]]$coef_r / SDarming_UE, 4)
effect_sizes_3$Effect_Size_Arming_With_Controls <- round(-1 * cont_endowment_results[[which(reg_out_cont == "arming")]]$coef_a / SDarming_UE, 4)
effect_sizes_3$Effect_Size_Defensive_Arming_No_Control <- round(-1 * cont_endowment_results[[which(reg_out_cont == "arm_def")]]$coef_r / SDarming_UE_def, 4)
effect_sizes_3$Effect_Size_Defensive_Arming_With_Controls <- round(-1 * cont_endowment_results[[which(reg_out_cont == "arm_def")]]$coef_a / SDarming_UE_def, 4)
effect_sizes_3$Effect_Size_Aggressive_Arming_No_Control <- round(-1 * cont_endowment_results[[which(reg_out_cont == "arm_att")]]$coef_r / SDarming_UE_agg, 4)
effect_sizes_3$Effect_Size_Aggressive_Arming_With_Controls <- round(-1 * cont_endowment_results[[which(reg_out_cont == "arm_att")]]$coef_a / SDarming_UE_agg, 4)

# Print the effect sizes list
effects_3 <- data.frame(effect_sizes_3, row.names = NULL) %>% round(.,2)
names(effects_3) <- names(coef_3)

## MDES table 3

attack_SE_3 <- unlist(c(SE_attack_r_3, SE_attack_a_3))

cont_SE_3 <-
  unlist(
    lapply(continuous_margins_3, function(inner_list) {
      lapply(inner_list, function(x) x["SE"])
    })
  )

unround_MDES_3 <- c(attack_SE_3, cont_SE_3) * 2.8

MDES_3 <- round(unround_MDES_3, 2)

names(MDES_3) <- names(coef_3)

##### TABLE 3 view kbl

table3_df <- rbind(coef_3, intercept_3, nobs_3, effects_3, MDES_3)

rownames(table3_df) <- c('Player is disadvantaged',
                         'Control mean',
                         'Observations',
                         'Effect size',
                         'MDES')

table3 <-
  table3_df %>%
  kbl(align='c') %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  add_header_above(c('',
                     "Attack" = 2,
                     "Arming" = 2,
                     "Arming (def)" = 2,
                     "Arming (att)" = 2))