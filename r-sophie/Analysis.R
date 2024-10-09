# Load necessary packages
# If you haven't installed these packages, uncomment the install.packages lines below
# install.packages("haven")      # For reading .dta files
# install.packages("dplyr")      # For data manipulation
# install.packages("rms")        # For statistical models
# install.packages("broom")      # For tidying models
# install.packages("ggplot2")    # For plotting
# install.packages("margins")    # For marginal effects
# install.packages("stargazer")  # For creating LaTeX tables

library(haven)
library(dplyr)
library(rms)
library(broom)
library(ggplot2)
library(margins)
library(stargazer)

# # Define folder paths
# FolderDirect <- "C:/Users/thoma/Desktop/Replication_Conflict"    # Path of the folder "Replication_Conflict"
# out <- "C:/Users/thoma/Desktop/Replication_Conflict/Output"      # Path of output folder

FolderDirect <- getwd()  # Folder path for replication data
dir.create('out')
out <- sprintf('%s/out', getwd())     # Folder path for output

# Change working directory
# setwd(FolderDirect)

# Load the dataset
df <- read.csv("out/modified_data.csv")
ababa <- read_dta('conflict_replication1.dta')
write.csv(ababa, file = file.path("conflict_replication1.csv"), row.names = FALSE)

# # If you need to delete a folder (this is the equivalent of the `rmdir` command in Stata)
# if (dir.exists(out)) {
#   unlink(out, recursive = TRUE)
# }
# 
# # Create a new directory for output
# dir.create(out)

# Define variable lists
covariates <- c("age", "male", "econstudent", "bachelor", "comp1", "comp2", "likestowin", "compulsive", "trust", 
                "trustworth", "lossavers", "no_cointosses", "riskaverse", "auszahlung", "Altruist", "Selfish", "Maximin")

reg_cov <- c("male", "age", "comp1", "comp2", "likestowin", "compulsive", "trust", "trustworth", "riskaverse", 
             "lossaversion_ordinal", "Altruist", "Selfish", "Maximin")

Outcomes_beliefs <- c("belief_arming", "belief_attack", "belief_UP")

Out_beliefs_bin <- c("belief_attack", "belief_UP")
Out_beliefs_cont <- "belief_arming"

list_associations <- c("attack", "arming", "choose_UP")
reg_out_bin <- c("attack", "choose_UP", "choose_AP", "choose_AC")
reg_out_cont <- c("arming", "arm_att", "arm_def", "rel_arming")

reg_out_bin_f <- c("attack", "choose_UP")
reg_out_cont_f <- "arming"

# Perform non-parametric tests (equivalent to ranksum in Stata)
df_summary <- df %>%
  group_by(indep_obs) %>%
  summarize(
    treatment_mean = mean(treatment, na.rm = TRUE),
    attack_mean = mean(attack, na.rm = TRUE),
    arming_mean = mean(arming, na.rm = TRUE),
    choose_UP_mean = mean(choose_UP, na.rm = TRUE),
    conflict_mean = mean(conflict, na.rm = TRUE),
    unarmed_peace_mean = mean(unarmed_peace, na.rm = TRUE)
  )

# Wilcoxon rank-sum test (equivalent to ranksum in Stata)
wilcox.test(df$attack ~ df$treatment)  # p-value equivalent to 0.0571 in Stata
wilcox.test(df$arming ~ df$treatment)  # p-value equivalent to 0.0381 in Stata
wilcox.test(df$choose_UP ~ df$treatment)  # p-value equivalent to 0.4571 in Stata
wilcox.test(df$conflict ~ df$treatment)  # p-value equivalent to 0.0550 in Stata
wilcox.test(df$unarmed_peace ~ df$treatment)  # p-value equivalent to 0.333 in Stata

# Protective vs Aggressive Arming
df_protective_agg <- df %>% filter(endowment != 100)
wilcox.test(df_protective_agg$arming ~ df_protective_agg$attack)  # p-values will match Stata's output

# Create a balancing table (summarizing variables in treatment vs control)
df_period1 <- df %>% filter(period == 1, treatment == 0)

balancing_table <- df_period1 %>%
  summarise_at(vars(covariates), list(mean = ~mean(.), sd = ~sd(.), min = ~min(.), max = ~max(.)))

# Export balancing table to CSV
write.csv(balancing_table, file = paste0(out, "/BalancingTable.csv"))

# Average Treatment Effects for Binary Indicators
for (output in reg_out_bin) {
  # Mixed-effects probit model (equivalent to `meprobit` in Stata)
  model <- lrm(as.formula(paste(output, "~ treatment + period")), data = df, x = TRUE, y = TRUE)
  summary(margins(model, variables = "treatment"))
  
  model_with_cov <- lrm(as.formula(paste(output, "~ treatment + period +", paste(reg_cov, collapse = " + "))), data = df, x = TRUE, y = TRUE)
  summary(margins(model_with_cov, variables = "treatment"))
}

# Average Treatment Effects for Continuous Outcomes
for (output in reg_out_cont) {
  # Mixed-effects linear model (equivalent to `mixed` in Stata)
  model <- lmer(as.formula(paste(output, "~ treatment + period + (1 | indep_obs) + (1 | id)")), data = df)
  summary(margins(model, variables = "treatment"))
  
  model_with_cov <- lmer(as.formula(paste(output, "~ treatment + period +", paste(reg_cov, collapse = " + "), "+ (1 | indep_obs) + (1 | id)")), data = df)
  summary(margins(model_with_cov, variables = "treatment"))
}

# Effect Size Calculations
sd_arming_ue <- sd(df$arming[df$treatment == 0], na.rm = TRUE)
effect_size_arming_no_control <- -coef(model)["treatment"] / sd_arming_ue
cat("Effect Size Arming (no control):", round(effect_size_arming_no_control, 4), "\n")

# You can continue following the same structure for other effect sizes and model estimations

# Export the results to LaTeX tables
stargazer(model, model_with_cov, type = "latex", out = paste0(out, "/AverageEffects.tex"))

