# Clear all
rm(list = ls())

# Define folder paths
FolderDirect <- "D:/Users/B435076/Desktop/R-S/"
out <- "D:/Users/B435076/Desktop/R-S/Data and analysis/out"
setwd(FolderDirect)



library(dplyr)
library(haven)
library(broom)      # For tidying model output
library(margins)    # For computing marginal effects
library(stargazer)  # For creating tables in text or CSV


# Run data preparation R script (assumes you have an R script equivalent to the do-file)
source("D:/Users/B435076/Desktop/R-S/Teste limpa (1).R")

# Load Stata data file
ta <- read.csv("D:/Users/B435076/Desktop/R-S/Data and analysis/out/modified_data2.csv")


# Define variable lists
covariates <- c("age", "male", "econstudent", "bachelor", "comp1", "comp2", "likestowin", "compulsive", "trust", "trustworth", "lossavers", "no_cointosses", "riskaverse", "auszahlung", "Altruist", "Selfish", "Maximin")
reg_cov <- c("male", "age", "comp1", "comp2", "likestowin", "compulsive", "trust", "trustworth", "riskaverse", "ib0.lossaversion_ordinal", "Altruist", "Selfish", "Maximin")

Outcomes_beliefs <- c("belief_arming", "belief_attack", "belief_UP")
Out_beliefs_bin <- c("belief_attack", "belief_UP")
Out_beliefs_cont <- c("belief_arming")

list_associations <- c("attack", "arming", "choose_UP")
reg_out_bin <- c("attack", "choose_UP", "choose_AP", "choose_AC")
reg_out_cont <- c("arming", "arm_att", "arm_def", "rel_arming")

reg_out_bin_f <- c("attack", "choose_UP")
reg_out_cont_f <- c("arming")

#NON-PARAMETRIC TESTS

# Preserve the original data
ta_original <- ta  # Save a copy of the data

# Collapse (mean) for treatment and other variables by indep_obs
ta_collapsed <- ta %>%
  group_by(indep_obs) %>%
  summarize(across(c(treatment, attack, arming, choose_UP, conflict, unarmed_peace), mean, na.rm = TRUE))

# Perform ranksum test (Wilcoxon rank-sum) for each variable by treatment
wilcox.test(attack ~ treatment, data = ta_collapsed)       # p-value equivalent to 0.0571
wilcox.test(arming ~ treatment, data = ta_collapsed)       # p-value equivalent to 0.0381
wilcox.test(choose_UP ~ treatment, data = ta_collapsed)    # p-value equivalent to 0.4571
wilcox.test(conflict ~ treatment, data = ta_collapsed)     # p-value equivalent to 0.0550
wilcox.test(unarmed_peace ~ treatment, data = ta_collapsed)  # p-value equivalent to 0.333

###Protective vs Agressive

# Preserve the original data again
ta_original <- ta  # Save a copy of the data

# Filter out observations where endowment is not equal to 100
ta_filtered <- ta %>%
  filter(endowment != 100)

# Collapse (mean) for treatment and arming by indep_obs and attack
ta_collapsed_arming <- ta_filtered %>%
  group_by(indep_obs, attack) %>%
  summarize(arming = mean(arming, na.rm = TRUE))

# Perform ranksum test (Wilcoxon rank-sum) for arming by attack
# First, p-value for the whole dataset
p_whole <- wilcox.test(arming ~ attack, data = ta_collapsed_arming, exact = FALSE)

# Now, perform ranksum tests for different endowment categories (e.g., 80 and 120)
# UNEQUAL: 80
ta_unequal_80 <- ta_filtered %>%
  filter(endowment == 80)

ta_collapsed_80 <- ta_unequal_80 %>%
  group_by(indep_obs, attack) %>%
  summarize(arming = mean(arming, na.rm = TRUE))

p_unequal_80 <- wilcox.test(arming ~ attack, data = ta_collapsed_80, exact = FALSE)

# UNEQUAL: 120
ta_unequal_120 <- ta_filtered %>%
  filter(endowment == 120)

ta_collapsed_120 <- ta_unequal_120 %>%
  group_by(indep_obs, attack) %>%
  summarize(arming = mean(arming, na.rm = TRUE))

p_unequal_120 <- wilcox.test(arming ~ attack, data = ta_collapsed_120, exact = FALSE)

# Restore the original data
ta <- ta_original

# Print p-values
p_whole$p.value  # p-value for whole dataset
p_unequal_80$p.value  # p-value for UNEQUAL 80
p_unequal_120$p.value  # p-value for UNEQUAL 120


###BALANCING TABLE##

# Filter data to period 1 and treatment = 0 for control group summary stats
ta_control <- ta %>%
  filter(period == 1 & treatment == 0)

# Calculate summary statistics for the control group (treatment == 0)
summary_stats <- ta_control %>%
  summarise(across(c(age, male, econstudent, bachelor, comp1, comp2, likestowin, compulsive, trust, trustworth, lossavers, no_cointosses, riskaverse, auszahlung, Altruist, Selfish, Maximin),
                   list(mean = ~ mean(. , na.rm = TRUE), 
                        sd = ~ sd(. , na.rm = TRUE),
                        min = ~ min(. , na.rm = TRUE), 
                        max = ~ max(. , na.rm = TRUE), 
                        count = ~ sum(!is.na(.)))))

# Save the summary statistics to CSV
write.csv(summary_stats, file = "BalancingTable_new.csv", row.names = FALSE)

# Define the covariates
covariates <- c("age", "male", "econstudent", "bachelor", "comp1", "comp2", "likestowin", "compulsive", "trust", "trustworth", "riskaverse", "Altruist", "Selfish", "Maximin")

# Empty list to store regression results
reg_results <- list()

# Loop through each covariate
for (cov in covariates) {
  
  # Run regression of covariate on treatment with robust standard errors
  model <- lm(as.formula(paste(cov, "~ treatment")), data = df %>% filter(period == 1))
  
  # Compute marginal means using the margins package
  margins_model <- margins(model)
  
  # Extract the marginal mean for UNEQUAL (treatment = 0)
  mean_unequal <- summary(margins_model)$AME[1]
  
  # Compute the difference between treatment groups using the model's coefficients
  coef_equal <- coef(model)["treatment"]
  
  # Extract confidence interval range
  conf_int <- confint(model)["treatment",]
  half_ci_range <- (conf_int[2] - conf_int[1]) / 2
  
  # Store results in a dataframe
  reg_results[[cov]] <- data.frame(Covariate = cov,
                                   Mean_UNEQUAL = mean_unequal,
                                   Coef_EQUAL = coef_equal,
                                   Half_CI_Range = half_ci_range)
}

# Combine all regression results into one dataframe
reg_results_df <- do.call(rbind, reg_results)

# Export regression results to CSV
write.csv(reg_results_df, file = "BalancingTable_new.csv", append = TRUE, row.names = FALSE)

tab_test <- read.csv("D:/Users/B435076/Desktop/R-S/Data and analysis/BalancingTable_new.csv")
tab_bal <- read.csv("D:/Users/B435076/Desktop/R-S/Data and analysis/out/Balencingtable.csv")
