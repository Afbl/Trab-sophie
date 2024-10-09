# ---
# title: "R Notebook"
# output: html_notebook
# ---
# This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 
# Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 
# ```{r}
# plot(cars)
# ```
# Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.
# When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).
# The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.

# Load necessary libraries
library(haven)  # for loading .dta files
library(dplyr)  # for data manipulation
library(forcats) # for handling factors

# # Define paths to the folders
# folder_direct <- getwd()  # Folder path for replication data
# dir.create('out')
# out <- sprintf('%s/out', getwd())     # Folder path for output

# Load the dataset
gamedata <- read_dta("gamedata.dta")
head(gamedata)
colnames(gamedata)

# 'equal' is renamed to 'treatment'
# therefore equal \equiv 1
# 'unequal' is dropped
gamedata <- gamedata %>%
  rename(treatment = equal) %>%
  select(-unequal)

## Identification
# Data will be identified by columns 'id' (participantcode)
# and 'session' (sessioncode)
gamedata <- gamedata %>%
  arrange(treatment, participantcode) %>%  # sort wrt 'treatment' and 'participantcode'
  mutate(test = row_number()) %>%  # create column based on row indices
  group_by(participantcode) %>%  # group by 'participantcode'
  mutate(id = min(test)) %>%  # create 'id' column
  group_by(sessioncode) %>%  
  mutate(session = min(test)) %>% 
  ungroup() %>%
  select(-test) # will only drop 'test' for now

# Some variables renamed
gamedata <- gamedata %>%
  rename(
    period = subsessionround_number,
    endowment = playerendowment,
    attack = playerattack,
    production = playerproduction,
    arming = playerarming,
    opponent_production = playerother_production,
    opponent_arming = playerother_arming,
    opponent_attack = playerother_attack,
    auszahlung = playerfinal_payoff_rmb,
    win_conflict = playeris_winner,
    belief_attack = playerattack_est,
    belief_arming = playerarming_est
  )

# Create "Matching-Groups"
# !!!! explain, review game design
# Code in STATA commands ``*tab  indep_obs treatment`` at the end
gamedata <- gamedata %>%
  mutate(indep_obs = session) %>%
  mutate(indep_obs = ifelse(participantid_in_session > 8 & treatment, session + 1, indep_obs)) %>%
  mutate(indep_obs = ifelse(participantid_in_session > 16 & treatment, session + 2, indep_obs))

# Was there a conflict?
gamedata <- gamedata %>%
  mutate(conflict = ifelse(attack == 1 & opponent_attack == 1, 1, 0)) %>%
  mutate(win_conflict = ifelse(conflict == 0, NA, win_conflict))

# Relative Arming and labels
gamedata <- gamedata %>%
  mutate(rel_arming = arming / endowment) %>%
  mutate(arm_def = ifelse(attack == 0, arming, NA)) %>%
  mutate(arm_att = ifelse(attack == 1, arming, NA)) %>%
  


# Outcome: Unarmed Peace
gamedata <- gamedata %>%
  mutate(unarmed_peace = ifelse(conflict == 0 & production == endowment & (200 - endowment) == opponent_production, 1, 0))

# Individual Strategies
gamedata <- gamedata %>%
  mutate(Strategies3 = case_when(
    attack == 0 & production == endowment ~ 1,
    attack == 0 & arming > 0 ~ 2,
    attack == 1 & production == endowment ~ 3,
    attack == 1 & arming > 0 ~ 4
  ))

# Binary choice variables
gamedata <- gamedata %>%
  mutate(choose_UP = ifelse(attack == 0 & production == endowment, 1, 0)) %>%
  mutate(choose_UC = ifelse(attack == 1 & production == endowment, 1, 0)) %>%
  mutate(choose_AP = ifelse(attack == 0 & arming > 0, 1, 0)) %>%
  mutate(choose_AC = ifelse(attack == 1 & arming > 0, 1, 0))

# Label variables related to Unarmed Peace and Choices
# gamedata <- gamedata %>%
#   rename(
#     belief_UP = unarmed_peace,
#     check_game = choose_UP
#   )
gamedata <- gamedata %>%
  rename(
    belief_UP = unarmed_peace,
    check_game = choose_UP
  )

# Questionnaire: Risk Aversion
gamedata <- gamedata %>%
  mutate(riskaverse = (11 - playerqt1_risk) / 11) %>%
  select(-playerqt1_risk)

# Rename demographic variables
gamedata <- gamedata %>%
  rename(
    age = playerdem_age,
    male = playerdem_gender
  ) %>%
  mutate(male = recode(male, "2" = 0, "3" = 0))

# Student and degree variables
gamedata <- gamedata %>%
  mutate(econstudent = ifelse(playerdem_study == 12, 1, 0)) %>%
  rename(bachelor = playerdem_grad) %>%
  mutate(bachelor = recode(bachelor, "2" = 0, "3" = 0))

# Rename questionnaire variables
gamedata <- gamedata %>%
  rename(
    comp1 = playerqt2_compete1,
    comp2 = playerqt3_compete2,
    likestowin = playerqt4_win,
    compulsive = playerqt5_impulsive
  )

# Continue for other sections, adjusting the logic where needed

# Save the modified dataset
write.csv(gamedata, file = file.path(out, "modified_data.csv"), row.names = FALSE)
