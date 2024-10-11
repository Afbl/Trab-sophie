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

FolderDirect <- 'D:/Users/B435076/Desktop/R-S/Data and analysis'
dir.create('D:/Users/B435076/Desktop/R-S/Data and analysis/out')
out <- ('D:/Users/B435076/Desktop/R-S/Data and analysis/out')
setwd(FolderDirect)

# Load the dataset
gamedata <- read_dta('D:/Users/B435076/Desktop/R-S/Data and analysis/datagame.dta')
head(gamedata)
colnames(gamedata)

# 'equal' is renamed to 'treatment'
# therefore equal \equiv 1
# 'unequal' is dropped
gamedata <- gamedata %>%
  rename(treatment = equal)%>%
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
gamedata$treatment <- as.integer(gamedata$treatment)

gamedata <- gamedata %>%
  mutate(indep_obs = session) %>%
  mutate(indep_obs = ifelse(participantid_in_session > 8 & treatment, session + 1, indep_obs)) %>%
  mutate(indep_obs = ifelse(participantid_in_session > 16 & treatment, session + 2, indep_obs))

# Was there a conflict?
gamedata <- gamedata %>%
  mutate(conflict = ifelse(attack == 1 & opponent_attack == 1, 1, 0)) %>%
  mutate(win_conflict = ifelse(conflict == 0, NA, win_conflict))

attr(gamedata$conflict, "label") <- "Conflict"

gamedata <- gamedata %>%
  # Relative Arming
  mutate(rel_arming = arming / endowment) %>%
  # Defensive vs. Offensive Arming
  mutate(arm_def = ifelse(attack == 0, arming, NA),
         arm_att = ifelse(attack == 1, arming, NA))

# Add variable labels as metadata (not actual columns)
attr(gamedata$rel_arming, "label") <- "Relative Arming Level"
attr(gamedata$arm_def, "label") <- "Defensive Arming"
attr(gamedata$arm_att, "label") <- "Offensive Arming"


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
  ))%>%
  mutate(Strategies3 = factor(Strategies3,
                            levels = c(1, 2, 3, 4),
                            labels = c("Unarmed Peace", "Armed Peace", "Unarmed Conflict", "Armed Conflict")))

# Binary choice variables
gamedata <- gamedata %>%
  mutate(choose_UP = ifelse(attack == 0 & production == endowment, 1, 0)) %>%
  mutate(choose_UC = ifelse(attack == 1 & production == endowment, 1, 0)) %>%
  mutate(choose_AP = ifelse(attack == 0 & arming > 0, 1, 0)) %>%
  mutate(choose_AC = ifelse(attack == 1 & arming > 0, 1, 0))
  
#adding label TESTE OLHAR


attr(gamedata$choose_UP, "label") <- "Choice of Unarmed Peace"
# Label variables related to Unarmed Peace and Choices (OLHAR)

#gamedata <- gamedata %>%
  #rename(
   # belief_UP = unarmed_peace,
    #check_game = choose_UP
  #)

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

#label part

attr(gamedata$comp1, "label") <- "Ich bin ein wettbewerbsfreudiger Mensch"
attr(gamedata$comp2, "label") <- "Ich bin ein wettbewerbsfreudiger Mensch 2"  # Example for comp2
attr(gamedata$likestowin, "label") <- "Ich möchte gewinnen"
attr(gamedata$compulsive, "label") <- "Ich bin impulsiv"

#Games

#Trust

gamedata <- gamedata %>%
  rename(trust = playersend) %>%
  mutate(trustworth = (playerreturn2 / 6 + playerreturn3 / 12 + playerreturn4 / 18 +
                         playerreturn5 / 24 + playerreturn6 / 30 + playerreturn7 / 36 + 
                         playerreturn8 / 42 + playerreturn9 / 48) / 8) %>%
  select(-matches("playerreturn[1-9]"))


#Loss aversion1 


gamedata <- gamedata %>%
  mutate(cointosses = playerl1 + playerl2 + playerl3 + playerl4 + playerl5 + playerl6) %>%
  mutate(no_cointosses = 6 - cointosses) %>%
  mutate(lossavers = no_cointosses / 6)

# Now create the table for lossavers where period equals 1
result_table <- table(gamedata$lossavers[gamedata$period == 1])


# Initialize playerl_t columns (equivalent to Stata `gen playerl1_t = 0` for each playerl column)
gamedata <- gamedata %>%
  mutate(across(starts_with("playerl"), ~ 0, .names = "{col}_t"))

# Loop through playerl columns and update the *_t columns based on the condition
for (i in 6:1) {
  gamedata <- gamedata %>%
    mutate(across(paste0("playerl", 1:i), ~ ifelse(get(paste0("playerl", i)) == 1, 1, .), .names = "{col}_t"))
}

# Create cointosses_t and Fehler (error) columns
gamedata <- gamedata %>%
  mutate(cointosses_t = playerl1_t + playerl2_t + playerl3_t + playerl4_t + playerl5_t + playerl6_t,
         Fehler = ifelse(cointosses != cointosses_t, 1, 0))

# Handle cases where monotonicity is violated
gamedata <- gamedata %>%
  mutate(lossavers_rel_orig = lossavers,
         lossavers = ifelse(Fehler == 1, NA, lossavers))

# Mark weird loss aversion and multiple switching loss aversion cases
gamedata <- gamedata %>%
  mutate(weird_LossAversion = ifelse(Fehler == 1 & id != 155, 1, 0),
         mulSw_LossAversion = ifelse(id == 155, 1, 0))

# Create lossaversion_ordinal and update based on conditions
gamedata <- gamedata %>%
  mutate(lossaversion_ordinal = no_cointosses,
         lossaversion_ordinal = ifelse(weird_LossAversion == 1, 98, lossaversion_ordinal),
         lossaversion_ordinal = ifelse(mulSw_LossAversion == 1, 99, lossaversion_ordinal))

# Add labels for the lossaversion_ordinal variable
gamedata <- gamedata %>%
  mutate(lossaversion_ordinal = factor(lossaversion_ordinal, 
                                       levels = c(98, 99),
                                       labels = c("Weird (reverse) LA", "Multiple Switching LA Game")))

# Drop unnecessary columns
gamedata <- gamedata %>%
  select(-c(cointosses, cointosses_t, playerl1:playerl6, playerl1_t:playerl6_t, Fehler))



# Rename questionnaire variables
gamedata <- gamedata %>%
  rename(
    Rudi_Links1 = playerr1,
    Rudi_Links2 = playerr2,
    Rudi_Links3 = playerr3,
    Rudi_Links4 = playerr4,
    Rudi_Links5 = playerr5,
    Rudi_Links6 = playerr6,
    Rudi_Links7 = playerr7,
    Rudi_Links8 = playerr8,
    Rudi_Links9 = playerr9,
    Rudi_Links10 = playerr10
  )


# Initialize Rudi_Links_t variables to 0(ERRO no comando)

gamedata <- gamedata %>%
mutate(across(starts_with("Rudi_Links"), ~ 0, .names = "{.col}_t"))

# Update Rudi_Links_t for i = 1 to 5
for (i in 1:5) {
  gamedata <- gamedata %>%
    mutate(across(paste0("Rudi_Links", i:5, "_t"), 
                  ~ ifelse(get(paste0("Rudi_Links", i)) == 1, 1, .)))
}

# Update Rudi_Links_t for i = 6 to 10
for (i in 6:10) {
  gamedata <- gamedata %>%
    mutate(across(paste0("Rudi_Links", i:10, "_t"), 
                  ~ ifelse(get(paste0("Rudi_Links", i)) == 1, 1, .)))
}

# Create sum variables
gamedata <- gamedata %>%
  mutate(
    sum1 = Rudi_Links1 + Rudi_Links2 + Rudi_Links3 + Rudi_Links4 + Rudi_Links5,
    sum2 = Rudi_Links6 + Rudi_Links7 + Rudi_Links8 + Rudi_Links9 + Rudi_Links10,
    sum1t = Rudi_Links1_t + Rudi_Links2_t + Rudi_Links3_t + Rudi_Links4_t + Rudi_Links5_t,
    sum2t = Rudi_Links6_t + Rudi_Links7_t + Rudi_Links8_t + Rudi_Links9_t + Rudi_Links10_t,
    fehler1 = ifelse(sum1 != sum1t, 1, NA),
    fehler2 = ifelse(sum2 != sum2t, 1, NA),
    X_switch = ifelse(fehler1 != 1 & fehler2 != 1, 6 - sum1, NA),
    Y_switch = ifelse(fehler1 != 1 & fehler2 != 1, 6 - sum2, NA)
  )

# Recode X_switch and Y_switch
gamedata <- gamedata %>%
  mutate(
    X_switch = case_when(
      X_switch == 2 ~ 5,
      X_switch == 3 ~ 6,
      X_switch == 4 ~ 7,
      X_switch == 5 ~ 11,
      TRUE ~ X_switch
    ),
    Y_switch = case_when(
      Y_switch == 2 ~ 5,
      Y_switch == 3 ~ 6,
      Y_switch == 4 ~ 7,
      Y_switch == 5 ~ 11,
      TRUE ~ Y_switch
    )
  )

# Create X_score and Y_score, and handle special cases for sum1 == 0 and sum2 == 0
gamedata <- gamedata %>%
  mutate(
    X_score = 6.5 - X_switch,
    Y_score = Y_switch - 6.5,
    X_score = ifelse(sum1 == 0, -5.5, X_score),
    Y_score = ifelse(sum2 == 0, 5.5, Y_score)
  )

# Drop unnecessary variables
gamedata <- gamedata %>%
  select(-starts_with("Rudi_Links"), -fehler1, -fehler2, -starts_with("sum"), -X_switch, -Y_switch)


# EET Type 

gamedata <- gamedata %>%
  # Initialize EET_type to 0
  mutate(EET_type = 0) %>%
  
  # Apply the conditions to define EET_type
  mutate(
    EET_type = case_when(
      X_score > 0 & Y_score > 0 ~ 1,  # Altruist
      (X_score == 4.5 | X_score == 5.5) & (Y_score == 0.5 | Y_score == -0.5) ~ 2,  # Kiss-Up
      X_score > 0 & Y_score < 0 ~ 3,  # EqualityAvers
      (X_score == 0.5 | X_score == -0.5) & (Y_score == -4.5 | Y_score == -5.5) ~ 4,  # Kick-Down
      X_score < 0 & Y_score < 0 ~ 5,  # Spiteful
      (X_score == -4.5 | X_score == -5.5) & (Y_score == 0.5 | Y_score == -0.5) ~ 6,  # Envious
      X_score < 0 & Y_score > 0 ~ 7,  # InequalityAvers
      (X_score == 0.5 | X_score == -0.5) & (Y_score == 4.5 | Y_score == 5.5) ~ 8,  # MaxiMin
      
      # Selfish cases
      X_score > 0 & X_score <= 1.5 & Y_score > 0 & Y_score <= 1.5 ~ 9,
      X_score > 0 & X_score <= 1.5 & Y_score < 0 & Y_score >= -1.5 ~ 9,
      X_score < 0 & X_score >= -1.5 & Y_score > 0 & Y_score <= 1.5 ~ 9,
      X_score < 0 & X_score >= -1.5 & Y_score < 0 & Y_score >= -1.5 ~ 9,
      
      # Undefined
      is.na(X_score) ~ 0,
      
      TRUE ~ EET_type  # Keep EET_type as is if no conditions are met
    )
  )

# Optionally, label the EET_type variable as a factor with descriptive labels

gamedata <- gamedata %>%
  mutate(
    EET_type = factor(EET_type, levels = 0:9, 
                      labels = c("Undefined", "Altruist", "Kiss-Up", "EqualityAvers", 
                                 "Kick-Down", "Spiteful", "Envious", "InequalityAvers", 
                                 "Maximin", "Selfish"))
  )

# Filter by period == 1 to view a specific subset of the data
result <- gamedata %>%
  filter(period == 1) %>%
  count(EET_type)


#Alternative: Relevant types

gamedata <- gamedata %>%
  # Initialize EET_type_new to 0
  mutate(EET_type_new = 0) %>%
  
  # Update EET_type_new based on the conditions
  mutate(
    EET_type_new = case_when(
      EET_type == "Altruist" ~ 1,  # Altruist
      EET_type == "Selfish"  ~ 2,  # Selfish
      EET_type == "Maximin"  ~ 3,  # Maximin
      TRUE ~ EET_type_new  # Keep as 0 for all others
    )
  )


gamedata <- gamedata %>%
  mutate(
    EET_type_new = factor(EET_type_new, levels = 0:3, 
                          labels = c("Other", "Altruist", "Selfish", "Maximin"))
  )


# STRATEGIES OF PLAYERS

# Renaming the variables
gamedata <- gamedata %>%
  rename(
    attack_profit = playerattack_profit,
    attack_spite = playerattack_spite,
    attack_super = playerattack_super,
    attack_fight1 = playerattack_fight1,
    attack_fight2 = playerattack_fight2,
    attack_encourage = playerattack_encourage,
    attack_chance = playerattack_chance,
    attack_mistake = playerattack_mistake,
    attack_understand = playerattack_understand,
    noattack_risk = playernoattack_risk,
    noattack_efficient = playernoattack_efficient,
    noattack_ownprofit = playernoattack_ownprofit,
    noattack_otherprofit = playernoattack_otherprofit,
    noattack_discourage = playernoattack_discourage,
    noattack_mistake = playernoattack_mistake,
    noattack_understand = playernoattack_understand,
    arming_attack = playerarming_attack,
    arming_security = playerarming_security,
    arming_noattack = playerarming_noattack,
    arming_inefficient = playerarming_inefficient,
    arming_unequala1 = playerarming_unequala1,
    arming_unequala2 = playerarming_unequala2,
    arming_unequalb1 = playerarming_unequalb1,
    arming_unequalb2 = playerarming_unequalb2,
    arming_understand = playerarming_understand
  )

# Recoding variables and applying labels
recoded_vars <- c(
  "attack_profit", "attack_spite", "attack_super", "attack_fight1", "attack_fight2", "attack_encourage",
  "attack_chance", "attack_mistake", "attack_understand", "noattack_risk", "noattack_efficient",
  "noattack_ownprofit", "noattack_otherprofit", "noattack_discourage", "noattack_mistake", 
  "noattack_understand", "arming_attack", "arming_security", "arming_noattack", "arming_inefficient",
  "arming_unequala1", "arming_unequala2", "arming_unequalb1", "arming_unequalb2", "arming_understand"
)

gamedata <- gamedata %>%
  mutate(across(all_of(recoded_vars), ~ case_when(
    . == 0 ~ 5,  # Fully Agree
    . == 1 ~ 4,  # Rather Agree
    . == 4 ~ 3,  # Neither
    . == 3 ~ 1   # Fully Disagree
  )))


gamedata <- gamedata %>%
  mutate(across(all_of(recoded_vars), ~ factor(., levels = c(5, 4, 3, 2, 1), 
                                               labels = c("Fully Agree", "Rather Agree", "Neither", 
                                                          "Rather Disagree", "Fully Disagree"))))


#SPIEL verstanden

#levels(gamedata$attack_understand)
#levels(gamedata$noattack_understand)
#levels(gamedata$arming_understand)

#checkdeck


gamedata <- gamedata %>%
  mutate(
    attack_understand_num = recode(attack_understand, "Fully Agree" = 5,"Rather Agree" = 4, "Neither" = 3, "Rather Disagree" = 2, "Fully Disagree" = 1 ),
    noattack_understand_num = recode(noattack_understand, "Fully Agree" = 5,"Rather Agree" = 4, "Neither" = 3, "Rather Disagree" = 2, "Fully Disagree" = 1 ),
    arming_understand_num = recode(arming_understand, "Fully Agree" = 5,"Rather Agree" = 4, "Neither" = 3, "Rather Disagree" = 2, "Fully Disagree" = 1 )
  )%>%
  mutate(check_dec = case_when( 
    attack_understand_num < 4 & 
      noattack_understand_num < 4 & 
      arming_understand_num < 4 ~ 1,
    TRUE ~ 0
  ))

#checkgame

gamedata <- gamedata %>%
  mutate(checkgame = 0) %>%
  mutate(checkgame = case_when(
    playerdem_understand==3 ~ 1
    
  ))

#BELIEFS

gamedata<-gamedata %>%
  mutate(belief_UP = ifelse(belief_attack == 0 & belief_arming == 0, 1, 0),
         belief_UP = ifelse(period != 1, NA, belief_UP)) %>%

  group_by(id) %>%
  mutate(reg_bel_arm = mean(belief_arming, na.rm = TRUE),
         reg_bel_att = mean(belief_attack, na.rm = TRUE)) %>%
  ungroup()

attr(gamedata$belief_UP, "label") <- "Belief other Player plays UP"

#CORR_BEL

gamedata <- gamedata %>%

  mutate(corr_bel = ifelse(belief_attack == opponent_attack, 1, 0),
         corr_bel = ifelse(period > 1, NA, corr_bel)) 

gamedata <- gamedata %>%
group_by(id) %>%
  mutate(correct_beliefs = mean(corr_bel, na.rm = TRUE)) %>%
  ungroup()

gamedata <- gamedata %>%
select(-corr_bel)

#EET TYPES

gamedata <- gamedata %>%
  mutate(Altruist = ifelse(EET_type_new == 1, 1, 0),
         Selfish = ifelse(EET_type_new == 2, 1, 0),
         Maximin = ifelse(EET_type_new == 3, 1, 0))


#OUTCOME FOR DEVELOPMENT

gamedata <- gamedata %>%
  mutate(armed_peace = ifelse(conflict == 0 & (arming != 0 | opponent_arming != 0), 1, 0))

gamedata <- gamedata %>%
  mutate(armed_conflict = ifelse(conflict == 1 & (arming != 0 | opponent_arming != 0), 1, 0))





#Ordering

gamedata <- gamedata %>%
  select(treatment, session, id, period, endowment, belief_attack, belief_arming,
         production, arming, attack, opponent_production, opponent_arming, opponent_attack,
         conflict, rel_arming, arm_def, arm_att, unarmed_peace, choose_UP, everything())

#Save the modified dataset

#Drop

gamedata <- gamedata %>%
  select(
    -playerpayoff4:-playertype,
    -playerpayoff1:-playerpayoff3,
    -playerprize:-playerprob_percent,
    -playeractive:-playergamble,
    -playerback:-playerpayround2,
    -playerearnings:-sessioncode,
    -playerfinal_payoff, 
    -playerfinal_payoff_help,
    -participantid_in_session:-playertotal_arming
  )

#Save the modified dataset

write.csv(gamedata, file = file.path(out, "modified_data2.csv"), row.names = FALSE)

tabela_andre <- read.csv("D:/Users/B435076/Desktop/R-S/Data and analysis/out/modified_data2.csv")
tabela_origin <- read_dta("D:/Users/B435076/Desktop/R-S/Data and analysis/conflict_replication1.dta")

