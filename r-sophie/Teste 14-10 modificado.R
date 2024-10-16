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

FolderDirect <- "D:/Users/B435076/Documents/GitHub/Trab-sophie/r-sophie"
out <- ("D:/Users/B435076/Documents/GitHub/Trab-sophie/r-sophie/out")
setwd(FolderDirect)

# Load the dataset
gamedata <- read_dta("D:/Users/B435076/Documents/GitHub/Trab-sophie/r-sophie/data_partial2.dta")
head(gamedata)
colnames(gamedata)

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
    fehler1 = ifelse(sum1 != sum1t, 1, 0),
    fehler2 = ifelse(sum2 != sum2t, 1, 0),
    X_switch = ifelse(fehler1 != 1 & fehler2 != 1, 6 - sum1, 0),
    Y_switch = ifelse(fehler1 != 1 & fehler2 != 1, 6 - sum2, 0)
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


#drop unnecessary lines

gamedata <- gamedata %>%
  select(-matches("^Rudi_Links[1-9]$"),   # Matches Rudi_Links1 to Rudi_Links9
         -matches("^Rudi_Links10$"),      # Matches Rudi_Links10
         -matches("^Rudi_Links[1-9]_t$"), # Matches Rudi_Links1_t to Rudi_Links9_t
         -matches("^Rudi_Links10_t$"),    # Matches Rudi_Links10_t
         -c("X_switch", "Y_switch", "fehler1", "fehler2", "sum1","sum2", "sum1t", "sum2t"))


# EET Type 

gamedata <- gamedata %>%
  # Initialize EET_type to 0
  mutate(EET_type = 0) %>%
  
  # Apply the conditions to define EET_type
  mutate(
    EET_type = case_when(
     
      (X_score == 4.5 | X_score == 5.5) & (Y_score == 0.5 | Y_score == -0.5) ~ 2,  # Kiss-Up
      (X_score == 0.5 | X_score == -0.5) & (Y_score == -4.5 | Y_score == -5.5) ~ 4,  # Kick-Down
      X_score < 0 & Y_score < 0 ~ 5,  # Spiteful
      (X_score == -4.5 | X_score == -5.5) & (Y_score == 0.5 | Y_score == -0.5) ~ 6,  # Envious
      (X_score == 0.5 | X_score == -0.5) & (Y_score == 4.5 | Y_score == 5.5) ~ 8,  # MaxiMin
      
      # Selfish cases
      X_score > 0 & X_score <= 1.5 & Y_score > 0 & Y_score <= 1.5 ~ 9,
      X_score > 0 & X_score <= 1.5 & Y_score < 0 & Y_score >= -1.5 ~ 9,
      X_score < 0 & X_score >= -1.5 & Y_score > 0 & Y_score <= 1.5 ~ 9,
      X_score < 0 & X_score >= -1.5 & Y_score < 0 & Y_score >= -1.5 ~ 9,
      
      X_score > 0 & Y_score > 0  ~ 1,  # Altruist
      X_score > 0 & Y_score < 0 ~ 3,  # EqualityAvers
      X_score < 0 & Y_score < 0 ~ 5,  # Spiteful
      X_score < 0 & Y_score > 0 ~ 7,  # InequalityAvers
    
      # Undefined
      is.na(X_score) ~ 0,
      
      TRUE ~ EET_type  # Keep EET_type as is if no conditions are met
    )
  )




# Optionally, label the EET_type variable as a factor with descriptive labels



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
      EET_type == 1 ~ 1,  # Altruist
      EET_type == 9  ~ 2,  # Selfish
      EET_type == 8  ~ 3,  # Maximin
      TRUE ~ EET_type_new  # Keep as 0 for all others
    )
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
    . == 3 ~ 1,   # Fully Disagree
    . == 2 ~ 2   # Adicionado pra complementar
  )))


#checkdeck


gamedata <- gamedata %>%
  mutate(check_dec = case_when( 
    attack_understand < 4 & 
      noattack_understand < 4 & 
      arming_understand < 4 ~ 1,
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



gamedata <- gamedata %>%
  select(-participantcode)
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


write.csv(gamedata, file = file.path(out, "partial_data3.csv"), row.names = FALSE)
tabela_andre1 <- read.csv("D:/Users/B435076/Documents/GitHub/Trab-sophie/r-sophie/out/partial_data3.csv")

