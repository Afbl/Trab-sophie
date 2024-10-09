clear all

	gl FolderDirect "D:\Users\B435087\Desktop\osfstorage-archive\Data and analysis"				// Path of the folder "Replication_Conflict"
	gl out 			"D:\Users\B435087\Desktop\osfstorage-archive\Data and analysis\Output"		// Path of output folder

cd "$FolderDirect
use DATA_YGAME-D-24-00018R1.dta

**** Treatment definieren
rename  equal  treatment
label define treatment 0 "Unequal"  1 "Equal" 
label values treatment treatment
drop unequal 

*Generate numeric ID
sort  treatment participantcode
gen test = _n
egen id 	 =  min(test)  , by(participantcode)
egen session =  min(test)  , by(sessioncode)
drop test

rename 	subsessionround_number 	period 
rename 	playerendowment 		endowment 
rename 	playerattack 			attack
rename 	playerproduction 		production
rename 	playerarming 			arming
rename 	playerother_production 	opponent_production
rename 	playerother_arming 		opponent_arming
rename 	playerother_attack 		opponent_attack
rename  playerfinal_payoff_rmb 	auszahlung
rename 	playeris_winner 		win_conflict

rename  playerattack_est 		belief_attack
rename  playerarming_est 		belief_arming



* Matching-Groups
gen		indep_obs=session 
replace indep_obs=session+1 if participantid_in_session>8 	& treatment==1
replace indep_obs=session+2 if participantid_in_session>16	& treatment==1
*tab  indep_obs treatment

*Was there a Conflict
gen 	conflict=0
replace	conflict=1 if attack==1 | opponent_attack==1
replace win_conflict=. if conflict==0 	

label variable conflict 		"Conflict"



*Relative Arming 
gen rel_arming=arming/endowment 
label variable rel_arming 		"Relative Arming Level"
*Devensive vs. Offensive Arming
gen arm_def = arming 	if attack==0 
gen arm_att = arming 	if attack==1 
label variable arm_def	 		"Defensive Arming"
label variable arm_att	 		"Offensive Arming"

save partial, replace

********************************************************************************
*****							Unarmed Peace?							   *****
********************************************************************************
*Outcome
gen		unarmed_peace=0
replace unarmed_peace=1 	if conflict==0 & production==endowment & (200-endowment)==opponent_production

*Individual Behavior
gen 	Strategies3=1		if attack==0 & production==endowment 
replace Strategies3=2		if attack==0 & arming>0
replace Strategies3=3		if attack==1 & production==endowment 
replace Strategies3=4		if attack==1 & arming>0

label define Strategies3 	1 "Unarmed Peace" 2 "Armed Peace" 3 "Unarmed Conflict" 4 "Armed Conflict"
label values Strategies3 Strategies3


gen 	choose_UP=0
replace choose_UP=1			if attack==0 & production==endowment 
gen 	choose_UC=0
replace choose_UC=1			if attack==1 & production==endowment 
gen 	choose_AP=0
replace choose_AP=1			if attack==0 & arming>0 
gen 	choose_AC=0
replace choose_AC=1			if attack==1 & arming>0 


label variable unarmed_peace	"Outcome is Unarmed Peace"
label variable choose_UP		"Choice of Unarmed Peace"

********************************************************************************
***									Questionaire							 ***
********************************************************************************

gen 	riskaverse=(11-playerqt1_risk)/11
drop 	playerqt1_risk


rename playerdem_age 		age
rename playerdem_gender 	male			
recode						male 2/3=0	


gen    		econstudent = 0 
replace    	econstudent = 1 if playerdem_study==12

rename playerdem_grad		bachelor
recode						bachelor 2/3=0	///STIMMT DAS????

rename playerqt2_compete1 	comp1
rename playerqt3_compete2 	comp2
rename playerqt4_win 		likestowin
rename playerqt5_impulsive	compulsive
label variable comp1 "Ich bin ein wettbewerbsfreudiger Mensch"
label variable comp2 "Wettbewerb holt das Beste aus mir heraus."
label variable likestowin	"Ich sehe mich selbst als jemanden, der/die gerne gewinnt (und es hasst zu verlieren)."
label variable compulsive	"Ich bin impulsiv"

********************************************************************************
***									GAMES									 ***
********************************************************************************

********************************************************************************
***									TRUST									 ***
********************************************************************************
*Trust
rename 	playersend trust //Trust = Amount sent
*Trustworthiness (0 - 1 ... falls alles zurück gesendet wird 1) 
gen 	trustworth = (playerreturn2/6 + playerreturn3/12 + playerreturn4/18 + ///
		playerreturn5/24 + playerreturn6/30 + playerreturn7/36 + ///
		playerreturn8/42 + playerreturn9/48)/8 

drop 	playerreturn1-playerreturn9



********************************************************************************
*** 							Lossaversion								 ***
********************************************************************************
*Loss Aversion: 1 falls keine Münze geworfen, 0 falls alle Münzen geworfen
gen cointosses=playerl1+playerl2+playerl3+playerl4+playerl5+playerl6
gen no_cointosses=6-cointosses
gen lossavers=no_cointosses/6	
tab lossavers if period==1


*Check Monotonicity - 4 Spieler habens nicht gechekt!
foreach var of varlist playerl1-playerl6 {
	gen `var'_t = 0
}

forvalues i = 6(-1)1 {
foreach var of varlist playerl1-playerl`i' {
	replace `var'_t = 1  if playerl`i'==1
}
}

gen cointosses_t=playerl1_t+playerl2_t+playerl3_t+playerl4_t+playerl5_t+playerl6_t
gen Fehler=1 if cointosses!=cointosses_t

gen 	lossavers_rel_orig = lossavers
replace lossavers=. if Fehler==1 // Falls Monotonicity verletzt ist - lossavers raus

gen 	weird_LossAversion = 0 
replace weird_LossAversion = 1 	if Fehler==1 & id!=155
gen 	mulSw_LossAversion = 0 
replace mulSw_LossAversion = 1  if id==155

gen 	lossaversion_ordinal = no_cointosses 
replace lossaversion_ordinal = 98 if weird_LossAversion==1
replace lossaversion_ordinal = 99 if mulSw_LossAversion==1
label 	variable lossaversion_ordinal "No Coin Tosses in LA Game"
label  	define lossaversion_ordinal 98 "Weird (reverse) LA" 99 "Multiple Switching LA Game"
label 	value lossaversion_ordinal lossaversion_ordinal

drop cointosses	cointosses_t 	playerl1-playerl6 playerl1_t-playerl6_t Fehler

********************************************************************************
*** 					Equivalence Equity Task								 ***
********************************************************************************
*Check Monotonicity
rename playerr1  Rudi_Links1
rename playerr2  Rudi_Links2
rename playerr3  Rudi_Links3
rename playerr4  Rudi_Links4
rename playerr5  Rudi_Links5
rename playerr6  Rudi_Links6
rename playerr7  Rudi_Links7
rename playerr8  Rudi_Links8
rename playerr9  Rudi_Links9
rename playerr10 Rudi_Links10

foreach var of varlist Rudi_Links1-Rudi_Links10 {
	gen `var'_t = 0
}

forvalues i = 1(1)5 {
foreach var of varlist Rudi_Links`i'-Rudi_Links5 {
	replace `var'_t = 1  if Rudi_Links`i'==1
}
}

forvalues i = 6(1)10 {
foreach var of varlist Rudi_Links`i'-Rudi_Links10 {
	replace `var'_t = 1  if Rudi_Links`i'==1
}
}

gen sum1 = Rudi_Links1+Rudi_Links2+Rudi_Links3+Rudi_Links4+Rudi_Links5
gen sum2 = Rudi_Links6+Rudi_Links7+Rudi_Links8+Rudi_Links9+Rudi_Links10
gen sum1t = Rudi_Links1_t+Rudi_Links2_t+Rudi_Links3_t+Rudi_Links4_t+Rudi_Links5_t
gen sum2t = Rudi_Links6_t+Rudi_Links7_t+Rudi_Links8_t+Rudi_Links9_t+Rudi_Links10_t

gen fehler1 = 1 if sum1!=sum1t
gen fehler2 = 1 if sum2!=sum2t

*Scores for monotonic responses - Obwhol wir nicht 11 Reihen pro Liste testen sind die Zahlen derart gewählt
gen 	X_switch=6-sum1 if (fehler1!=1 & fehler2!=1)
gen 	Y_switch=6-sum2 if (fehler1!=1 & fehler2!=1)

foreach var of varlist X_switch Y_switch {
		recode `var' (2=5) (3=6) (4=7) (5=11)
}

gen 	X_score=6.5-X_switch
gen 	Y_score=Y_switch-6.5

replace X_score=(-5.5) 	if sum1==0
replace Y_score=  5.5  	if sum2==0

drop Rudi_Links1-Rudi_Links10 Rudi_Links1_t-Rudi_Links10_t X_switch Y_switch fehler1 fehler2 sum1-sum2t

*Define Types
gen 	EET_type=0 
replace	EET_type=1 		if X_score>0 & Y_score>0											// Altruist
replace	EET_type=2 		if (X_score==4.5 | X_score==5.5) & (Y_score==0.5 | Y_score==-0.5)	// KissUp
replace	EET_type=3 		if X_score>0 & Y_score<0											// EqualityAvers
replace	EET_type=4 		if (X_score==0.5 | X_score==-0.5) & (Y_score==-4.5 | Y_score==-5.5)	// Kick-Down
replace	EET_type=5 		if X_score<0 & Y_score<0											// Spiteful
replace	EET_type=6 		if (X_score==-4.5 | X_score==-5.5) & (Y_score==0.5 | Y_score==-0.5)	// Envious
replace	EET_type=7 		if X_score<0 & Y_score>0											// InequalityAvers
replace	EET_type=8 		if (X_score==0.5 | X_score==-0.5) & (Y_score==4.5 | Y_score==5.5)	// MaxiMin
replace	EET_type=9 		if X_score>0 & X_score<=1.5 & Y_score>0 & Y_score<=1.5				// Selfish
replace	EET_type=9 		if X_score>0 & X_score<=1.5 & Y_score<0 & Y_score>=-1.5				// Selfish
replace	EET_type=9 		if X_score<0 & X_score>=-1.5 & Y_score>0 & Y_score<=1.5				// Selfish
replace	EET_type=9 		if X_score<0 & X_score>=-1.5 & Y_score<0 & Y_score>=-1.5			// Selfish
replace EET_type=0 		if missing(X_score)													// Undefined

label define EET_type 	0 "Undefined" 1 "Altruist" 2 "Kiss-Up" 3 "EqualityAvers" 4 "Kick-Down" 5 "Spiteful" 6 "Envious" 7 "InequalityAvers" 8 "Maximin" 9 "Selfish"
label values EET_type EET_type
tab 	EET_type if period==1


*Alternative: Relevant Types
gen 		EET_type_new = 0 
replace		EET_type_new = 1 if EET_type==1 // Altruist
replace		EET_type_new = 2 if EET_type==9 // Selfish
replace		EET_type_new = 3 if EET_type==8 // MaxiMin

label define EET_type_new 	0 "Other" 1 "Altruist" 2 "Selfish"  3 "Maximin"
label values EET_type_new EET_type_new


********************************************************************************
*** 					Strategies of Players								 ***
********************************************************************************
{
label variable playerattack_profit 		"Attack… meine Auszahlung erhöhen wollte."
label variable playerattack_spite 		"Attack… die Produktion der anderen Person gewinnen wollte."
label variable playerattack_super 		"Attack… dachte der andere Spieler investiert nichts in Wettkampf."
label variable playerattack_fight1 		"Attack… Lust auf den Wettkampf hatte."
label variable playerattack_fight2 		"Attack… Spiel ohne Wettkampf langweilig ist."
label variable playerattack_encourage 	"Attack… der anderen Person überlegen war"
label variable playerattack_chance 		"Attack… bessere Chancen hatte den Wettkampf zu gewinnen."
label variable playerattack_mistake 	"Attack… Fehler gemacht. (Ich wollte keinen Wettkampf starten.)"
label variable playerattack_understand 	"Attack… Entscheidung nicht verstanden habe."


label variable playernoattack_risk 			"noAttack… ich das Risiko nicht eingehen wollte, meine Produktion zu verlieren."
label variable playernoattack_efficient 	"noAttack… ein Wettkampf einen Teil der Produktion zerstört"
label variable playernoattack_ownprofit 	"noAttack… es für mich besser ist, wenn kein Wettkampf stattfindet." 
label variable playernoattack_otherprofit 	"noAttack… es für die andere Person besser ist, wenn kein Wettkampf stattfindet."  
label variable playernoattack_discourage 	"noAttack… ich der anderen Person unterlegen war" 
label variable playernoattack_mistake 		"noAttack… ich einen Fehler gemacht. (Ich wollte keinen Wettkampf starten.)"
label variable playernoattack_understand	"noAttack… ich die Entscheidung nicht verstanden habe."

label variable playerarming_attack 			"Arming, weil ich einen Wettkampf starten, und diesen gewinnen wollte." 
label variable playerarming_security 		"Arming, um mich abzusichern, falls die andere Person einen Wettkampf startet."

label variable playerarming_noattack 		"Low Arming, da ich mir sicher war, dass kein Wettkampf gestartet wird." 
label variable playerarming_inefficient 	"Low Arming, weil ineffizient ist (da sie die Produktion verringert)."
label variable playerarming_unequala1 		"High Arming, da ich ein höheres Budget als die andere Person hatte."
label variable playerarming_unequala2 		"High Arming, da ich ein niedrigeres Budget als die andere Person hatte."
label variable playerarming_unequalb1 		"Low Arming, da ich ein höheres Budget als die andere Person hatte."
label variable playerarming_unequalb2 		"Low Arming, da ich ein niedrigeres Budget als die andere Person hatte."
label variable playerarming_understand		"Arming… Entscheidung nicht verstanden"

rename playerattack_profit 		  	attack_profit 		
rename playerattack_spite 		  	attack_spite 		
rename playerattack_super 		  	attack_super 		
rename playerattack_fight1 		  	attack_fight1 		
rename playerattack_fight2 		  	attack_fight2 		
rename playerattack_encourage 	  	attack_encourage 	
rename playerattack_chance 		  	attack_chance 		
rename playerattack_mistake 	  	attack_mistake 	
rename playerattack_understand    	attack_understand 
rename playernoattack_risk 		 	noattack_risk 			
rename playernoattack_efficient 	noattack_efficient 	
rename playernoattack_ownprofit 	noattack_ownprofit 	
rename playernoattack_otherprofit 	noattack_otherprofit 	
rename playernoattack_discourage 	noattack_discourage 	
rename playernoattack_mistake 		noattack_mistake 		
rename playernoattack_understand	noattack_understand	
rename playerarming_attack 			arming_attack
rename playerarming_security 		arming_security
rename playerarming_noattack 		arming_noattack 	
rename playerarming_inefficient     arming_inefficient 
rename playerarming_unequala1 	    arming_unequala1 	
rename playerarming_unequala2 	    arming_unequala2 	
rename playerarming_unequalb1 	    arming_unequalb1 	
rename playerarming_unequalb2 	    arming_unequalb2 	
rename playerarming_understand	    arming_understand	
}


label define Zustimmung 	5 "Fully Agree" 4 "Rather Agree " 3 "Neither" 2 "Rather Disagree" 1 "Fully Disagree" 
foreach	var of varlist attack_profit-arming_understand {
	recode `var' (0=5) (1=4) (4=3) (3=1)
	label values `var' Zustimmung
}


********************************************************************************
*** 							SPIEL verstanden							 ***
********************************************************************************
gen 		check_dec = 0 
replace 	check_dec = 1 	if  attack_understand < 4 & noattack_understand < 4 &  arming_understand <4

gen 		check_game=0 
replace		check_game=1 	if playerdem_understand==3



********************************************************************************
*** 								BELIEFS 								 ***
********************************************************************************
gen 	belief_UP = 0 
replace belief_UP = 1 if belief_attack==0 & belief_arming==0 
replace belief_UP = . if period!=1
label 	variable belief_UP 			"Belief other Player plays UP"


egen reg_bel_arm = mean(belief_arming) ,by(id)
egen reg_bel_att = mean(belief_attack) ,by(id)

gen  		corr_bel=0
replace		corr_bel=1 if belief_attack==opponent_attack
replace		corr_bel=. if period>1

egen correct_beliefs = mean(corr_bel) ,by(id)
drop corr_bel

* Generate Binary EET_Types - one variable for each type
tab 	EET_type_new  ,gen(EET_binary)
drop 	EET_binary1
rename 	EET_binary2 Altruist
rename 	EET_binary3 Selfish
rename 	EET_binary4 Maximin


* Outcomes for Develpment: Figure 3
gen 	armed_peace = 0 
replace armed_peace = 1 	if conflict==0 & (arming!=0 | opponent_arming!=0)
gen 	armed_conflict = 0 
replace armed_conflict = 1 	if conflict==1 & (arming!=0 | opponent_arming!=0)



order 	treatment session id period endowment belief_attack belief_arming ///
		production arming attack opponent_production opponent_arming opponent_attack ///
		conflict rel_arming arm_def arm_att unarmed_peace choose_UP 

	
drop participantcode	
drop playerpayoff4-playertyp		
drop playerpayoff1-playerpayoff3
drop playerprize-playerprob_percent
drop playeractive-playergamble
drop playerback-playerpayround2
drop playerearnings-sessioncode
drop playerfinal_payoff playerfinal_payoff_help
drop participantid_in_session-playertotal_arming


*save
save conflict_replication1, replace










