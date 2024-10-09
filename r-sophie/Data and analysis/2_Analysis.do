clear all

	gl FolderDirect "D:\Users\B435087\Desktop\osfstorage-archive\Data and analysis"			// Path of the folder "Replication_Conflict"
	gl out 			"D:\Users\B435087\Desktop\osfstorage-archive\Data and analysis\Output"		// Path of output folder

cd "$FolderDirect


ssc install xlincom 

*Run data preparation do-file: 
	do "1_Data_Import_and_Prep.do"

use conflict_replication1.dta


********************************************************************************
**************      		Create Folder: Output						********
*******************************************************************************
shell 	rmdir "$FolderDirect\Output" /s
		mkdir "$FolderDirect\Output" 





********************************************************************************
*****							Variable Lists							   *****
********************************************************************************
gl covariates 	"age male econstudent bachelor comp1 comp2 likestowin compulsive trust trustworth lossavers no_cointosses riskaverse auszahlung Altruist Selfish Maximin"
gl reg_cov 		 "male age comp1 comp2 likestowin compulsive trust trustworth riskaverse ib0.lossaversion_ordinal Altruist Selfish Maximin"

gl Outcomes_beliefs  "belief_arming belief_attack belief_UP"

gl Out_beliefs_bin  	"belief_attack belief_UP"
gl Out_beliefs_cont  	"belief_arming"

gl list_associations "attack arming choose_UP"
gl reg_out_bin   "attack choose_UP choose_AP choose_AC "
gl reg_out_cont  "arming	 arm_att arm_def rel_arming"

gl reg_out_bin_f   "attack choose_UP"
gl reg_out_cont_f  "arming"

********************************************************************************
*****							Non-Parametric Tests					   *****
********************************************************************************
preserve
collapse  (mean)   treatment attack arming choose_UP conflict unarmed_peace ,by(indep_obs)
ranksum  attack 		,by(treatment)	// p=0.0571
ranksum  arming 		,by(treatment)  // p=0.0381
ranksum  choose_UP 		,by(treatment) 	// p=0.4571
ranksum  conflict 		,by(treatment) // p=0.0550
ranksum  unarmed_peace	,by(treatment) // p=0.333
restore


* Protective vs. Aggressive Arming
preserve
keep if endowment!=100
collapse  (mean)   treatment arming   	,by(indep_obs attack)
ranksum  arming 						,by(attack) 
restore

// UNEQUAL: p=0.0209
// EQUAL: 	p=0.0039

// UNEQUAL 80: 	p=0.0209
// UNEQUAL 120: p=0.0209




********************************************************************************
*****								Balancing Table						   *****
********************************************************************************
	
eststo clear
estpost sum $covariates if period==1 & treatment==0
esttab  using "$out\Balencingtable.csv" ,replace ///
		cells("mean(fmt(2)) sd(fmt(2)par) min(fmt(0)) max(fmt(0)) count(fmt(0))") noobs   ///
		title("Balencing Table") 
	
	
eststo clear
foreach cov of global covariates {
	eststo b_`cov': 	qui reg `cov' ib0.treatment if period==1, vce(robust)
	qui margins, over(treatment) atmeans post
	estadd scalar Mean_UNEQUL   = r(table)[1,1]		: b_`cov'
	
	qui lincom _b[1.treatment] - _b[0.treatment]
	estadd scalar Coef_EUQAL = r(estimate)			: b_`cov'
	estadd scalar half_CI_Range = (r(ub)-r(lb))/2	: b_`cov'
}


esttab 	using "$out\Balencingtable.csv" ,   append ///
		nobaselevels cells(b(fmt(3)star) se(fmt(3) par)) ///
		keep(*.treatment) star(* .05 ** .01 *** .001) ///
		title("Balencing Table") 
	



********************************************************************************
*****						Average Treatment Effects					   *****
********************************************************************************

eststo clear
*Binary Indicators 
foreach output of global reg_out_bin {
eststo     r1_`output': qui meprobit `output' ib0.treatment period 			|| indep_obs: || id: ,vce(robust)
eststo     r_`output':  qui margins ,dydx(treatment) post
		   local   coef_`output'_r = r(table)[1,2]	
		
eststo     a1_`output': qui meprobit `output' ib0.treatment period $reg_cov || indep_obs: || id: ,vce(robust)
eststo     a_`output':  qui margins ,dydx(treatment) atmeans post
		   local   coef_`output'_a = r(table)[1,2]

qui meprobit `output' ib0.treatment period								|| indep_obs: || id: ,vce(robust)
eststo     rr_`output': qui margins ,over(treatment) post
qui meprobit `output' ib0.treatment period $reg_cov 					|| indep_obs: || id: ,vce(robust)
eststo     aa_`output': qui margins ,over(treatment) atmeans post

}

*Continous Outcomes
foreach output of global reg_out_cont {
eststo  r1_`output': qui mixed  `output' ib0.treatment period	 				|| indep_obs: || id: ,vce(robust)
eststo  r_`output':  qui margins ,dydx(treatment) post
		local   coef_`output'_r = r(table)[1,2]	
		
eststo  a1_`output': qui mixed  `output' ib0.treatment period $reg_cov 				|| indep_obs: || id: ,vce(robust)
eststo  a_`output':  qui margins ,dydx(treatment) atmeans post
		local   coef_`output'_a = r(table)[1,2]	

qui mixed  `output' ib0.treatment period	 									|| indep_obs: || id: ,vce(robust)
eststo   rr_`output': qui margins ,over(treatment) post
qui mixed  `output' ib0.treatment period $reg_cov 								|| indep_obs: || id: ,vce(robust)
eststo   aa_`output': qui margins ,over(treatment) atmeans post

}



********************************************************************************
*						Relative Effect Size								   *
********************************************************************************

sum 	arming if treatment==0 & attack==0
local   SDarming_UE_def= r(sd)
sum 	arming if treatment==0
local   SDarming_UE = r(sd)
sum 	attack if treatment==0
local   SDatt_UE = r(sd) 
sum 	arming if treatment==0 & attack==1
local   SDarming_UE_agg = r(sd)

local 	effect_att = -1*`coef_attack_r'/`SDatt_UE'
display "Effect Size Attack: (no control)" round(`effect_att', 0.0001)
local 	effect_att = -1*`coef_attack_a'/`SDatt_UE'
display "Effect Size Attack: (with controls)" round(`effect_att', 0.0001)

local 	effect_arming = -1*`coef_arming_r'/`SDarming_UE'
display "Effect Size Arming (no control): " round(`effect_arming', 0.0001)
local 	effect_arming = -1*`coef_arming_a'/`SDarming_UE'
display "Effect Size Arming (with controls): " round(`effect_arming', 0.0001)

local 	effect_att_def = -1*`coef_arm_def_r'/`SDarming_UE_def'
display "Effect Size Defensive Arming: (no control)" round(`effect_att_def', 0.0001)
local 	effect_att_def = -1*`coef_arm_def_a'/`SDarming_UE_def'
display "Effect Size Defensive Arming: (control)" round(`effect_att_def', 0.0001)

local 	effect_att_agg = -1*`coef_arm_att_r'/`SDarming_UE_agg'
display "Effect Size Aggressive Arming: (no control)" round(`effect_att_agg', 0.0001)
local 	effect_att_agg = -1*`coef_arm_att_a'/`SDarming_UE_agg'
display "Effect Size Aggressive Arming: (control)" round(`effect_att_agg', 0.0001)

********************************************************************************
esttab 	rr_attack aa_attack   rr_arming aa_arming rr_arm_att aa_arm_att rr_arm_def aa_arm_def   ///
	using "$out\AverageEffects.tex" ,   replace ///
	mtitles("Attack" "Attack" "Arming" "Arming" ) ///
	nobaselevels title("Table 2: Average Treatment Effects - Control Means") cells(b(fmt(3))) ///
	label keep(0.treatment) star(* .05 ** .01 *** .001)

esttab 	r_attack a_attack   r_arming a_arming r_arm_att a_arm_att r_arm_def a_arm_def   ///
	using "$out\AverageEffects.tex" ,   append ///
	mtitles("Attack" "Attack" "Arming" "Arming" ) ///
	nobaselevels title("Table 2: Average Treatment Effects") cells(b(fmt(3)star) se(fmt(3) par)  ) ///
	label keep(*.treatment) star(* .05 ** .01 *** .001)	

esttab rr_choose_UP aa_choose_UP rr_choose_AP aa_choose_AP rr_choose_AC aa_choose_AC ///
	using "$out\AverageEffects.tex" ,   append ///
	mtitles("Choose UP" "Choose UP" "Choose AP" "Choose AP" "Choose AC" "Choose AC") ///
	nobaselevels title("Table S5: Average Treatment Effect - Control Means") cells(b(fmt(3))) ///
	label keep(0.treatment) star(* .05 ** .01 *** .001)
	
esttab r_choose_UP a_choose_UP r_choose_AP a_choose_AP r_choose_AC a_choose_AC ///
	using "$out\AverageEffects.tex" ,   append ///
	mtitles("Choose UP" "Choose UP" "Choose AP" "Choose AP" "Choose AC" "Choose AC") ///
	nobaselevels title("Table S5: Average Treatment Effect") cells(b(fmt(3)star) se(fmt(3) par)) ///
	label keep(*.treatment) star(* .05 ** .01 *** .001)	
	
esttab 	r1_attack a1_attack   r1_arming a1_arming r1_arm_att a1_arm_att r1_arm_def a1_arm_def   ///
	using "$out\AverageEffects.tex" ,   append ///
	mtitles("Attack" "Attack" "Arming" "Arming" ) ///
	nobaselevels title("Table S8: Average Treatment Effect") ///
	cells(b(fmt(3)star) se(fmt(3) par)) ///
	label star(* .05 ** .01 *** .001)	
	
********************************************************************************
* 						Table S4: 	Only Period 1							   *
********************************************************************************
eststo     r1_attack_1: qui meprobit attack ib0.treatment if period==1				|| session: ,vce(robust)
eststo     r_attack_1:  qui margins ,dydx(treatment) post
eststo     a1_attack_1: qui meprobit attack ib0.treatment $reg_cov if period==1		|| session: ,vce(robust)
eststo     a_attack_1:  qui margins ,dydx(treatment) atmeans post

qui meprobit attack ib0.treatment period if period==1							|| session:  ,vce(robust)
eststo     rr_attack_1: qui margins ,over(treatment) post
qui meprobit attack ib0.treatment period $reg_cov if period==1					|| session:  ,vce(robust)
eststo     aa_attack_1: qui margins ,over(treatment) atmeans post
 
eststo  r1_arming_1:    qui mixed  arming ib0.treatment if period==1			|| session:  ,vce(robust)
eststo   r_arming_1:    qui margins ,dydx(treatment) post
eststo a1_arming_1: 	  qui mixed  arming ib0.treatment $reg_cov if period==1	|| session: ,vce(robust)
eststo   a_arming_1:    qui margins ,dydx(treatment) atmeans post

qui mixed  arming ib0.treatment period if period==1							|| session:  ,vce(robust)
eststo   rr_arming_1: qui margins ,over(treatment) post
qui mixed  arming ib0.treatment period $reg_cov if period==1				|| session:  ,vce(robust)
eststo   aa_arming_1: qui margins ,over(treatment) atmeans post

*Outcomes
foreach output of varlist  conflict unarmed_peace {
eststo  r1_`output': qui meprobit `output' ib0.treatment period 				|| indep_obs: ,vce(robust)
eststo  r_`output' : qui margins ,dydx(treatment) post

qui meprobit `output' ib0.treatment period  									|| indep_obs: ,vce(robust)
eststo     rr_`output': qui margins ,over(treatment) post
}


********************************************************************************
********************************************************************************

esttab rr_conflict rr_unarmed_peace  ///
	using "$out\AverageEffects.tex" ,   append ///
	mtitles("Conflict" "UP" ) ///
	nobaselevels title("Table S6: Average Treatment Effect: Outcomes - Control Means") cells(b(fmt(3))) ///
	label keep(0.treatment) star(* .05 ** .01 *** .001)	
esttab r_conflict r_unarmed_peace  ///
	using "$out\AverageEffects.tex" ,   append ///
	mtitles("Conflict" "UP" ) ///
	nobaselevels title("Table S6: Average Treatment Effect: Outcomes") cells(b(fmt(3)star) se(fmt(3) par)) ///
	label keep(*.treatment) star(* .05 ** .01 *** .001)

	


********************************************************************************
*****						Average Endowment Effects					   *****
********************************************************************************

eststo clear 
*Attacking
qui meprobit attack ib120.endowment  		  				if treatment==0	|| indep_obs: || id: ,vce(robust)
eststo r_attack: qui margins ,dydx(endowment) post
	   local   coef_attack_r = r(table)[1,1]
qui meprobit attack ib120.endowment $reg_cov  				if treatment==0	|| indep_obs: || id: ,vce(robust)
eststo 	a_attack: qui margins ,dydx(endowment) atmeans post
		local   coef_attack_a = r(table)[1,1]
		
qui meprobit attack ib120.endowment  		  				if treatment==0	|| indep_obs: || id: ,vce(robust)
eststo rr_attack: qui margins ,over(endowment) post
qui meprobit attack ib120.endowment $reg_cov  				if treatment==0	|| indep_obs: || id: ,vce(robust)
eststo aa_attack: qui margins ,over(endowment) atmeans post


*Arming & Relative Arming
foreach output of global reg_out_cont {
qui mixed  	`output' ib120.endowment 			  			if treatment==0	|| indep_obs: || id: ,vce(robust)
eststo   	r_`output':  qui margins ,dydx(endowment) post
			local   coef_`output'_r = r(table)[1,1]	
qui mixed  	`output' ib120.endowment $reg_cov    			if treatment==0	|| indep_obs: || id: ,vce(robust)
eststo   	a_`output':  qui margins ,dydx(endowment) atmeans post
			local   coef_`output'_a = r(table)[1,1]

qui mixed  `output' ib120.endowment 			  			if treatment==0	|| indep_obs: || id: ,vce(robust)
eststo   rr_`output':  qui margins ,over(endowment) post
qui mixed  `output' ib120.endowment $reg_cov    			if treatment==0	|| indep_obs: || id: ,vce(robust)
eststo   aa_`output':  qui margins ,over(endowment) atmeans post
}

********************************************************************************
*						Relative Effect Size								   *
********************************************************************************
 
sum 	arming if endowment==120
local   SDarming_120 = r(sd)
sum 	attack if endowment==120
local   SDatt_120 = r(sd) 
sum 	arming if endowment==120 & attack==1
local   SDarming_120_agg = r(sd)
sum 	arming if endowment==120 & attack==0
local   SDarming_120_def= r(sd)

cls
local 	effect_att = -1*`coef_attack_r'/`SDatt_120'
display "Effect Size Attack: (no control)" round(`effect_att', 0.0001)
local 	effect_att = -1*`coef_attack_a'/`SDatt_120'
display "Effect Size Attack: (with controls)" round(`effect_att', 0.0001)

local 	effect_arming = -1*`coef_arming_r'/`SDarming_120'
display "Effect Size Arming (no control): " round(`effect_arming', 0.0001)
local 	effect_arming = -1*`coef_arming_a'/`SDarming_120'
display "Effect Size Arming (with controls): " round(`effect_arming', 0.0001)

local 	effect_att_def = -1*`coef_arm_def_r'/`SDarming_120_def'
display "Effect Size Defensive Arming: (no control)" round(`effect_att_def', 0.0001)
local 	effect_att_def = -1*`coef_arm_def_a'/`SDarming_120_def'
display "Effect Size Defensive Arming: (control)" round(`effect_att_def', 0.0001)

local 	effect_att_agg = -1*`coef_arm_att_r'/`SDarming_120_agg'
display "Effect Size Aggressive Arming: (no control)" round(`effect_att_agg', 0.0001)
local 	effect_att_agg = -1*`coef_arm_att_a'/`SDarming_120_agg'
display "Effect Size Aggressive Arming: (control)" round(`effect_att_agg', 0.0001)

********************************************************************************

esttab 	rr_attack aa_attack rr_arming aa_arming rr_arm_att aa_arm_att rr_arm_def aa_arm_def ///
		using "$out\AverageEffects.tex" ,   append  keep(120.endowment) ///
		mtitles("Attack" "Attack" "Arming" "Arming" "Offensive Arming" "Offensive Arming" "Defensive Arming" "Defensive Arming")  ///
		nobaselevels title("Table 3: Average effect of different resource endowments - control means") cells(b(fmt(3)) ) ///
		star(* .05 ** .01 *** .001) 

esttab 	r_attack a_attack r_arming a_arming r_arm_att a_arm_att r_arm_def a_arm_def ///
		using "$out\AverageEffects.tex" ,   append ///
		mtitles("Attack" "Attack" "Arming" "Arming" "Offensive Arming" "Offensive Arming" "Defensive Arming" "Defensive Arming")  ///
		nobaselevels title("Table 3: Average effect of different resource endowments") cells(b(fmt(3)star) se(fmt(3) par)) ///
		star(* .05 ** .01 *** .001) 
		
	

	
********************************************************************************
*****								Beliefs								   *****
********************************************************************************

eststo clear
*Binary Indicators 
foreach output of global Out_beliefs_bin {
eststo     r1_`output': qui probit `output' ib0.treatment 			,vce(robust)
eststo     r_`output':  qui margins ,dydx(treatment) post
eststo     a1_`output': qui probit `output' ib0.treatment $reg_cov 	,vce(robust)
eststo     a_`output':  qui margins ,dydx(treatment) atmeans post

						 probit `output' ib0.treatment 				,vce(robust)
eststo     rr_`output':  qui margins ,over(treatment) 
						 probit `output' ib0.treatment $reg_cov 	,vce(robust)
eststo     aa_`output':  qui margins ,over(treatment) atmeans 

}


foreach output of global Outcomes_beliefs  {
						qui reg  `output' ib0.treatment	 			,vce(robust)
eststo  r_`output':    	qui margins ,dydx(treatment) post
						qui reg  `output' ib0.treatment $reg_cov 	,vce(robust)
eststo  a_`output':    	qui margins ,dydx(treatment) atmeans post

						 reg  `output' ib0.treatment	 			,vce(robust)
eststo   rr_`output': 	 qui margins ,over(treatment) post
						 reg  `output' ib0.treatment $reg_cov 		,vce(robust)
eststo   aa_`output': 	 qui margins ,over(treatment) atmeans post
}


esttab 	rr_belief_UP aa_belief_UP rr_belief_attack aa_belief_attack  rr_belief_arming aa_belief_arming    ///
	using "$out\AverageEffects.tex",   append ///
	mtitles("Belief UP" "Belief UP" "Belief Attack" "Belief Attack" "Belief Arming" "Belief Arming" ) ///
	nobaselevels title("Table S9: Beliefs - control means") cells(b(fmt(3)) ) ///
	label keep(0.treatment) star(* .05 ** .01 *** .001)

esttab 	r_belief_UP a_belief_UP   r_belief_attack a_belief_attack r_belief_arming a_belief_arming   ///
	using "$out\AverageEffects.tex" ,   append ///
	mtitles("Belief UP" "Belief UP" "Belief Attack" "Belief Attack" "Belief Arming" "Belief Arming") ///
	nobaselevels title("Table S9: Beliefs") cells(b(fmt(3)star) se(fmt(3) par)  ) ///
	label keep(*) star(* .05 ** .01 *** .001)	


	
	
********************************************************************************
*****					Multinomial Regression							   *****
********************************************************************************
eststo clear

preserve 
drop if Strategies3==3
		
cls
qui mlogit  Strategies3 trust riskaverse lossavers , base(2)
qui estpost margins 	, at(trust=(0(2)16)) atmeans
esttab . using "$out\Figure 5_data.csv", cell("b se")  append ///
			title("Multinomial Marginsplot - TRUST") 


qui mlogit  Strategies3 trust riskaverse lossavers , base(2)
qui estpost margins 	, at(riskaverse=(0(0.1)1)) atmeans
esttab . using "$out\Figure 5_data.csv", cell("b se")  append ///
			title("Multinomial Marginsplot - Risk")

mlogit  Strategies3 trust riskaverse lossavers, base(2)
estpost margins 	, at(lossavers=(0(0.1)1)) atmeans
esttab . using "$out\Figure 5_data.csv", cell("b se")  append ///
			title("Multinomial Marginsplot - Loss")

			
* Table B7: Multinomial regression: 		
eststo clear 
mlogit  Strategies3 trust riskaverse lossavers, base(2)
mlogit  Strategies3 trust riskaverse lossavers, rrr base(2)	 	/// not exported to esttab: 

restore 


********************************************************************************
*****								Figure 1							   *****
********************************************************************************
foreach var in attack arming arm_att arm_def {
qui sum `var'  if treatment==0		,d
display "`var' - Mittelwert (UNEQUAL):" 	r(mean)
display "`var' - Lower Limit (UNEQUAL):" 	r(mean)-1.96*(r(sd)/sqrt(r(N)))
display "`var' - Upper Limit (UNEQUAL):" 	r(mean)+1.96*(r(sd)/sqrt(r(N)))

qui sum `var'  if treatment==1		,d
display "`var' - Mittelwert (EQUAL):" 		r(mean)
display "`var' - Lower Limit (EQUAL):" 		r(mean)-1.96*(r(sd)/sqrt(r(N)))
display "`var' - Upper Limit (EQUAL):" 		r(mean)+1.96*(r(sd)/sqrt(r(N)))
}


********************************************************************************
*****						Figure 2 - Choices				  				****
********************************************************************************
*sort 	Strategies3
*browse 	period id Strategies3	treatment if treatment==1 
*browse 	period id Strategies3	treatment if treatment==0


********************************************************************************
*****						Figure 3 - Development			  				****
********************************************************************************
*	collapse  (mean)  unarmed_peace armed_peace armed_conflict   ,by(treatment period )	
*	replace 	armed_peace 	= armed_peace+unarmed_peace
*	replace 	armed_conflict	= armed_conflict + armed_peace
*	browse



********************************************************************************
*****								Figure 4							   *****
********************************************************************************
foreach var in attack arming arm_att arm_def {
qui sum `var'  if endowment==120		,d
display "`var' - Mittelwert (120):" 	r(mean)
display "`var' - Lower Limit (120):" 	r(mean)-1.96*(r(sd)/sqrt(r(N)))
display "`var' - Upper Limit (120):" 	r(mean)+1.96*(r(sd)/sqrt(r(N)))

qui sum `var'  if endowment==80		,d
display "`var' - Mittelwert (80):" 		r(mean)
display "`var' - Lower Limit (80):" 	r(mean)-1.96*(r(sd)/sqrt(r(N)))
display "`var' - Upper Limit (80):" 	r(mean)+1.96*(r(sd)/sqrt(r(N)))
}











