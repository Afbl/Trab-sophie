* 	Main Data Analysis

0) 	Ent-zip Replication_Conflict.zip

1) 	In the folder, there are several files:

	One data file with the raw data: DATA_YGAME-D-24-00018R1.dta

	Two STATA do-files: 

		1_Data_Import_and_Prep.do 
		2_Analysis.do


2) 	Open 2_Analysis.do (it will also run 1_Data_Import_and_Prep.do)

	Input the directory of your folder into the third line of the do-file: 	
	"gl FolderDirect "[your directory the folder "Replication_Conflict ]"	

	Run the do-file. It will create a Stata data-file 
	"conflict_replication1.dta" 



You will need the package "lincom — Linear combinations of parameters" to run the analysis. 


*********************************************************

OUTPUT

The output of the analysis will automatically be stored inside the newly created folder ("Output")

TABLES: 

The balencing table is stored in "Balencingtable.csv". The upper part shows the mean values and standard deviations. The lower part shows the differences between treatments. 


All regression tables (except for Table S7) are stored in "AverageEffects.tex"

	Every regression table is separated into two parts: 
		first part: 	only for extracting control means
		second part: 	regression coefficients



FIGURES

All figures are created using GraphPad Prism 9.0 (https://www.graphpad.com/scientific-software/prism/www.graphpad.com/scientific-software/prism/).

The data for the figures is generated at the bottom part of the do-file. 

	The relevant parts for Figure 2 and Figure 3 need to be run separately, because we use the command collapse and browse.


Figure 1 and Figure 1 is created using GraphPad Prism 9.0 . 


