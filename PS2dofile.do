*******************************
*------- PROBLEM SET 2 -------*
*---------- GROUP 3 ----------*
* Arianna Danese - 3162886
* Chiara Mosconi - 3158558
* Emanuela Narduzzi - 3173310
*******************************
clear all
set more off

/* Gets user name */
local user = c(username)
display "`user'"

/* Stores filepath conditionally */
if ("`user'" == "erick") {
    global filepath "/home/erick/TEMP/"
}

if ("`user'" == "user") {
    global filepath "/Users/user/Desktop/STATA/micro/files/"
}

if ("`user'" == "ariannadanese") {
    global filepath "/Users/ariannadanese/Desktop/Micrometrics/files/"
}

if ("`user'" == "chiaramosconi") {
    global filepath "/Users/chiaramosconi/Downloads/files/"
}
// Set directory
cd "$filepath"

*----------------------------------------------------------------*
**************************---QUESTION 1---************************
*----------------------------------------------------------------*

*--------------------------------------*
*------------Question 1.a--------------*


*--------------------------------------*

*--------------------------------------*
*------------Question 1.b--------------*
import delimited "$filepath/pset_4.csv"
gen reform1=0
replace reform1=1 if lfdivlaw > 1967 &  lfdivlaw < 1989
tab st if reform1==1
collapse (mean) div_rate [fw=stpop], by(year reform1) 

twoway (line div_rate year if reform1==1) (line div_rate year if reform1==0), xline(1968 1988) legend(label(1 Reform) label(2 Control))
