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

reshape wide div_rate, i(year) j(reform1)

* Generate the difference between treated and untreated states
gen diff = div_rate1 - div_rate0


twoway (line div_rate1 year ) (line div_rate0 year )  (line diff year, lpattern(dash)), xline(1968 1988) legend(label(1 Reform 1968-1988) label(2 Control) label(3 "Difference"))

clear all
import delimited "$filepath/pset_4.csv"
gen reform2 = 2

replace reform2 = 1 if  lfdivlaw > 1968 & lfdivlaw < 1974
replace reform2 = 0 if lfdivlaw ==  2000

drop if year > 1978
drop if reform2 == 2


collapse (mean) div_rate [fw=stpop], by(year reform2)

reshape wide div_rate, i(year) j(reform2)

*Generate the difference between treated and untreated states
gen diff = div_rate1 - div_rate0

twoway (line div_rate1 year) (line div_rate0 year)  (line diff year, lpattern(dash)), xline(1968 1969) legend(label(1 Reform 1969-1973) label(2 Reform 2000) label(3 "Difference"))



