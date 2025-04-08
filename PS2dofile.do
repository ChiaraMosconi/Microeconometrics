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
    global filepath "/Users/user/Desktop/STATA/micro/assignment 2/files/"
}

if ("`user'" == "ariannadanese") {
    global filepath "/Users/ariannadanese/Desktop/Micrometrics/files ps2/"
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
collapse (mean) div_rate [aw=stpop], by(year reform1) 

reshape wide div_rate, i(year) j(reform1)

* Generate the difference between treated and untreated states
gen diff = div_rate1 - div_rate0


twoway (line div_rate1 year ) (line div_rate0 year )  (line diff year, lpattern(dash)), xline(1968 1988) legend(label(1 Reform 1968-1988) label(2 Control) label(3 "Difference"))
graph export plot_b1.png, replace
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

twoway (line div_rate1 year) (line div_rate0 year)  (line diff year, lpattern(dash)), xline(1968.5) legend(label(1 Reform 1969-1973) label(2 Reform 2000) label(3 "Difference"))

*parallel trends: YES
*--------------------------------------*

*------------Question 1.c--------------*
*--------------------------------------*
clear all
import delimited "$filepath/pset_4.csv", clear
gen UNILATERAL = 2

replace UNILATERAL = 1 if  lfdivlaw > 1968 & lfdivlaw < 1974
replace UNILATERAL = 0 if lfdivlaw ==  2000

drop if year != 1978 & year != 1968 
drop if UNILATERAL == 2

generate POST=0
replace POST=1 if year == 1978

generate POST_UNILATERAL=0
replace POST_UNILATERAL=1 if POST==1 & UNILATERAL==1
save pset_4new.csv, replace

reg div_rate POST_UNILATERAL POST [aw=stpop], vce(robust)
outreg2 using "$filepath/tablec", ctitle (Regression Table Question C) replace dta

*since we do not include UNILATERAl we are disregarding the panel data structure of the dataset and pooling all observations together: this means that we are not taking into account in our regression the fact that we have a number N of US states that are observed in two periods in time (1968 and 1978)
*POST_UNILATERAL coefficient = 1.70 → Suggests that after unilateral divorce laws were introduced, the divorce rate increased significantly.
*POST coefficient = 1.38 → Indicates that divorce rates generally increased over time, even in states that did not introduce unilateral divorce.
*Interpretation: This regression does not control for pre-existing differences between treated and untreated states. If states adopting unilateral divorce already had rising divorce rates, this estimate might overstate the causal effect.

reg div_rate POST_UNILATERAL UNILATERAL POST [aw=stpop]
outreg2 using "$filepath/tablec", ctitle(Regression Table Question C) append dta

diff div_rate,  t(UNILATERAL) p(POST) robust
outreg2 using "$filepath/tablec", ctitle(Regression Table Question C) append dta

use "$filepath/tablec_dta", replace
export excel using "$filepath/tablec", replace



*This regression includes a treatment group identifier (UNILATERAL) to account for baseline differences.
*POST_UNILATERAL coefficient = -0.005 (statistically significant but near zero)
*→ Contradicts the pooled OLS and suggests that unilateral divorce had little to no effect on divorce rates when controlling for pre-existing differences.
*UNILATERAL coefficient = 1.71
*→ Indicates that states that eventually adopted unilateral divorce already had higher divorce rates before the reform.
*POST coefficient = 2.13
*→ Suggests a general increase in divorce rates over time for all states.

*The pooled OLS shows a positive and large effect of unilateral divorce on divorce rates.
*The DiD specification, which accounts for pre-existing differences, finds almost no effect.
*The key takeaway: The OLS regression likely overestimated the effect of unilateral divorce laws because it didn't control for states that already had higher divorce rates.

*--------------------------------------*

*------------Question 1.d--------------*
*--------------------------------------*

use "$filepath/pset_4new.csv", clear

matrix table_1 = J(3, 3, .)

sum div_rate if UNILATERAL==1 & POST==1 [aw=stpop]
matrix table_1[1,1]=r(mean)

sum div_rate if UNILATERAL==0 & POST==1 [aw=stpop]
matrix table_1[1,2]=r(mean)

sum div_rate if UNILATERAL==1 & POST==0 [aw=stpop]
matrix table_1[2,1]=r(mean)

sum div_rate if UNILATERAL==0 & POST==0 [aw=stpop]
matrix table_1[2,2]=r(mean)


matrix table_1[1,3]=table_1[1,1]-table_1[1,2]
matrix table_1[2,3]=table_1[2,1]-table_1[2,2]
matrix table_1[3,1]=table_1[1,1]-table_1[2,1]
matrix table_1[3,2]=table_1[1,2]-table_1[2,2]
matrix table_1[3,3]=table_1[3,1]-table_1[3,2]

matrix colnames table_1= "UNILATERAL=1" "UNILATERAL=0" "Di

matrix rownames table_1= "POST=1" "POST=0" "Difference 1"
	
* Exporting the table
putexcel set "$filepath/table_1.xlsx", replace
putexcel A1=matrix(table_1) , names nformat(number_d2)
putexcel (A1:A4), overwr bold border(right thick) 
putexcel (A1:D1), overwr bold border(bottom thick) 
putexcel (C1:C4), border(right thick) 
putexcel (A4:D4), border(top thick)	
*--------------------------------------*


*------------Question 1.e--------------*
*--------------------------------------*
clear
use "$filepath/pset_4", replace
encode st, generate(state)

xtset state year
drop if year>=1989
tab year
gen IMP_UNILATERAL=0
replace IMP_UNILATERAL=1 if year>=lfdivlaw
*regression (i)
reg div_rate IMP_UNILATERAL i.year i.state [aw=stpop], vce(robust)
estimates store reg1
outreg2 using "$filepath/tableE", ctitle (Regression Table Question E) replace dta

*regression (ii)
forval i=1/51{
	bysort state (year): gen timetrend_lin_`i'=_n if state==`i' 
	replace timetrend_lin_`i'=0 if timetrend_lin_`i'==.
}
local state_timetrend timetrend_lin_*

reg div_rate IMP_UNILATERAL i.year i.state `state_timetrend' [aw=stpop], vce(cluster state)
estimates store reg2
outreg2 using "$filepath/tableE", ctitle(Regression Table Question E) append dta

*regression (iii)

forval i=1/51{
	bysort state (year): gen timetrend_sq_`i'=_n^2 if state==`i'
	replace timetrend_sq_`i'=0 if timetrend_sq_`i'==.
}
local state_timetrend_sq timetrend_sq_*
reg div_rate IMP_UNILATERAL i.year i.state `state_timetrend' `state_timetrend_sq' [aweight = stpop], vce(cluster state)
estimates store reg3
outreg2 using "$filepath/tableE", ctitle(Regression Table Question E) append dta


use "$filepath/tableE_dta", replace
export excel using "$filepath/tableE", replace

*we account for the fact that not only years and states have specificities when looking at div_rate but that these specificities vary over time 
*--------------------------------------*


*------------Question 1.f--------------*
*------------Question 1.f--------------*
*--------------------------------------*
clear
set obs 6
gen obs = _n
gen state = floor(.9 + obs/3)
bysort state: gen year = _n
gen D = state == 1 & year == 3
replace D = 1 if state == 2 & (year == 2 | year == 3)

/* Creates simulated outcomes */
gen Y = 0.1 + 0.02 * (year == 2) + 0.05 * (D == 1) + runiform() / 100
gen Y2 = 0.1 + 0.02 * (year == 2) + 0.05 * (D == 1) + 0.3 * (state == 2 & year == 3) + runiform() / 100
gen Y3 = 0.1 + 0.02 * (year == 2) + 0.05 * (D == 1) + 0.4 * (state == 2 & year == 3) + runiform() / 100
gen Y4 = 0.1 + 0.02 * (year == 2) + 0.05 * (D == 1) + 0.5 * (state == 2 & year == 3) + runiform() / 100
save pset_simulated, replace

reg Y i.D i.state i.year, vce(robust)
outreg2 using "$filepath/table_f", ctitle (Regression Table Question F) replace dta

reg Y2 i.D i.state i.year, vce(robust)
outreg2 using "$filepath/table_f", ctitle (Regression Table Question F) append dta

reg Y3 i.D i.state i.year, vce(robust)
outreg2 using "$filepath/table_f", ctitle (Regression Table Question F) append dta

reg Y4 i.D i.state i.year, vce(robust)
outreg2 using "$filepath/table_f", ctitle (Regression Table Question F) append dta
use "$filepath/table_f_dta", replace
export excel using "$filepath/table_f", replace

*------------Question 1.g--------------*
*--------------------------------------*
* ssc install twowayfeweights
use "$filepath/pset_simulated", replace
twowayfeweights Y state year D, type(feTR)
twowayfeweights Y2 state year D, type(feTR)
twowayfeweights Y3 state year D, type(feTR)
twowayfeweights Y4 state year D, type(feTR)
*--------------------------------------*

*------------Question 1.f--------------*
*--------------------------------------*
clear all
use "$filepath/pset_4", replace
encode st, generate(state)

gen init_stpop = .
bysort state (year): replace init_stpop = stpop if year == 1956
bysort state (year): replace init_stpop = init_stpop[_n-1] if missing(init_stpop)

xtset state year
drop if year>=1989
tab year
gen IMP_UNILATERAL=0
replace IMP_UNILATERAL=1 if year>=lfdivlaw
reg div_rate IMP_UNILATERAL i.state i.year [aweight = init_stpop], vce(cluster state)

bacondecomp div_rate IMP_UNILATERAL [aweight = init_stpop] 


*------------Question 1.i--------------*
*--------------------------------------*
clear all
use "$filepath/pset_4", replace
encode st, generate(state)
xtset state year

gen tau = year - lfdivlaw // Compute tau

// Create dummies
tabulate tau, generate(tempD)  

// Rename dummies to reflect negative and positive tau values
foreach i of numlist -44/63 {
    local abs_i = abs(`i')  // Get absolute value
    if `i' < 0 {
        rename tempD`=`i'+45' D_m`abs_i'  // Negative years (D_mX)
    }
    else {
        rename tempD`=`i'+45' D_p`i'  // Positive years (D_pX)
    }
}

gen D_15plus = (tau >= 15)
gen D_minus10minus = (tau <= -10)

**Run the regresson using the unilateral divorce dummies Dτ st you created and sector (πs) and year (γt) fixed effects.
*i)
forvalues i = 10/44 {
	drop D_m`i'
}

forvalues i = 15/63 {
	drop D_p`i'
}

reg div_rate D_m* D_p* i.state i.year [aw=stpop], robust
estimates store reg1
outreg2 using "$filepath/tableI", ctitle (Regression Table Question I) replace dta
*ii)
forval i=1/51{
	bysort state (year): gen timetrend_lin_`i'=_n if state==`i' 
	replace timetrend_lin_`i'=0 if timetrend_lin_`i'==.
}
local state_timetrend timetrend_lin_*

reg div_rate D_m* D_p* i.state i.year `state_timetrend' [aw=stpop], robust
estimates store reg2
outreg2 using "$filepath/tableI", ctitle (Regression Table Question I) append dta
*iii)
forval i=1/51{
	bysort state (year): gen timetrend_sq_`i'=_n^2 if state==`i'
	replace timetrend_sq_`i'=0 if timetrend_sq_`i'==.
}
local state_timetrend_sq timetrend_sq_*

reg div_rate D_m* D_p* i.state i.year `state_timetrend' `state_timetrend_sq' [aw=stpop], robust
estimates store reg3
outreg2 using "$filepath/tableI", ctitle (Regression Table Question I) append dta

use "$filepath/tableI_dta", replace
export excel using "$filepath/table_I", replace
*--------------------------------------*

*------------Question 1.j--------------*
*--------------------------------------*
coefplot reg1, keep (D_m* D_p*) xline(0) ci(95)
graph export coefplot1.png, replace
coefplot reg2, keep (D_m* D_p*) xline(0) ci(95)
graph export coefplot2.png, replace
coefplot reg3, keep (D_m* D_p*) xline(0) ci(95)
graph export coefplot3.png, replace


