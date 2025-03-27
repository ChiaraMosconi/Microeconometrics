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
collapse (mean) div_rate [aw=stpop], by(year reform1) 

reshape wide div_rate, i(year) j(reform1)

* Generate the difference between treated and untreated states
gen diff = div_rate1 - div_rate0


twoway (line div_rate1 year ) (line div_rate0 year )  (line diff year, lpattern(dash)), xline(1968 1988) legend(label(1 Reform 1968-1988) label(2 Control) label(3 "Difference"))
graph export "$path/plot_b1.png", replace

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

reg div_rate POST_UNILATERAL POST [aw=stpop], vce(robust)
outreg2 using "$path/table_c.xls", title("Regression Table Question C") label excel append


*since we do not include UNILATERAl we are disregarding the panel data structure of the dataset and pooling all observations together: this means that we are not taking into account in our regression the fact that we have a number N of US states that are observed in two periods in time (1968 and 1978)
*POST_UNILATERAL coefficient = 1.70 → Suggests that after unilateral divorce laws were introduced, the divorce rate increased significantly.
*POST coefficient = 1.38 → Indicates that divorce rates generally increased over time, even in states that did not introduce unilateral divorce.
*Interpretation: This regression does not control for pre-existing differences between treated and untreated states. If states adopting unilateral divorce already had rising divorce rates, this estimate might overstate the causal effect.

reg div_rate POST_UNILATERAL UNILATERAL POST [aw=stpop]
diff div_rate,  t(UNILATERAL) p(POST), vce(robust)
outreg2 using "$path/table_c.xls", title("Regression Table Question C")  label excel append

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


