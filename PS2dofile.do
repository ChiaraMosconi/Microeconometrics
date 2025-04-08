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
*we should use analytic weights (aweight) because the variable div rate (divorces per 1,000 people) is an average computed at the state-year level. Analytic weights are specifically designed for situations where each observation represents a mean, and the weight is the size of the sample from which that mean was taken.
*frequency weights are not the appropriate choice because they would treat the data as if there are multiple identical observations, which is not the case here since the div rate represents a mean. 
*sampling weights are not the appropriate choice because the data used is not survey data where observations have different probabilities of being sampled.
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
graph export "$filepath/paralleltrends.png", replace

*Graph (i) [Reform 1968–1988 vs Control]: In the pre-1968 period, both treatment and control groups show very similar upward trends in divorce rates. The green difference line is relatively flat, suggesting parallel pre-trends. After 1968, the treatment group diverges, which is what we would expect if the reform had an effect.
*Graph (ii) [Reform 1969–1973 vs Reform 2000]: the pre-treatment trends look similar between groups and the green dashed line (difference) is relatively stable. After 1969, the 1969–1973 adopters' divorce rate accelerates
*Both graphs show evidence of parallel pre-trends, which is crucial for the difference-in-differences approach to be valid.
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
*The coefficient on POST_UNILATERAL (1.701) captures the raw difference in divorce rates between treated and control states in 1978, not accounting for pre-existing differences between treated and control states.

reg div_rate POST_UNILATERAL UNILATERAL POST [aw=stpop]
outreg2 using "$filepath/tablec", ctitle(Regression Table Question C) append dta

diff div_rate,  t(UNILATERAL) p(POST) robust
outreg2 using "$filepath/tablec", ctitle(Regression Table Question C) append dta

use "$filepath/tablec_dta", replace
export excel using "$filepath/tablec", replace

*This regression includes a treatment group identifier (UNILATERAL) to account for baseline differences.
*The DiD estimator is the coefficient on POST_UNILATERAL (-0.005) which is statistically insignificant.
*→ Contradicts the pooled OLS and suggests that unilateral divorce had little to no effect on divorce rates when controlling for pre-existing differences.
*UNILATERAL coefficient = 1.71 - Indicates that states that eventually adopted unilateral divorce already had higher divorce rates before the reform - This is why the pooled OLS was misleading: it conflated pre-existing differences with the treatment effect.
*POST coefficient = 2.13 → it reflects a general time trend, suggesting a general increase in divorce rates over time for all states.

*The pooled OLS shows a positive and large effect of unilateral divorce on divorce rates.
*The DiD specification, which accounts for pre-existing differences, finds almost no effect.
*The key takeaway: The POLS regression likely overestimated the effect of unilateral divorce laws because it didn't control for states that already had higher divorce rates.
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
*IMP_UNILATERAL: -0.055 (p=0.346)

*regression (ii)
forval i=1/51{
	bysort state (year): gen timetrend_lin_`i'=_n if state==`i' 
	replace timetrend_lin_`i'=0 if timetrend_lin_`i'==.
}
local state_timetrend timetrend_lin_*

reg div_rate IMP_UNILATERAL i.year i.state `state_timetrend' [aw=stpop], vce(cluster state)
estimates store reg2
outreg2 using "$filepath/tableE", ctitle(Regression Table Question E) append dta
*IMP_UNILATERAL: +0.477 (p=0.023

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
*IMP_UNILATERAL: +0.334 (p=0.032)

*regression (i): The DiD coefficient is small, negative, and statistically insignificant. This suggests that, after controlling for state and year fixed effects, unilateral divorce laws had no detectable effect on divorce rates.

*in the following regressions, we account for the fact that not only years and states have specificities when looking at div_rate but that these specificities vary over time 
*in regression (i) we were assuming that state-specific effects are constant over time and that year-specific effects are common across all states, however we were not accounting for the possibility that states evolve differently over time 
*by adding state-specific time trends, we are allowing each state to have its own trajectory

*regression (ii) - adding State-Specific Linear Time Trends - The DiD coefficient becomes positive and statistically significant which suggests that states adopting unilateral divorce laws had an increase in divorce rates. The change from negative to positive of the DiD coefficient between regression (i) and (ii) implies that omitted trends were biasing the DiD estimate downward


*regression (iii) - including State-Specific Linear Time Trends and State-Specific Quadratic Time Trends - Adding quadratic trends reduces the estimate slightly (0.33), but the effect remains significant.

*the results change across the three specifications because in regression (i) we are assuming that state-specific factors are time-invariant, however if there is a possibility that they evolve over time, then the (i) model was not correctly specified. 
*the differences we might observe between (ii) and (iii) have to do with the shape we assume state-specific time trends have. In (ii) we assume that the trends are linear, however in reality trends may accellerate or decelerate and non-linear trend capture this feature

*the estimates across (i) (ii) and (iii) would not change if the parallel trend assumption held perfectly: then control and treated states wold follow identical divorce rate trends in the absence of the policy and there would be no state-specific time trends 

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
graph export "$output/graphs/bacondecomp.png", replace

/*Goodman-Bacon (2021) shows that the two way fixed effects estimate is a weighted average of all possible 2x2 DiD estimates
Goodman-Bacon (2021 proposes a decomposition of the difference-in-differences (DiD) estimator when treatment adoption timing varies across units, that is when there is staggered adoption. 
The main idea is that the traditional two-way fixed effects (TWFE) estimator (with state and time fixed effects in our case) can be expressed as a weighted average of all possible simpler 2x2 DiD comparisons in the data. 
*/

*the graph plots on the x-axis the weight each 2x2 estimate gets in the TWFE regression and on the y-axis the actual 2x2 DiD estimate. The dashed line shows the overall DiD estimate (-0.0298609), i.e., the average treatment effect estimated by your TWFE regression.

*the most influential 2x2 Difference-in-Differences are the ones determined by the "never treated vs timing". we also see this in the Bacon Decomposition output: most of the weight (88%/0.8800) is coming from comparisons between treated units and never-treated units. These comparisons produce a negative estimate (−0.084), which could drive the overall DiD estimate downward.

*regarding negative weight, there is no evidence of issues with negative weights in the graph 
*Goodman-Bacon (2021) explains that when treatment effects do not change over time,TWFEDD all weights are positive. Negative weights only arise when average treatment effects vary overtime.

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

rename D_15plus D_p15
rename D_minus10minus D_m10
save pset_4eventstudy, replace 
reg div_rate D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_p* i.state i.year [aw=stpop], robust
estimates store reg1
outreg2 using "$filepath/tableI", ctitle (Regression Table Question I) replace dta
*ii)
forval i=1/51{
	bysort state (year): gen timetrend_lin_`i'=_n if state==`i' 
	replace timetrend_lin_`i'=0 if timetrend_lin_`i'==.
}
local state_timetrend timetrend_lin_*

reg div_rate D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_p* i.state i.year `state_timetrend' [aw=stpop], robust nocons
estimates store reg2
outreg2 using "$filepath/tableI", ctitle (Regression Table Question I) append dta
*iii)
forval i=1/51{
	bysort state (year): gen timetrend_sq_`i'=_n^2 if state==`i'
	replace timetrend_sq_`i'=0 if timetrend_sq_`i'==.
}
local state_timetrend_sq timetrend_sq_*

reg div_rate D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_p* i.state i.year `state_timetrend' `state_timetrend_sq' [aw=stpop], robust nocons
estimates store reg3
outreg2 using "$filepath/tableI", ctitle (Regression Table Question I) append dta

use "$filepath/tableI_dta", replace
export excel using "$filepath/table_I", replace
*--------------------------------------*

*------------Question 1.j--------------*
*--------------------------------------*
coefplot reg1, keep (D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_p*) xline(0) ci(95)
graph export coefplot1.png, replace
coefplot reg2, keep (D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_p*) xline(0) ci(95)
graph export coefplot2.png, replace
coefplot reg3, keep (D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_p*) xline(0) ci(95)
graph export coefplot3.png, replace
coefplot reg1 reg2 reg3, keep(D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_p*) xline(0) ci(95) 
graph export coefplotall.png, replace

*------------Question 1.k--------------*
*--------------------------------------*

*--------------------------------------*





*------------Question 1.l--------------*
*--------------------------------------*
ssc install avar
ssc install eventstudyinteract
ssc install ftools
help eventstudyinteract
clear all
use "$filepath/pset_4eventstudy", replace


gen control_cohort=0
replace control_cohort=1 if lfdivlaw==2000
eventstudyinteract div_rate D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_p* [aw=stpop] , absorb(i.year i.state) cohort(lfdivlaw) control_cohort(control_cohort) vce(cluster state)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(diagonal(st_matrix("e(V_iw)"))))
matrix C = C \ A'
matrix list C
coefplot matrix(C[1]), se(C[2]) keep(D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_p*) vertical yline(0) xtitle("Years after divorce reform") ytitle("Estimated effect") ///
				title("Dependent variable: divorce rate") xlabel(, alternate) nolabel
graph export "$filepath/plot_Ql1.png", replace

eventstudyinteract div_rate D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_p* [aw=stpop] , absorb(i.year i.state) cohort(lfdivlaw) covariates(`state_timetrend')  control_cohort(control_cohort) vce(cluster state)
matrix C = e(b_iw)
mata st_matrix("A",sqrt(diagonal(st_matrix("e(V_iw)"))))
matrix C = C \ A'
matrix list C
coefplot matrix(C[1]), se(C[2]) keep(D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_p*) vertical yline(0) xtitle("Years after divorce reform") ytitle("Estimated effect") ///
				title("Dependent variable: divorce rate") xlabel(, alternate) nolabel
graph export "$filepath/plot_Ql2.png", replace

eventstudyinteract div_rate D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_p* [aw=stpop] , absorb(i.year i.state) cohort(lfdivlaw) covariates(`state_timetrend' `state_timetrend_sq')  control_cohort(control_cohort) vce(cluster state)
matrix C = e(b_iw)
mata st_matrix("A",sqrt(diagonal(st_matrix("e(V_iw)"))))
matrix C = C \ A'
matrix list C
coefplot matrix(C[1]), se(C[2]) keep(D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_p*) vertical yline(0) xtitle("Years after divorce reform") ytitle("Estimated effect") ///
				title("Dependent variable: divorce rate") xlabel(, alternate) nolabel
graph export "$filepath/plot_Ql3.png", replace


*are your results consistent with the ones from the original paper? Briefly explain what kind of correction your proposed algorithm is performing.*


