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

reg div_rate POST_UNILATERAL UNILATERAL POST [aw=stpop], vce(robust)
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
*We also note that the DinD estimate that is found when using the diff command is different from the estimate that we found when running reg div_rate POST_UNILATERAL UNILATERAL POST [aw=stpop], vce(robust). This does not worry us since both coefficients are found to be not significant at any level. We believe that this difference is due to the fact that when running the reg command we can include different types of weights and in this case we have opted to use analytical weights, whereas the diff command does not allow for weighting procedures

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

matrix colnames table_1= "UNILATERAL=1" "UNILATERAL=0" "Difference 2"

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
reg div_rate IMP_UNILATERAL i.year i.state [aw=stpop], vce(cluster state)
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
set seed 3333

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
*state 1 receives treatment in year 3, state 2 receives treatment in year 2 and 3
*we have generated four outcome variables Y, Y2, Y3, Y4

*OUTCOME Y: here the parallel trends assumption holds perfectly. The true effect is 0.05, since we construct Y such that 0.05 * (D == 1)
*Our estimates is equal to 0.0523, therefore it is close to the true value. Moreover, it is significant as the p-value=0.031. Therefore, the model is correctly estimating the treatment effect.

*OUTCOME Y2, Y3 and Y4: the true effect is still 0.05 as all gen Y* contain 0.05 * (D == 1). However, we have added state-specific trends for State 2 in Year 3 (for Y2 this is 0.3 * (state == 2 & year == 3), for Y3 this is 0.4 * (state == 2 & year == 3) and for Y4 this is 0.5 * (state == 2 & year == 3)) - We can interpret this +0.3, +0.4 and +0.5 coefficients as an example of state- year specific trends or as an example of heterogeneity of treatment across time, that is a failure of the constant effect assumption. 

*Looking at our estimates when regressing Y2, Y3 and Y4, we see that the estimated effect becomes insignificant and has the wrong sign (it is negative). 
*Moreover it becomes more and more negative when going from Y2 to Y3 that is when the coefficient in front of (state == 2 & year == 3)  becomes larger. Therefore the bias grows as the state-specific trend increases (+0.3 → +0.5).
*The treatment coefficient is biased downward and even changes sign because state 2 is experiencing this extra trend in year 3 and the model is not able to distinguish between the true treatment effect and State 2's extra trend. The model wrongly attributes this state-year trend to the treatment and this leads to incorrect estimates.

*------------Question 1.g--------------*
*--------------------------------------*
* ssc install twowayfeweights
use "$filepath/pset_simulated", replace
twowayfeweights Y state year D, type(feTR)
twowayfeweights Y2 state year D, type(feTR)
twowayfeweights Y3 state year D, type(feTR)
twowayfeweights Y4 state year D, type(feTR)


	/*De Chaisemartin and d'Haultfoeuille (2020) prove that ,under the common trends assumption:
(1) the coefficient beta obtained by running a Two-Way Fixed Effects regression is the expectation of a weighted sum of the ATE in each group and period (g,t), where weights could potentially be negative 
(2) the coefficient beta obtained by running a Two-Way Fixed Effects regression is a biased estimator of the true Average Treatment

In the De Chaisemartin and d'Haultfoeuille (2020), the staggered adoption design that is taken examined is exactly the same to the one we are analyzing: three time periods (t=3, year 1, year 2 and year 3) and two groups (g=2, state 1 and state 2). Group 1  is untreated at periods 1 and2 and treated at period 3, while group 2 is untreated at period 1 and treated both at periods 2 and 3.
	
Then the β_{fe} is a weighted sum of the expected ATE (state 1, period 3), ATE (state 2, period 2) and ATE (state 2, period 3), that is the the three treated (state, year) cells. 
The weights of this sum are 1/2, for ATE (state 1, period 3), 1, for ATE (state 2, period 2), and -1/2 for ATE (state 2, period 3).  This is also confirmed by the output of the twowayfeweights command: the Positive weights are 2 and sum up to 1.5 and the negative weights  are just 1 and it is equal to -0.5.

This last negative weight, that is the weight associated with ATE(state 2, period 3), is the problematic one as it is the one biasing the β_{fe} and making it a misleading measure of the treatment effect. 

This is the reason why, despite all ATEs being positive in the regression with Y4, we have a negative coefficient. 

Let us consider the situation at Y4:
β_{fe} = 1/2 * E[ATE 1,3] + 1 * E[ATE 2,2] − 1/2 * E[ATE 2,3] .

By substituting with the ATEs we have 
β = (1/2)*0.05 + 1*0.05 - (1/2)*0.55 = 0.025 + 0.05 - 0.275 = -0.2

which is more or less close to the value -0.2001 we get when running the regression "reg Y4 i.D i.state i.year, vce(robust)", taking into account that there is still some randomness allowed when creating the outcome Y4

Intuitively, what is happening is that over time the treatment effect cumulates and state 2, during the second year of treatment (that is at year 3), is experiencing a larger average treatment effect and therefore, as shown by De Chaisemartin and d'Haultfoeuille (2020), β_{fe} is more likely to assign a negative weight to periods where a large fraction of groups are treated, and to groups treated for many periods. Therefore, in our simple scenario, negative weights are a concern because we have a state that is treated for more periods compared to the other state. In general, this is a concern when analyzing staggered adoption designs. 
We can say that in the scenario with Y4, the constant effect assumption is violated.

This is the key problem with Two-Way Fixed Effects in staggered adoption settings: TWFE rely on comparisons that can become invalid when treatment effects vary over time or across groups.  

In staggered adoption some units are treated earlier than others, however TWFE compares later-treated units (State 1 in Year 3) to already-treated units (State 2 in Year 3). This comparison is a "bad" one because the control group (State 2 in Year 3) has already been treated. If treatment effects were constant, TWFE would still work (as it can be seen in the baseline Y scenario). However, if the constant effect assumption is violated, the TWFE estimate becomes biased and even opposite signed.
	*/

*--------------------------------------*

*------------Question 1.h--------------*
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
graph export "$filepath/bacondecomp.png", replace

	/*Goodman-Bacon (2021 proposes a decomposition of the two-way fixed effects (TWFE) estimator when treatment adoption timing varies across units, that is when there is staggered adoption. 

Goodman-Bacon (2021) shows that the two way fixed effects estimate is a weighted average of all possible 2x2 DiD estimates that is of all possible two-group, two-period DiD comparisons. 
Each 2x2 comparison has a treatment effect estimate (β) and a  weight that reflects its influence on the overall TWFE estimate.

By looking at the output of the "bacondecomp div_rate IMP_UNILATERAL [aweight = init_stpop]" we see that the overall TWFE estimate is meant to be (−0.08×0.88)+(0.53×0.093)+(−0.15×0.027)≈−0.03 which matches the regression coefficient = -0.029

The graph plots on the x-axis the weight each 2x2 estimate gets in the TWFE regression and on the y-axis the actual 2x2 DiD estimate. The dashed line shows the overall DiD estimate (-0.0298609), i.e., the average treatment effect estimated by your TWFE regression.

All weights here are positive, as confirmed by both the output and the plot (therefore there is no issue of negative weights). However, the problem here is that the largest weight (88%) is associated to a negative estimate (-0.08) from  treated vs. never-treated comparisons, which pulls the overall DiD coefficient downward. Unlike in De Chaisemartin & d'Haultfoeuille (2020), the problem here isn't negative weights but heavily weighted negative 2x2 estimates.

The potential prpblem here is still that the effect of treatment could change as time passes: maybe immediately after treatment, there's a boost, but then the effect could diminish, reverse, or accumulate over time.
*/

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


/*In his paper Wolfers re-examines the findings of Friedberg (1998) and arrives at different conclusions regarding the impact of unilateral divorce laws on divorce rates. While Friedberg concludes that the adoption of unilateral divorce laws caused a significant and permanent increase in divorce rates, Wolfers finds that this effect was temporary and dissipated within approximately a decade after implementation.

Friedberg's analysis is based on a state-level panel from 1968 to 1988 and employs a difference-in-differences framework that includes state and year fixed effects, as well as state-specific linear time trends to control for preexisting trends in divorce rates.
Her preferred specification suggests that the introduction of unilateral divorce laws increased the divorce rate by approximately 0.447 divorces per 1,000 people annually. This effect is interpreted as permanent and is estimated to account for about one-sixth of the overall increase in U.S. divorce rates since the 1960s.
Wolfers replicates Friedberg's results but identifies a methodological issue in the interpretation of the state-specific time trends.
He argues that Friedberg's specification confounds the dynamic response to policy reform with pre-existing trends, particularly because her model uses a single treatment dummy variable for all post-reform years. This approach can incorrectly attribute part of the post-reform adjustment to time trends, especially when dynamic effects—such as an initial spike in divorces followed by a return to baseline—are not explicitly modeled.

To address this, Wolfers extends the dataset back to 1956 and implements an event-study design with multiple dummy variables indicating years since reform. This allows for a flexible modeling of the policy's dynamic effects. His findings show that divorce rates increase sharply in the first years after the introduction of unilateral divorce laws but gradually decline, with no statistically significant effect remaining after about ten years. In some specifications, the long-run effect is even negative, though this result is sensitive to model assumptions.

Wolfers also uses census data to examine the stock of individuals who have ever been divorced. These data show no significant long-run increase in the ever-divorced population following the legal reforms, supporting the conclusion that the policy impact was not persistent.*/


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


