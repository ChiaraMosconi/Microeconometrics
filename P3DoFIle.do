*******************************
*------- PROBLEM SET 3 -------*
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
    global filepath "/Users/user/Desktop/STATA/micro/assignment 3/stata_files"
}

if ("`user'" == "ariannadanese") {
    global filepath "/Users/ariannadanese/Desktop/Micrometrics/files ps3/"
}

if ("`user'" == "chiaramosconi") {
    global filepath "/Users/chiaramosconi/Downloads/files/"
}
// Set directory
*cd "$filepath"


*ssc install rdrobust
*ssc install rddensity
*ssc install lpdensity

*In this problem set, any rdrobust output should be reported with Conventional betas and standard errors.
*Hint: Unless asked otherwise, use as default options for your rdrobust estimates:
*kernel(triangular) p(1) bwselect(mserd)

*----------------------------------------------------------------*
**************************---QUESTION 1---************************
*----------------------------------------------------------------*

use "$filepath/pset_3", clear
describe

*Before estimating our treatment effect, we ought to perform a set of diagnosis tests: (T1) show that a discontinuity in treatment exists at our cutoff; (T2) show that discontinuities in other covariates or pre-determined variables do not exist at our cutoff; (T3) test for the null hypothesis that the density of our running variable does not exhibit a discontinuity at the cutoff; (T4) show that discontinuities in our running variable and outcomes do not exist away from our cutoff.

*--------------------------------------*
*------------Question 1.a--------------*
*(T1) show that a discontinuity in treatment exists at our cutoff
rdplot T X, graph_options(title(Islamic Mayor-Islamic Vote discontinuity plot) ytitle(treatment variable) xtitle(running variable) legend(off)) 
graph rename exercise_1a, replace
graph export exercise_1a.pdf, replace

/* From the graph we can observe a discontinuity in treatment at the cutoff running variable = 0. 
This is a sharp design as the probability of receiving treatment changes from 0 to 1 at the cutoff of the running variable. 
In practice, at the at the cutoff value of 0 (i.e. where the Islamic party just wins or loses), the treatment variable jumps from 0 to 1. When the vote margin is less than 0 (Islamic party loses), the treatment is 0. When the vote margin is greater than 0 (Islamic party wins), the treatment is 1.
A fuzzy RD would instead show a jump in the probability of treatment, but not a deterministic shift: the treatment probability would increase at the 0 cutoff but would not go from 0 to 1.*/

*--------------------------------------*

*--------------------------------------*
*------------Question 1.b--------------*

local covariates "hischshr1520m i89 vshr_islam1994 partycount lpop1994 merkezi merkezp subbuyuk buyuk"

local num: list sizeof covariates
mat balance = J(`num',4,.)
mat list balance 
local row = 1
foreach z in `covariates' {
    qui rdrobust `z' X
	mat balance[`row',1] = round(e(h_l), .001)
	mat balance[`row',2] = round(e(tau_cl), .001)
	mat balance[`row',3] = round(e(pv_cl), .001)
	mat balance[`row',4] = round(e(N_h_l), .001)
	local ++row
}
mat rownames balance = "Share Men (15-20 y.o.) with High Sch Education" "Islamic Mayor in 1989" "Islamic vote share 1994" "N parties receiving votes 1994" " Log Population in 1994" " District center" " Province center" "Sub-metro center" "Metro center"
mat colnames balance = "MSE-Optimal Bandwidth" "RD Estimator" "p-value" "Effective Number of Observations"
mat list balance 

putexcel set "$filepath/Table_1.xlsx", replace
putexcel A1=matrix(balance), names nformat(number_d2)
putexcel A1="Label"
putexcel (A1:A10), overwr bold border(right thick) 
putexcel (B1:E1), overwr bold border(bottom thick)
*--------------------------------------*

* (c) *
// Create a mapping between variable names and their labels
local hischshr1520m_label "Share Men (15-20 y.o.) with High Sch Education"
local i89_label "Islamic Mayor in 1989"
local vshr_islam1994_label "Islamic vote share 1994"
local partycount_label "N parties receiving votes 1994"
local lpop1994_label "Log Population in 1994"
local merkezi_label "District center"
local merkezp_label "Province center"
local subbuyuk_label "Sub-metro center"
local buyuk_label "Metro center"

foreach z in `covariates' {
    // Get the label for the current variable
    local graph_title : di "``z'_label'"
    
    rdplot `z' x , graph_options(title("`graph_title'-X discontinuity") legend(off) name(`z'_X, replace)) 
}

graph combine hischshr1520m_X i89_X vshr_islam1994_X partycount_X lpop1994_X merkezi_X merkezp_X subbuyuk_X buyuk_X, ///
    title("RD Plots for Baseline Covariates") ///
    note("Discontinuity plots for all baseline covariates")
    
graph export "$filepath/Graph_1.png", replace width(2000) height(1600)

* (d) *
rdrobust Y X, kernel(triangular) p(1) bwselect(mserd)
scalar h_left = -e(h_l)
scalar h_right = e(h_r)
twoway (histogram X if X >=h_left & X < 0, freq width(1) color(eltblue)) ///
         (histogram X if X >= 0 & X <= h_right, freq width(1) color(sienna) xline(0, lcolor(black) lpattern(solid))), xlabel(-30(10)30) name(hist_1, replace) ///
      graphregion(color(white)) xtitle(Score) ytitle(Number of Observations) legend(off) 


local h_l = h_left
local h_r = h_right
rddensity X, plot plot_range(`h_l' `h_r') graph_opt(name(hist_2, replace) legend(off) xline(0, lcolor(black) lpattern(solid)) xtitle(Score) ytitle(Density))
graph combine hist_1 hist_2
graph export "$filepath/Graph_2.png", replace

* (e) *
rddensity X, plot all

/* Rddensity is a test to provide evidence for or against the presence of a discontinuity in our running variable X's density at the zero cutoff as  proposed in Cattaneo, Jansson and Ma (2020). The credibility of regression discontinuity design is linked to a presumed as-good-as-random assignment to treatment near cutoff, which makes the individuals just above and below the zero cutoff credile counterfactuals. For this reason, random assignment near cutoff requires the absence of manipulation of the running variable near cutoff. 
In this case, the null hypothesis of "NO manipulation" is that the density of the running variable is "continuous" at the cutoff. For a valid RD design, we should not have any discontinuity - as it hints for a manipulation of the RD design. Fortunatly, and pointing towards the validity of RD design, we do not reject the null hypothesis using the robust betas, since the manipulation test is T=-1.3937 with an associated p-value of 0.1634. Hence, there is evidence in favor of random assignment at cutoff. 
However, using conventional betas, the manipulation test is T=-2.445 and the associated p-value of 0.0145 would lead to the rejection of the null at the 5% level. This would invalidate the RD design. Additionally, for a viusal check we plotted the associated graph of the test. From the latter, we can see that the two confidence intervals overlap at the cutoff. This is further graphical evidence in favor of the absence of manipulation. */ 


* (f) *
*** Baseline Test
rdrobust Y X, c(-10) 
*conventional p-value: 0.003 ; robust p-value: 0.006
rdrobust Y X, c(-5) 
*conventional p-value: 0.246 ; robust p-value: 0.268 
rdrobust Y X, c(5) 
*conventional p-value: 0.624 ; robust p-value: 0.472
rdrobust Y X, c(10) 
*conventional p-value: 0.193 ; robust p-value: 0.162

*** Robust Procedure
preserve
drop if T==1
rdrobust Y X, c(-10)
*conventional p-value: 0.157 ; robust p-value: 0.295 
rdrobust Y X, c(-5)
*conventional p-value: 0.300 ; robust p-value: 0.495
restore

preserve
drop if T==0
rdrobust Y X, c(10)
*conventional p-value: 0.301 ; robust p-value: 0.416
rdrobust Y X, c(5)
*conventional p-value: 0.447 ; robust p-value: 0.462
restore


* (g) *
rdplot Y X, nbins(20 20) binselect(es) graph_options(title("RD plot") ytitle(Outcome) xtitle(Running Variable) graphregion(color(white)) legend(off))
graph export "$filepath/RDplot_Y_X.pdf", replace

* (h) *
rdrobust Y X, p(1) kernel(uni) bwselect(mserd)
* coefficient: 3.2019 ; conventional p-value: 0.018; robust p-value: 0.041
rdrobust Y X, p(1) kernel(tri) bwselect(mserd)
* coefficient: 3.0195 ; conventional p-value: 0.034 ; robust p-value: 0.076   

ereturn list
scalar  opt_i=e(h_l)

/* Electing a mayor from an Islamic party has a statistically significant effect on the educational attainment of women at the 10% level for all specifications. Note that p-values increase from uniform to trinagular kernel, and from conventional to robust. In particular, employing different kernels has a minor effect on the magnitude of the coefficients (triangular kernel is 3% smaller than uniform kernel). Hence, results are robust to different kernel choices. In particular, having an Islamic mayor in 1994 increases the share of women aged 15-20 with high-school education by 3.20 percentage points (using uniform kernel) or 3.02 percentage points (using triangular kernel). */


* (i) *

gen X_2=X^2
gen X_3=X^3
gen X_4=X^4

gen X_T=T*X
gen X_T_2=T*X_2
gen X_T_3=T*X_3
gen X_T_4=T*X_4

sum X, d
scalar min = r(min)
scalar max = r(max)
gen weights = .
replace weights = (1-abs(X/min)) if X<0
replace weights = (1-abs(X/max)) if X>=0

*global regression
reg Y T X X_2 X_3 X_4 X_T X_T_2 X_T_3 X_T_4 
* T coefficient: 3.682873 ; T p-value: 0.024

*global regression with triangular kernel 
reg Y T X X_2 X_3 X_4 X_T X_T_2 X_T_3 X_T_4 [aw = weights]
* T coefficient: 3.028359 ; T p-value: 0.043


* (j) *
*** First approach
rdrobust Y X, kernel(triangular) bwselect(mserd)
scalar h_l=-e(h_l)
scalar h_r=e(h_r)

local opt_i = e(h_l)

reg Y X if X < 0 & X >= h_l
matrix coef_left = e(b)
scalar intercept_left = coef_left[1, 2]

reg Y X if X >= 0 & X <= h_r
matrix coef_right = e(b)
scalar intercept_right = coef_right[1, 2]

scalar difference = intercept_right - intercept_left

scalar list difference
rdrobust Y X, kernel(triangular) p(1) bwselect(mserd)
*difference =  3.0595105. 

*** Correct approach
scalar h_l = -h_l

gen weights_2 = .

replace weights_2 = (1 - abs(X/h_l)) if X < 0 & X >= h_l
replace weights_2 = (1 - abs(X/h_r)) if X >= 0 & X <= h_r

reg Y X [aw = weights_2] if X >= h_l & X < 0
matrix coef_left = e(b)
scalar intercept_left = coef_left[1, 2]

reg Y X [aw = weights_2] if X >= 0 & X <= h_r
matrix coef_right = e(b)

scalar intercept_right = coef_right[1, 2]
scalar difference = intercept_right - intercept_left

scalar list difference
rdrobust Y X, kernel(triangular)

/* A global approach to sharp RDD estimation requires to use the full sample and run a regression of the outcome on a constant, while adding a higher-order polynomial (in this case, of order 4). The limitation of this method is that it gives large weights to points far from the discontiunity, and estimates are sensitive to the degree of polynomial fitted. For this reason, it is often preferrable to use a local approach and restrict the sample to an optimal bandwith, and employ a linear polynomial. Such local linear regressions are run above and below the cutoff, and the treatment effect is identified by the difference between the two intercepts. In this case, the treatment effect identified by the difference is 3.06. This means that having an Islamic mayor in 1994 increases the share of women aged 15-20 with high-school education by 3.06 percentage points. 

This is different from the estimate reported in (h) using the triangular kernel (3.02). In fact, to replicate rdrobust estimates under triangular kernels, we need to estimate WLS incorporating the relevant weights, computed through the trinagular kernel formula. Using such procedure yields a difference estimate of 3.02, which is identical to the one in (h), hence holding an equivalent interpretation. */

* (k) *
rdrobust Y X, kernel(tri) p(1) bwselect(mserd)
di .5*abs(h_l)
di .75*abs(h_l)
di 1*abs(h_l)
di 1.25*abs(h_l)
di 1.5*abs(h_l)

matrix define R = J(5, 6, .)
global bandwidths "8.6199686 12.929953 17.239937 21.549922 25.859906"
local r = 1
foreach k of global bandwidths {
	rdrobust Y X, h(`k') kernel(tri) p(1)
	matrix R[`r', 1] = `k'
	matrix R[`r', 2] = e(tau_cl)
	matrix R[`r', 3] = e(tau_bc)
	matrix R[`r', 4] = e(se_tau_cl)
	matrix R[`r', 5] = R[`r', 2] - invnormal(0.975) * R[`r', 4]
	matrix R[`r', 6] = R[`r', 2] + invnormal(0.975) * R[`r', 4]
	local r = `r' + 1
}

preserve
	clear
	svmat R
	twoway (rcap R5 R6 R1, lcolor(navy)) /*
	*/ (scatter R2 R1, mcolor(cranberry) yline(0, lcolor(black) lpattern(dash))), /*
	*/ graphregion(color(white)) xlabel(8.6199686 12.929953 17.239937 21.549922 25.859906) ytitle("RD Treatment Effect") /*
	*/ legend(off) xtitle("Bandwidth") yscale(range(-5 10)) 
	graph export "$filepath/Graph_3.png", replace
restore


/* From this graph it is possible to see that our coefficients do not significantly vary. This is a sign that the RD design is robust to different bandwidth choices. Yet, we do need a bandwith that exceeds 17.239937 to yield statistically significant estimates i.e. identify a significant jump. The underlying idea is that for large enough bandwiths, we increase the number of observations, decreasing the variance and the standard errors, hence shrinking the confidence interval. */

*----------------------------------------------------------------*
**************************---QUESTION 2---************************
*----------------------------------------------------------------*
clear all 

*(a)*
use "$filepath/fraud_pcenter_final", replace


gen  X= _dist
label variable X "Distance from the coverage boundary"
replace X= - _dist if cov == 0
gen T=cov
label variable T "Coverage dummy"

gen Y_1 = vote_comb
label variable Y_1 "Share of votes under Category C fraud"
gen Y_2 = vote_comb_ind
label variable Y_2 "At least one station with category C fraud"


rdplot T X if X> -20 & X< 20, graph_options( ylab(,nogrid) xlab(,nogrid) xtitle("X (distance to coverage boundary )" ) ytitle("T (cell phone coverage)")legend(off))
graph export "$filepath/RDplot_T_X_p(4).pdf", replace
rdplot T X if X> -20 & X< 20, p(1) graph_options( ylab(,nogrid) xlab(,nogrid) xtitle("X (distance to coverage boundary )" ytitle("T (cell phone coverage)"))legend(off))
graph export "$filepath/RDplot_T_X_p(1).pdf", replace


*** Using Y1
rdrobust Y_1 X, fuzzy(T) kernel(triangular) p(1) bwselect(mserd)
outreg2 using "$filepath/TABLE_1.tex", ctitle(Share of votes under Category C fraud) label addstat(Conventional p-value, e(pv_cl), Robust p-value, e(pv_rb), Order Loc. Poly. (p), e(p), Order Bias (q), e(q)) tex(frag) replace
rdplot Y_1 X if X> -20 & X< 20, p(1) graph_options( ylab(,nogrid) xlab(,nogrid) xtitle("X (distance to coverage boundary )") ytitle("Y (Share of votes under Category C fraud)")legend(off))
graph export "$filepath/RDplot_Y_1_X_p(1).pdf", replace

rdrobust Y_1 X, fuzzy(T) kernel(triangular) p(4) bwselect(mserd)
outreg2 using "$filepath/TABLE_1.tex", ctitle(Share of votes under Category C fraud) label addstat(Conventional p-value, e(pv_cl), Robust p-value, e(pv_rb), Order Loc. Poly. (p), e(p), Order Bias (q), e(q)) tex(frag) append
rdplot Y_1 X if X> -20 & X< 20, graph_options( ylab(,nogrid) xlab(,nogrid) xtitle("X (distance to coverage boundary )") ytitle("Y (Share of votes under Category C fraud)")legend(off))
graph export "$filepath/RDplot_Y_1_X_p(4).pdf", replace

*** Using Y2
rdrobust Y_2 X, fuzzy(T) kernel(triangular) p(1) bwselect(mserd)
outreg2 using "$filepath/TABLE_2.tex", ctitle(At least one station with category C fraud) label addstat(Conventional p-value, e(pv_cl), Robust p-value, e(pv_rb), Order Loc. Poly. (p), e(p), Order Bias (q), e(q)) tex(frag) replace
rdplot Y_2 X if X> -20 & X< 20, p(1) graph_options( ylab(,nogrid) xlab(,nogrid) xtitle("X (distance to coverage boundary )") ytitle("Y (At least one station with category C fraud)")legend(off))
graph export "$filepath/RDplot_Y_2_X_p(1).pdf", replace

rdrobust Y_2 X, fuzzy(T) kernel(triangular) p(4) bwselect(mserd)
outreg2 using "$filepath/TABLE_2.tex", ctitle(At least one station with category C fraud) label addstat(Conventional p-value, e(pv_cl), Robust p-value, e(pv_rb), Order Loc. Poly. (p), e(p), Order Bias (q), e(q)) tex(frag) append
rdplot Y_2 X if X> -20 & X< 20, graph_options( ylab(,nogrid) xlab(,nogrid) xtitle("X (distance to coverage boundary )") ytitle("Y (At least one station with category C fraud)")legend(off))
graph export "$filepath/RDplot_Y_2_X_p(4).pdf", replace

/* Our analysis deviates slightly from Gonzalez (2021)'s sharp RD design by introducing some noise in the measurement of longitude, which influences the distance of polling stations from the coverage boundary, our running variable. Assuming this noise is random and orthogonal to the actual distance, there's a chance that a station outside the coverage area may be measured as within it, and vice versa. This creates a fuzzy threshold at the coverage boundary, increasing the probability of treatment discontinuously but not entirely determining it. The imperfect correlation between treatment assignment and measured position relative to the boundary is reflected in negative distance variable measures in our data and in our plot of propensity score estimates against distance from the boundary. Therefore, we use a fuzzy RD design, akin to local Wald estimation using the threshold as an instrument for treatment assignment, to consistently identify the causal effect of mobile coverage on fraudulent voting, given certain assumptions.

Similar to standard IV settings, to identify the parameter of interest consistently, we need to ensure that the coverage boundary, our instrumental variable, is locally exogenous. This requires that in a small enough neighborhood of the boundary, polling stations are sorted as if randomly, unrelated to other covariates, and that the threshold itself doesn't induce a shift in the outcome variable beyond its correlation with treatment assignment. These requirements stem from the assumption of continuity in expected potential outcomes. Any discontinuity at the threshold not caused by the treatment could indicate endogenous sorting or a direct effect of the threshold, affecting both potential outcome functions.

Several potential challenges to the validity of this design have been recognized:
*1.	Selection bias into coverage areas.
*2.	Mobile coverage spillovers, where polling centers in non-coverage regions near borders may still benefit from positive spillovers.
*3.	Relocation of fraudulent activities to polling centers outside coverage areas.
*4.	Lack of data on additional mobile service providers.
*5.	Cell tower shutdowns, potentially influenced by violence and actions of groups like the Taliban.
*6.	Variations in signal quality due to environmental conditions.

To tackle these challenges, Gonzalez's (2021) study meticulously investigates these various potential biases. Initially, the section confirms the presence of selection bias into coverage areas but finds no noticeable impact on the alteration in fraudulent activities at the coverage boundary. Moreover, there is no evidence suggesting that mobile coverage spillovers or spatial displacement of frauds affect the estimates. Concerning unaccounted-for mobile service providers, which might underestimate the main results' coverage effect, the authors show that adjusting for this missing data does not significantly change the estimates. Regarding cell tower shutdowns, potentially linked to increased violence and Taliban operations, their impact on election-day coverage is explored. However, excluding provinces with the highest violence rates does not lead to any significant alteration in results. Finally, the study examines the sensitivity of results to minor shifts in coverage area boundaries due to environmental factors, finding no meaningful sensitivity to such variations. However, it acknowledges that the presence of such variations should prompt an ITT-like interpretation of their sharp RD output. Under a fuzzy design, though, the impact of such slight oscillations in the coverage boundary is different. Our instrumental variable estimates have a probability limit inversely related to first stage strength. Instead, the causal parameter of interest is inversely related to the covariance between our instrument (position relative to the threshold) and actual coverage. If the coverage dummy reflects coverage under standard environmental conditions, and on election day coverage can randomly deviate from its standard perimeter, then actual coverage is noisier and more weakly related to the boundary than the coverage dummy, leading to coefficients that systematically underestimate the parameter of interest. */


*(b)*
/* The additional results section of Gonzalez (2021) delves into scenarios where the proposed geographic boundary may not precisely determine mobile coverage on election day, such as potential fraud/coverage spillover effects near the boundary and changes in the boundary's shape due to shifting environmental conditions. The latter scenario isn't ruled out in Gonzalez's framework, implying that it doesn't entirely invalidate their sharp RD design. Instead, it suggests interpreting the estimates as intent-to-treat coefficients, estimating the direct effect of the boundary as a treatment proxy on the outcome variable. Obtaining actual fuzzy RD estimates directly isn't feasible due to missing data on coverage on election day, rendering the first stage coefficient unattainable. Recognizing this fuzziness in the boundary, adding further noise due to imperfect longitude measurement doesn't necessarily demand a design shift. This noise could be seen as an additional imperfection in predicting treatment, exacerbating existing fuzziness and diminishing first stage strength. However, since first stage estimates couldn't be acquired initially, this doesn't impact the research design. Consequently, the intent-to-treat interpretation of the sharp estimates remains feasible, but they are expected to be noisier, as additional exogenous variation induced by coverage might not be captured by the available proxy. Another approach to preserving a sharp design would be if the coverage boundary were unidimensional, extending from west to east, so that only differences in latitude determine whether a polling station falls within or outside the coverage area, rendering measurement error inconsequential. */

*(c)*
	
*** Optimal bandwidth
foreach var in 600 95 ecc comb comb_ind {
		rdbwselect vote_`var' X if ind_seg50==1, vce(cluster segment50)
		scalar hopt_`var'=e(h_mserd)
		forvalues r=1/2 {
			rdbwselect vote_`var' X if ind_seg50==1 & region2==`r', vce(cluster segment50)
			scalar hopt_`var'_`r'=e(h_mserd)
	}
}

xtset, clear
xtset segment50 pccode

*table_onedim_results
/*foreach var in /*600 95 ecc*/ comb_ind comb {	
	* All regions
	xtreg vote_`var' cov##c.(dist) if ind_seg50==1 & dist<=hopt_`var', fe robust 
		est store col1_a_`var'

	* Southeast
	xtreg vote_`var' cov##c.(dist) if ind_seg50==1 & dist<=hopt_`var'_1 & ///
	region2==1, fe robust 
		est store col1_b_`var'

	* Northwest
	xtreg vote_`var' cov##c.(dist) if ind_seg50==1 & dist<=hopt_`var'_2 & ///
	region2==2, fe robust 
		est store col1_c_`var'
 }

 
 
 
*codice dei ragazzi

*** Local Linear Regression
gen instrument_T=0
replace instrument_T=1 if X>0
gen interaction=T*X
label variable interaction "Interaction between coverage dummy and outcome variable"
gen instrument_interaction=X*instrument_T

 
*** Only using the treatment instrument 

foreach var in comb_ind comb {	
	* All regions
	xtivreg vote_`var' (T = instrument_T) X if ind_seg50==1 & _dist<=hopt_`var', fe  vce(robust)
		est store col1_a1_`var'
		

	* Southeast
	xtivreg vote_`var' (T = instrument_T) X if ind_seg50==1 & _dist<=hopt_`var'_1 & region2==1, fe vce(robust)  
		est store col1_b1_`var'
		

	* Northwest
	xtivreg vote_`var' (T = instrument_T) X if ind_seg50==1 & _dist<=hopt_`var'_2 & region2==2, fe vce(robust)
		est store col1_c1_`var'
	
 }


*** Adding treatment and interaction instrumented

foreach var in comb_ind comb {	
	* All regions
	xtivreg vote_`var' (T interaction = instrument_T instrument_interaction) X if ind_seg50==1 & _dist<=hopt_`var', fe  vce(robust)
		est store col1_a2_`var'
		

	* Southeast
	xtivreg vote_`var' (T interaction = instrument_T instrument_interaction) X if ind_seg50==1 & _dist<=hopt_`var'_1 & region2==1, fe vce(robust)  
		est store col1_b2_`var'
		

	* Northwest
	xtivreg vote_`var' (T interaction = instrument_T instrument_interaction) X if ind_seg50==1 & _dist<=hopt_`var'_2 & region2==2, fe vce(robust)
		est store col1_c2_`var'
	
 }*/

/* The coefficients linked to the proportions of votes affected by fraud, or the likelihood of fraud in individual polling centers, vary when calculated using optimal bandwidth selection compared to those derived from global fitting. Specifically, our coefficients lose in statistical significance. This decline in significance in the optimal bandwidth estimation can be explained by two main factors: 
•	Transitioning from a global to a local regression can significantly reduce the number of observations, leading to decreased precision and consequently higher standard errors. This increase in standard errors may diminish the significance of our coefficients.
•	Bias correction: a narrower bandwidth allows for more precise estimation, helping to avoid the mis-specifications inherent in the global model. This is because in global regressions, extreme observations may bias the overall fit. In this scenario, a coefficient showing no statistical difference from zero may indicate that the actual effect is indeed zero, and it is more precisely computed  using a local estimation method.

These two phenomena are connected to the trade-off between bias and variance in local and global estimation within a regression-discontinuity design. Essentially, narrower bandwidths result in smaller sample sizes, leading to increased variance. However, they also enable more precise estimation of the true effect at the threshold. However, unraveling the effects of these two factors presents a challenge. Consequently, determining whether the difference between local and global outcomes arises from one factor or the other remains elusive.

Further insights into the results can be offered. Firstly, the significance of our findings is particularly dependent on South-East regions, echoing similar observations made in Gonzalez's study. Additionally, our analysis indicates no noticeable effect of the interaction term. This absence suggests a consistent slope of the outcome variable across covariates, irrespective of the threshold being crossed. */












