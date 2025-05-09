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

*--------------------------------------*
*------------Question 1.c--------------*
local covariates "hischshr1520m i89 vshr_islam1994 partycount lpop1994 merkezi merkezp subbuyuk buyuk"

// Create a mapping between variable names and their labels
local hischshr1520m_label "Share Men (15–20) w/ High Sch Ed"
local i89_label "Islamic Mayor in 1989"
local vshr_islam1994_label "Islamic vote share 1994"
local partycount_label "N parties receiving votes 1994"
local lpop1994_label "Log Pop 1994"
local merkezi_label "District center"
local merkezp_label "Province center"
local subbuyuk_label "Sub-metro center"
local buyuk_label "Metro center"

foreach z in `covariates' {
    // Get the label for the current variable
    local graph_title : di "``z'_label'"
    
    rdplot `z' x , graph_options(title("`graph_title'-X discontinuity", size(small)) legend(off) name(`z'_X, replace)) 
}

graph combine hischshr1520m_X i89_X vshr_islam1994_X partycount_X lpop1994_X merkezi_X merkezp_X subbuyuk_X buyuk_X, ///
    title("RD Plots for Baseline Covariates") ///
    note("Discontinuity plots for all baseline covariates")
    
graph export "$filepath/Graph_1.png", replace
*--------------------------------------*

*--------------------------------------*
*------------Question 1.d--------------*

rdrobust Y X, kernel(triangular) p(1) bwselect(mserd)
scalar h_left = -e(h_l)
scalar h_right = e(h_r)
twoway (histogram X if X >=h_left & X < 0, freq width(1) color(eltblue)) ///
         (histogram X if X >= 0 & X <= h_right, freq width(1) color(sienna) xline(0, lcolor(black) lpattern(solid))), xlabel(-30(10)30) name(hist_1, replace) ///
      graphregion(color(white)) xtitle(Score) ytitle(Number of Observations) legend(off) 
graph export "$filepath/hist_1.png", replace


local h_l = h_left
local h_r = h_right
rddensity X, plot plot_range(`h_l' `h_r') graph_opt(name(hist_2, replace) legend(off) xline(0, lcolor(black) lpattern(solid)) xtitle(Score) ytitle(Density))
graph export "$filepath/hist_2.png", replace

graph combine hist_1 hist_2
graph export "$filepath/Graph_2.png", replace
*--------------------------------------*

* (e) *
rddensity X, plot all

/* The rddensity command tests for a discontinuity in the density of the running variable X at the cutoff (zero), following the methodology proposed by Cattaneo, Jansson, and Ma (2020). In a valid regression discontinuity (RD) design, treatment assignment near the cutoff should be as good as random, making the individuals just above and below the zero cutoff credile counterfactuals. This implies that the distribution of the running variable should be smooth and continuous around the cutoff, with no signs of manipulation.
We test the null hypothesis of no manipulation, which implies continuity in the density of X at the cutoff. Using robust bias-corrected estimates, the test yields a test statistic of T=−1.3937 and a p-value of 0.1634. Since this p-value is above conventional significance levels, we fail to reject the null hypothesis, providing evidence of random assignment at the cutoff and supporting the validity of the RD design.
However, when using conventional estimates, the test statistic is T=−2.445 with a p-value of 0.0145. This would lead to rejection of the null at the 5% level, suggesting possible manipulation and undermining the RD design.

To complement the statistical results, we also examine the density plot. The visual inspection shows that the confidence intervals on either side of the cutoff overlap, offering additional graphical evidence in favor of continuity and the absence of manipulation. */



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

/* As a robustness check, we test for potential discontinuities in the outcome variable at several placebo cutoffs away from the actual threshold. The absence of significant discontinuities at these alternative points supports the credibility of the RD design by reinforcing the assumption that any observed effect at the true cutoff is not driven by spurious patterns in the data.

Using both conventional and robust p-values, we find no statistically significant discontinuities at the -5, 5, and 10 cutoffs, with all p-values well above the 10% significance level. This suggests no evidence against the null hypothesis of continuity at these placebo thresholds.

However, at the -10 cutoff, we initially reject the null using conventional (p = 0.003) and robust (p = 0.006) methods, which could be interpreted as problematic. Upon closer examination using a more robust testing procedure—where observations from the opposite treatment group are dropped—the p-values increase substantially (p = 0.157 conventional; p = 0.295 robust), indicating that the earlier rejection may have been due to contamination from treated observations. With this adjustment, the null hypothesis of no discontinuity is no longer rejected, supporting the validity of the RD design.
 */
 
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

/* We examine the effect of electing a mayor from an Islamic party on women's educational attainment using a local linear regression approach with different kernel and bandwidth choices.

The results show a statistically significant impact at the 10% level across all specifications. When using a uniform kernel, the estimated effect is 3.20 percentage points, with a conventional p-value of 0.018 and a robust p-value of 0.041. With a triangular kernel, the estimate is slightly lower at 3.02 percentage points, and the p-values rise slightly (0.034 conventional, 0.076 robust), though still within the bounds of statistical significance at the 10% level.

These findings suggest that the choice of kernel has a minor effect on the estimated magnitude (the triangular kernel produces an estimate roughly 3% smaller than the uniform kernel): electing an Islamic mayor in 1994 is associated with a meaningful increase in the share of women aged 15–20 with high school education, and the results are robust to alternative kernel choices and inference methods. */


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

/* A global approach to sharp Regression Discontinuity Design (RDD) estimation involves using the full sample and regressing the outcome on a constant and a higher-order polynomial of the running variable — here, a 4th-degree polynomial. While this method captures broader trends, it tends to assign excessive weight to observations far from the cutoff and can yield results that are highly sensitive to the chosen polynomial degree. Due to these limitations, a local approach may be preferable, focusing on observations within an optimal bandwidth around the cutoff and using a linear polynomial.

In the local linear regression framework, separate regressions are run on either side of the cutoff, and the treatment effect is identified by the difference in intercepts. In our case, this approach yields an estimated effect of 3.06 percentage points, indicating that electing an Islamic mayor in 1994 increased the proportion of women aged 15–20 with a high school education by that amount.

This estimate differs slightly from the 3.02 percentage point effect reported in section (h), which was obtained using rdrobust with a triangular kernel. To replicate the rdrobust result, one must implement a weighted least squares (WLS) regression incorporating weights derived from the triangular kernel function. Doing so produces an estimate of 3.02, perfectly matching the rdrobust output and confirming the equivalence of the two approaches under appropriate weighting. */


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


/* The graph shows that the estimated treatment effects remain stable across different bandwidths, suggesting the RD design is robust to bandwidth selection. Statistical significance, however, is only achieved when the bandwidth exceeds 17.24, indicating that a larger bandwidth is needed to detect a significant effect.This outcome reflects a common bias-variance trade-off in RD estimation: increasing the bandwidth includes more observations, which reduces variance and standard errors, thereby narrowing the confidence intervals at the cost of potentially introducing bias, as observations farther from the cutoff may be less comparable.
In this case, the consistency of the point estimates across bandwidths and the gain in statistical precision with larger bandwidths indicate that the results are both stable and statistically robust when a sufficient number of observations is included. */

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

/*As reported in Gonzalez (2021), "Distance to boundary" refers to the distance between a polling center and the closest point in the cell phone coverage boundary. "Negative" values of distance give the distance of polling centers/villages in non- coverage areas.*/

gen Y_1= total_ecc/totalv_pc
label variable Y_1 "Ratio of compromised votes"
gen Y_2 = vote_comb
label variable Y_2 "Share of votes under Category C fraud"
gen Y_3 = vote_comb_ind
label variable Y_3 "At least one station with category C fraud"

rdplot T X if X> -20 & X< 20, graph_options( ylab(,nogrid) xlab(,nogrid) xtitle("X (distance to coverage boundary )" ) ytitle("T (cell phone coverage)")legend(off))
graph export "$filepath/RDplot_T_X_p(4).pdf", replace
rdplot T X if X> -20 & X< 20, p(1) graph_options( ylab(,nogrid) xlab(,nogrid) xtitle("X (distance to coverage boundary )" ytitle("T (cell phone coverage)"))legend(off))
graph export "$filepath/RDplot_T_X_p(1).pdf", replace

/*we plotted the treatment variable used at Gonzalez (2021) as a function of the running variable according to different functional forms. In particular we tried fitting a 1st, 2nd, 3rd and 4th degree polynomial, finding that the 4th degree polynomial was the best fit.*/


*** Using T
rdrobust T X, kernel(triangular) p(1) bwselect(mserd)

rdrobust T X, kernel(triangular) p(4) bwselect(mserd)

*we replicate here Gonzalez analysis using as running variable the distance from coverage, measured using a proxy for longitude
*** Using Y1 
rdrobust Y_1 X, fuzzy(T) kernel(triangular) p(1) bwselect(mserd)
outreg2 using "$filepath/TABLE_1.tex", ctitle(Ratio of compromised votes) label addstat(Conventional p-value, e(pv_cl), Robust p-value, e(pv_rb), Order Loc. Poly. (p), e(p), Order Bias (q), e(q)) tex(frag) replace
rdplot Y_1 X if X> -20 & X< 20, p(1) graph_options( ylab(,nogrid) xlab(,nogrid) xtitle("X (distance to coverage boundary )") ytitle("Y (Ratio of compromised votes)")legend(off))
graph export "$filepath/RDplot_Y_1_X_p(1).pdf", replace

rdrobust Y_1 X, fuzzy(T) kernel(triangular) p(4) bwselect(mserd)
outreg2 using "$filepath/TABLE_1.tex", ctitle(Ratio of compromised votes) label addstat(Conventional p-value, e(pv_cl), Robust p-value, e(pv_rb), Order Loc. Poly. (p), e(p), Order Bias (q), e(q)) tex(frag) append
rdplot Y_1 X if X> -20 & X< 20, graph_options( ylab(,nogrid) xlab(,nogrid) xtitle("X (distance to coverage boundary )") ytitle("Y (Ratio of compromised votes)")legend(off))
graph export "$filepath/RDplot_Y_1_X_p(4).pdf", replace

*** Using Y2
rdrobust Y_2 X, fuzzy(T) kernel(triangular) p(1) bwselect(mserd)
outreg2 using "$filepath/TABLE_2.tex", ctitle(Share of votes under Category C fraud) label addstat(Conventional p-value, e(pv_cl), Robust p-value, e(pv_rb), Order Loc. Poly. (p), e(p), Order Bias (q), e(q)) tex(frag) replace
rdplot Y_2 X if X> -20 & X< 20, p(1) graph_options( ylab(,nogrid) xlab(,nogrid) xtitle("X (distance to coverage boundary )") ytitle("Y (Share of votes under Category C fraud)")legend(off))
graph export "$filepath/RDplot_Y_2_X_p(1).pdf", replace

rdrobust Y_2 X, fuzzy(T) kernel(triangular) p(4) bwselect(mserd)
outreg2 using "$filepath/TABLE_2.tex", ctitle(Share of votes under Category C fraud) label addstat(Conventional p-value, e(pv_cl), Robust p-value, e(pv_rb), Order Loc. Poly. (p), e(p), Order Bias (q), e(q)) tex(frag) append
rdplot Y_2 X if X> -20 & X< 20, graph_options( ylab(,nogrid) xlab(,nogrid) xtitle("X (distance to coverage boundary )") ytitle("Y (Share of votes under Category C fraud)")legend(off))
graph export "$filepath/RDplot_Y_2_X_p(4).pdf", replace

*** Using Y3
rdrobust Y_3 X, fuzzy(T) kernel(triangular) p(1) bwselect(mserd)
outreg2 using "$filepath/TABLE_3.tex", ctitle(At least one station with category C fraud) label addstat(Conventional p-value, e(pv_cl), Robust p-value, e(pv_rb), Order Loc. Poly. (p), e(p), Order Bias (q), e(q)) tex(frag) replace
rdplot Y_3 X if X> -20 & X< 20, p(1) graph_options( ylab(,nogrid) xlab(,nogrid) xtitle("X (distance to coverage boundary )") ytitle("Y (At least one station with category C fraud)")legend(off))
graph export "$filepath/RDplot_Y_3_X_p(1).pdf", replace

rdrobust Y_3 X, fuzzy(T) kernel(triangular) p(4) bwselect(mserd)
outreg2 using "$filepath/TABLE_3.tex", ctitle(At least one station with category C fraud) label addstat(Conventional p-value, e(pv_cl), Robust p-value, e(pv_rb), Order Loc. Poly. (p), e(p), Order Bias (q), e(q)) tex(frag) append
rdplot Y_3 X if X> -20 & X< 20, graph_options( ylab(,nogrid) xlab(,nogrid) xtitle("X (distance to coverage boundary )") ytitle("Y (At least one station with category C fraud)")legend(off))
graph export "$filepath/RDplot_Y_3_X_p(4).pdf", replace



/* Our empirical strategy differs slightly from the sharp RD design in González (2021) because we allow for some measurement error in longitude, which affects how we calculate the distance of polling stations from the mobile coverage boundary—the running variable. If this error is random and unrelated to the true location, it means some polling stations that are actually outside the coverage area might appear inside it, and vice versa. As a result, the probability of treatment increases discontinuously at the boundary but no longer jumps from zero to one, producing a fuzzy RD design rather than a sharp one. This fuzziness is clearly illustrated in Figure 1, which plots the probability of treatment T (cell phone coverage) against distance X from the coverage boundary. While there is a visible jump at X=0, the transition is imperfect: some polling stations just below the threshold are covered, and some just above are not, violating the core assumption of a sharp RD design. 
We formally assess this discontinuity using polynomial regression (1st and 4th degree) with the rdrobust command, treating coverage status as the outcome and distance as the running variable. The estimation results show a small but positive discontinuity in treatment probability at the boundary (τ =0.1126 s.e.=0.056 for p(1), τ =0.052 s.e. = 0.055 for p(4)), which is statistically significant at 95% level with p(1), but insignificant with p(4)(p = 0.344). The robust z-score (0.717) and wide confidence intervals further reinforce the imprecision. These results confirm that treatment is not deterministically assigned at the cutoff, violating the sharp RD assumption./*

/*Which assumptions must hold in order for the one-dimensional RD estimates of Gonzalez (2021) to be valid?

The key assumptions required for the validity of the one-dimensional RD estimates are:

(1) smooth potential outcomes: potential outcomes should vary smoothly and any discontinuity at the treatment boundary (distance = 0) should solely reflect the treatment effect

In the context of the Gonzalez (2021) estimates, polling center elevation, slope, and distance to the closest primary road are exceptions to the smooth potential outcomes assumption: this is because cell phone coverage is dependent on geographic features such as changes in elevation and slope and ruggedness of the terrain. In spite of these observed and significant changes of baseline characteristic across the boundary, robustness checks show that the main RD results in obtained in the paper are not sensitive to the inclusion of these covariates

(2) no manipulation: there must be no strategic sorting 
	- polling stations/ villages cannot have been placed deliberately "just inside" or "just outside" coverage in anticipation of the election. -> the authors 		perform Cattaneo, Jansson, and Ma's (2019) test for breaks in the density of the forcing variable at the boundary and find no evidence of endogenous assignment/sorting near the boundary
	- since the locaton of the centers was determined entirely by the UN-led IEC, it is unlikely that corrupt candidates were able to manipulate the assignment process
	
(3) every neighborhood should offer both treated and control units for comparison​​ (boundary positivity) / the authors restrict the sample to only neighborhoods with at least one polling center on each side of the coverage boundary i.e. they drop any boundary segments with polling centers on only one side of the coverage boundary

(4) SUTVA/no spillovers 

*/

*(b)*
/*
In a sharp regression discontinuity (RD) design, treatment assignment must be a deterministic function of a running variable. In the specific case in which we have a proxy for longitude, the way to keep a sharp design would be to have a unidimensional treatment boundary, extending strictly from west to east, i.e. the treatment boundary should depend only on latitude. In that case, the running variable can be defined as runvar = latitude – φ₀, and longitude plays no role. Any measurement error due to noisy longitude data is thus irrelevant and cannot cause misclassification, preserving the sharp RD structure. González (2021) confirms this with falsification tests using latitude-only cut-offs: because only latitude matters, mismeasured longitude does not affect treatment assignment, and estimated discontinuities vanish.
*/

*(c)*
	
*** Optimal bandwidth
foreach var in comb comb_ind {
		rdbwselect vote_`var' X if ind_seg50==1, vce(cluster segment50)
		scalar hopt_`var'=e(h_mserd)
		forvalues r=1/2 {
			rdbwselect vote_`var' X if ind_seg50==1 & region2==`r', vce(cluster segment50)
			scalar hopt_`var'_`r'=e(h_mserd)
	}
}

xtset, clear
xtset segment50 pccode

*we first calculate the point estimates following table_onedim_results
foreach var in comb_ind comb {	
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


*we also calculate the point estimates following the IV strategy used in the RDD slides, both using treatment as instrument and interacting it with the X variable.
*** Local Linear Regression
gen instrument_T=0
replace instrument_T=1 if X>0
gen interaction=T*X
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
	
 }

/* The coefficients linked to the proportions of votes affected by fraud, or the likelihood of fraud in individual polling centers, vary when calculated using optimal bandwidth selection compared to those derived from global fitting. Specifically, our coefficients lose in statistical significance. This decline in significance in the optimal bandwidth estimation can be explained by two main factors: 
•	Transitioning from a global to a local regression can significantly reduce the number of observations, leading to decreased precision and consequently higher standard errors. This increase in standard errors may diminish the significance of our coefficients.
•	Bias correction: a narrower bandwidth allows for more precise estimation, helping to avoid the mis-specifications inherent in the global model. This is because in global regressions, extreme observations may bias the overall fit. In this scenario, a coefficient showing no statistical difference from zero may indicate that the actual effect is indeed zero, and it is more precisely computed  using a local estimation method.

These two phenomena are connected to the trade-off between bias and variance in local and global estimation within a regression-discontinuity design. Essentially, narrower bandwidths result in smaller sample sizes, leading to increased variance. However, they also enable more precise estimation of the true effect at the threshold. However, unraveling the effects of these two factors presents a challenge. Consequently, determining whether the difference between local and global outcomes arises from one factor or the other remains elusive.

Further insights into the results can be offered. Firstly, the significance of our findings is particularly dependent on South-East regions, echoing similar observations made in Gonzalez's study. Additionally, our analysis indicates no noticeable effect of the interaction term. This absence suggests a consistent slope of the outcome variable across covariates, irrespective of the threshold being crossed. */












