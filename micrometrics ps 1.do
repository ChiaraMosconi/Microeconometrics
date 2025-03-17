*******************************
*------- PROBLEM SET 1 -------*
* Arianna Danese 
* Chiara Mosconi
* Emanuela Narduzzi
*******************************
clear all
set more off

*----------------------------------------------------------------*
**************************---QUESTION 1---************************
*----------------------------------------------------------------*

*ssc install estout, replace
*ssc install randomizr, replace
*ssc install ritest, replace
*ssc install outreg2, replace
cd "/Users/ariannadanese/Desktop/Micrometrics"
use "/Users/ariannadanese/Desktop/Micrometrics/files/jtrain2.dta", replace
*--------------------------------------*
*------------Question 1.a--------------*
matrix balcheck=(.,.,.,.,.,.)

local i=1
foreach var of varlist age educ black hisp re74 re75 nodegree{

	qui ttest `var', by(train) unequal 
	
	matrix balcheck[`i',1]=r(mu_2)
	matrix balcheck[`i',2]=r(sd_2) 
	matrix balcheck[`i',3]=r(mu_1)
	matrix balcheck[`i',4]=r(sd_1) 
	matrix balcheck[`i',5]= r(mu_1)-r(mu_2)
	matrix balcheck[`i',6]=r(se) 
	matrix list balcheck
		local i=`i'+1
	if `i'<=7 matrix balcheck=(balcheck \ .,.,.,.,.,.) 
	
}


matrix rownames balcheck= "Age" "Education" "Black" "Hispanic" "Real Earn 74" "Real Earn 75" "No Degree"
matrix colnames balcheck= "Mean Trt (1)" "StDev Trt" "Mean Ctrl (2)" "StDev Ctrl" "(1)-(2)" "StDev (1)-(2)"


local rows = rowsof(balcheck)
local cols = colsof(balcheck)

forval i = 1/`rows' {
    forval j = 1/`cols' {
        matrix balcheck[`i',`j'] = round(balcheck[`i',`j'], 0.001)
    }
}

	
	matrix list balcheck, f(%9.3f) title("Balance check")
	cd "/Users/ariannadanese/Desktop/Micrometrics/"
	save balcheck.dta, replace
	
	
	putexcel set "/Users/ariannadanese/Desktop/Micrometrics/Table_1.xlsx", replace
	
	putexcel A1=matrix(balcheck), names nformat(number_d2)
	putexcel (A2:A8), overwr bold border(right thick) 
	putexcel (B1:G1), overwr bold border(bottom thick) 

/* (a) Checking for Covariate Balance (TABLE 1)

Before proceeding with any causal analysis, we assess whether the treatment and control groups are balanced in terms of their pre-treatment characteristics. In an ideal randomized experiment, we would expect that the distribution of covariates across these groups is similar, meaning that any difference in outcomes can be attributed solely to the effect of the training program. To evaluate this, we examine differences in the means of several key covariates: age, education, race (Black), ethnicity (Hispanic), whether an individual lacks a high school degree (nodegree), and pre-treatment earnings (re74 and re75).

The results reveal some unexpected imbalances. In particular, there is a notable and statistically significant difference in the racial and educational composition of the two groups. Specifically, there is a lower proportion of hispanic individuals (significant at 90% level) and individuals without a high school degree (this last coefficient is significant also at the 99% level). Differences in age,education and pre-treatment earnings are relatively small and not statistically significant, suggesting that, on average, individuals in both groups had similar earnings histories before participating in the training program. However, the imbalance in demographic characteristics raises concerns about possible selection bias, meaning that the treatment group may not be fully comparable to the control group. While same variability is to be expected, only 71.42% of the variables are balanced in the sample, which is below the 80% expected and reccomended threshold. This may raise problems of comparability. 
      
If participation in the program was not entirely random, differences in outcomes could be partially driven by these pre-existing differences rather than the training itself.

This suggests that controlling for these imbalanced variables in subsequent regression analyses will be important to isolate the effect of training from other potential confounding factors.*/
*--------------------------------------*

*--------------------------------------*
*------------Question 1.b--------------*
reg re78 train , vce(rob)
matrix table = r(table)
matrix list table
scalar train_coef = table[1,1]
scalar list train_coef
scalar train_se = table[2,1]
scalar list train_se

/*(b) Estimating the Effect of Job Training on Earnings (re78)

We estimate the impact of the training program on post-treatment earnings in 1978 (measured in 1982’s US$1000) by running a simple Ordinary Least Squares (OLS) regression where the dependent variable is re78, and the key explanatory variable is train, a binary indicator of whether an individual participated in the job training program. The regression equation is: re78=β0+β1⋅train+ϵ

The estimated coefficient for train is 1.79, meaning that, on average, individuals who received job training earned approximately $1,794 more in 1978 than those who did not participate. The estimate is statistically significant at the 1% level, with a p-value of 0.008, indicating strong evidence that the program had a positive effect on earnings. The standard error of the estimate is 0.67, which suggests a reasonable level of precision, and is robust to heteroskedasticity.

This result aligns with economic intuition: a well-designed training program should equip participants with skills that improve their employability and earnings potential. However, given the imbalance observed in pre-treatment characteristics, it is possible that part of this estimated effect is due to differences in characteristics rather than the causal effect of training itself. To address this concern, we next include additional covariates in the regression model to see if the estimated impact of training remains stable.*/
*--------------------------------------*

*--------------------------------------*
*------------Question 1.c--------------*
global x_1 "train"
global x_2 "age educ black hisp"
global x_3 "re74 re75"

reg re78 $x_1 , vce(rob)
count if train==0
local n_ctrl = r(N)
count if train==1
local n_trt = r(N)
estimates store reg1
outreg2 [reg1] using "/Users/ariannadanese/Desktop/Micrometrics/Table_2", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') ctitle (reg 1) replace dta

reg re78 $x_1 $x_2 , vce(rob)
count if train==0
local n_ctrl = r(N)
count if train==1
local n_trt = r(N)
estimates store reg2
outreg2 [reg2] using "/Users/ariannadanese/Desktop/Micrometrics/Table_2", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') ctitle (reg 2) append dta

reg re78 $x_1 $x_2 $x_3 , vce(rob)
count if train==0
local n_ctrl = r(N)
count if train==1
local n_trt = r(N)
estimates store reg3
outreg2 [reg3] using "/Users/ariannadanese/Desktop/Micrometrics/Table_2", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') ctitle (reg 3) append dta


use "/Users/ariannadanese/Desktop/Micrometrics/Table_2_dta"
export excel using "/Users/ariannadanese/Desktop/Micrometrics/Table_2", replace
*--------------------------------------*

*--------------------------------------*
*------------Question 1.d--------------*
use "/Users/ariannadanese/Desktop/Micrometrics/files/jtrain2.dta", replace

reg re78 $x_1 $x_2 $x_3

*technical note: we do not use vce(rob) because the option dfbeta() that we use afterwards is not allowed after robust estimation*

/*(c) Assessing Robustness: The Impact of Adding Control Variables (TABLE 2)

To determine whether our initial estimate is sensitive to omitted variable bias, we extend the regression model by sequentially adding control variables. The first regression includes only train as an explanatory variable. In the second specification, we add age, education, Black, and Hispanic as additional controls to account for demographic differences. Finally, in the third specification, we also include re74 and re75 to control for prior earnings history.

The results show that the estimated effect of training remains relatively stable across all three models. Initially, the coefficient on train is 1.79, but after adding demographic controls, it decreases slightly to 1.69, and when pre-treatment earnings are included, it remains at 1.68. The statistical significance is maintained throughout, with p-values consistently below 0.01. This suggests that, while some selection bias may be present, it does not appear to be driving the estimated effect.

An interesting finding is that education has a positive and statistically significant impact on earnings, meaning that higher levels of education are associated with increased earnings, independent of training. Conversely, being Black has a negative coefficient, which, although not always statistically significant, suggests the presence of racial disparities in earnings.*/

dfbeta, stub(dfbeta)

gen influence_train = dfbeta1

sort(influence_train)


*First we remove the most influential individuals (the ones in which the influence of training was the highest)
gen row_num_opposite = 446 - _n
reg re78 $x_1 $x_2 $x_3 if row_num_opposite != 10 & row_num_opposite != 5 & row_num_opposite != 3, vce(rob)
*Then we remove the least influential individuals (the ones in which the influence of training was the lowest)
gen row_num = _n
reg re78 $x_1 $x_2 $x_3 if row_num != 10 & row_num != 5 & row_num != 3, vce(rob)
*Then we remove the most influential individuals (the ones in which the influence of training was the lowest and highest)
reg re78 $x_1 $x_2 $x_3 if row_num != 10 & row_num != 5 & row_num != 3 & row_num_opposite != 10 & row_num_opposite != 5 & row_num_opposite != 3, vce(rob)


*alternative method

preserve
keep if _n < _N - 3  & _n > 2
reg re78 $x_1 $x_2 $x_3, vce(robust)  
outreg2 using "/Users/ariannadanese/Desktop/Micrometrics/Table_3", ctitle (Removing highest and lowest 3) replace dta
restore 

preserve
keep if _n < _N - 5  & _n > 4
reg re78 $x_1 $x_2 $x_3, vce(robust)   
outreg2 using "/Users/ariannadanese/Desktop/Micrometrics/Table_3", ctitle (Removing highest and lowest 5) append dta
restore

preserve
keep if _n < _N - 10  & _n > 9
reg re78 $x_1 $x_2 $x_3, vce(robust) 
outreg2 using "/Users/ariannadanese/Desktop/Micrometrics/Table_3", ctitle (Removing highest and lowest 10) append dta
restore

use "/Users/ariannadanese/Desktop/Micrometrics/Table_3_dta", replace
export excel using "/Users/ariannadanese/Desktop/Micrometrics/Table_3", replace


/*(d) Sensitivity Analysis: Influence of Outliers on the Training Effect

One concern in regression analysis is whether the estimated coefficients are being disproportionately driven by a small number of highly influential observations. To investigate this, we calculate DFBETAs for train, which measure how much the estimated coefficient changes when each individual observation is removed from the dataset. We then identify the third, fifth, and tenth most influential observations—both the largest positive and negative influences—and re-run the regression excluding these cases.

The results indicate that, while the estimated effect of training fluctuates slightly, it remains positive and statistically significant in all cases. When the most influential observations are removed, the coefficient drops slightly to 1.36, but when the least influential observations are removed, it increases to 1.85. Finally, when both sets of extreme observations are excluded, the estimate stabilizes around 1.53, still significantly different from zero.

These results suggest that no single individual or small group of individuals is disproportionately driving the estimated impact of training. The findings remain robust even after removing influential cases, reinforcing the conclusion that the training program had a genuine positive effect on earnings.*/

*--------------------------------------*

*----------------------------------------------------------------*
**************************---QUESTION 2---************************
*----------------------------------------------------------------*
use "/Users/ariannadanese/Desktop/Micrometrics/files/jtrain3.dta", replace
*--------------------------------------*
*------------Question 2.a--------------*
matrix balcheck1=(.,.,.,.,.,.)

local i=1
foreach var of varlist age educ black hisp re74 re75 {

	qui ttest `var', by(train) unequal 
	
	matrix balcheck1[`i',1]=r(mu_2)
	matrix balcheck1[`i',2]=r(sd_2) 
	matrix balcheck1[`i',3]=r(mu_1)
	matrix balcheck1[`i',4]=r(sd_1) 
	matrix balcheck1[`i',5]= r(mu_1)-r(mu_2)
	matrix balcheck1[`i',6]=r(se) 
	matrix list balcheck1
		local i=`i'+1
	if `i'<=7 matrix balcheck1=(balcheck1 \ .,.,.,.,.,.) 
	
}


matrix rownames balcheck1= "Age" "Education" "Black" "Hispanic" "Real Earn 74" "Real Earn 75"
matrix colnames balcheck1= "Mean Trt (1)" "StDev Trt" "Mean Ctrl (2)" "StDev Ctrl" "(1)-(2)" "StDev (1)-(2)"


local rows = rowsof(balcheck1)
local cols = colsof(balcheck1)

forval i = 1/`rows' {
    forval j = 1/`cols' {
        matrix balcheck1[`i',`j'] = round(balcheck1[`i',`j'], 0.001)
    }
}

	
	matrix list balcheck1, f(%9.3f) title("Balance check 1")
	cd "/Users/ariannadanese/Desktop/Micrometrics/"
	save balcheck1.dta, replace
	use balcheck.dta, clear
	matrix balcheck_final = balcheck, balcheck1
	save balcheck_final, replace
	use balcheck_final, clear
	putexcel set "/Users/ariannadanese/Desktop/Micrometrics/Table_1.xlsx", replace
	
	putexcel A1=matrix(balcheck_final), names nformat(number_d2)
	putexcel (A2:A8), overwr bold border(right thick) 
	putexcel (B1:G1), overwr bold border(bottom thick)

/*
(a) Balance Check in jtrain3
We replicate the balance check procedure from jtrain2, but this time using a different dataset, jtrain3. In jtrain3, the same 185 treated individuals present in jtrain2 are used as the treatment group, while the control group is not the experimental one but a group of 2,490 men under 55 years who are not retired. Another difference is that nodegree does not appear as a covariate. 

The results reveal that the differences between treatment and control groups are even more pronounced than in jtrain2. 

The most striking imbalances appear in age (there is a substantial age difference of 9 years), education, and pre-treatment earnings (re74 and re75). The treatment group is significantly younger, less educated, and had substantially lower earnings before the intervention. 

We now turn to the analysis of statistical significance. Since we have the difference in means and the standard error of this difference, we can compute a t-statistic:
t=\frac{(1)-(2)}{StDev(1)-(2)}
The thumb rule for assessing significance is that: if this ratio is higher than 1.96, then the the difference is statistically significant at the 5% level.
All differences are statistically significant at the 5% level, except for the "Hispanic" variable.

Therefore, we can conclude that the treatment and control groups in jtrain3 are not well balanced. Key variables like age, education, race, and past earnings show large and statistically significant differences, which suggests potential issues with randomization. This imbalance could introduce bias in estimating treatment effects*/
*--------------------------------------*

*--------------------------------------*
*------------Question 2.b--------------*
use "/Users/ariannadanese/Desktop/Micrometrics/files/jtrain3.dta", replace	
gen treated=.
    set seed 88888
    gen random=uniform()
    sort random
	egen random_order=rank(random)
	qui sum random 
	gen N =r(N)
	replace treated=0 if random_order<=(N/2)
	replace treated=1 if random_order>(N/2) & random_order<=N
*--------------------------------------*

*--------------------------------------*
*------------Question 2.c--------------*
ssc install randtreat
randtreat, generate (treated_2) misfits(missing)
pwcorr treated treated_2, sig 
pwcorr treated treated_2, sig star(.05)

save jtrain4

/*(b) and (c) Simulating a Randomized Treatment Assignment

To further explore the role of randomness in treatment assignment, we create a fake treatment variable by randomly allocating half of the sample to treatment and the other half to control. We do this twice: first by manually assigning observations to groups, and then using the randtreat command to ensure an unbiased allocation.

When using randtreat, we see that the assignment produces just 1 misfit (Misfits arise when the size of the sample is not a  multiple of the number of treatments to be allocated. In this case, as the number of observations is not even and we have two groups, treatment and control, there will be one observation that can't be attributed among the two groups). We decide to deal with this misfit by using the misfits(missing) option as the presence of just one misfit is not worrying. 

After creating these new treatment groups, we check whether they are correlated with the original treatment variable. The correlation coefficient is small and is not statistically significant, confirming that our simulated treatment assignment is indeed independent of the original non-random assignment.*/
*--------------------------------------*

*--------------------------------------*
*------------Question 2.d--------------*
matrix balcheck2=(.,.,.,.,.,.)

local i=1
foreach var of varlist age educ black hisp re74 re75 {

	qui ttest `var', by(treated) unequal 
	
	matrix balcheck2[`i',1]=r(mu_2)
	matrix balcheck2[`i',2]=r(sd_2) 
	matrix balcheck2[`i',3]=r(mu_1)
	matrix balcheck2[`i',4]=r(sd_1) 
	matrix balcheck2[`i',5]= r(mu_1)-r(mu_2)
	matrix balcheck2[`i',6]=r(se) 
	matrix list balcheck2
		local i=`i'+1
	if `i'<=7 matrix balcheck2=(balcheck2 \ .,.,.,.,.,.) 
	
}


matrix rownames balcheck2= "Age" "Education" "Black" "Hispanic" "Real Earn 74" "Real Earn 75"
matrix colnames balcheck2= "Mean Trt (1)" "StDev Trt" "Mean Ctrl (2)" "StDev Ctrl" "(1)-(2)" "StDev (1)-(2)"


local rows = rowsof(balcheck2)
local cols = colsof(balcheck2)

forval i = 1/`rows' {
    forval j = 1/`cols' {
        matrix balcheck2[`i',`j'] = round(balcheck2[`i',`j'], 0.001)
    }
}

	
	matrix list balcheck2, f(%9.3f) title("Balance check 1")
	cd "/Users/ariannadanese/Desktop/Micrometrics/"
	save balcheck2.dta, replace
	use balcheck_final.dta, clear
	matrix balcheck_final2 = balcheck_final, balcheck2
	save balcheck_final2, replace
	use balcheck_final2, clear
	putexcel set "/Users/ariannadanese/Desktop/Micrometrics/Table_1.xlsx", replace
	
	putexcel A1=matrix(balcheck_final2), names nformat(number_d2)
	putexcel (A2:A8), overwr bold border(right thick) 
	putexcel (B1:G1), overwr bold border(bottom thick)

/*(d) Balance Check with Fake Treatment Assignment

Finally, we perform another balance check using the newly created randomly assigned treatment variable. The results show that, as expected, the groups are now almost perfectly balanced across all covariates. Differences in means are close to zero, and none are statistically significant. 

This confirms that proper randomization has taken place and now, under adequate randomization, the concerns about selection bias disappear. As the treatment and control groups are now well balanced, any estimated treatment effects will be not/less biased by pre-existing differences between groups.
 */
*--------------------------------------*

*--------------------------------------*
*------------Question 2.e--------------*
use jtrain4.dta, clear
global x_1 "treated"
global x_2 "age educ black hisp"
global x_3 "re74 re75"

reg re78 $x_1 , vce(rob)
count if treated==0
local n_ctrl = r(N)
count if treated==1
local n_trt = r(N)
estimates store reg1
outreg2 [reg1] using "/Users/ariannadanese/Desktop/Micrometrics/Table_2", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') ctitle (Randomised Treatment 1) append dta

reg re78 $x_1 $x_2 , vce(rob)
count if treated==0
local n_ctrl = r(N)
count if treated==1
local n_trt = r(N)
estimates store reg2
outreg2 [reg2] using "/Users/ariannadanese/Desktop/Micrometrics/Table_2", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') ctitle (Randomised Treatment 2) append dta

reg re78 $x_1 $x_2 $x_3 , vce(rob)
count if treated==0
local n_ctrl = r(N)
count if treated==1
local n_trt = r(N)
estimates store reg3
outreg2 [reg3] using "/Users/ariannadanese/Desktop/Micrometrics/Table_2", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') ctitle (Randomised Treatment 3) append dta

use "/Users/ariannadanese/Desktop/Micrometrics/Table_2_dta"
export excel using "/Users/ariannadanese/Desktop/Micrometrics/Table_2", replace


/* We find that the coefficient for "treated" is not significant at any level and remains not significant before and after the the introduction of the  control variables. 

This lack of significance is expected because the for "treated" in question 2.b we randomly assigned half of the individuals to a fake treatment and the other half to a fake control group, however no treatment actually occurred for all those people, only 185 observations in our dataset actually went through the job training program. If we were to find a treatment effect for 1338 individuals assigned to a fake treatment group (i.e. no treatment has actually taken place) then it would be worrying and we should double check what is happening.

As we add the covariates, some of them become significant and become useful in explaining the outcome variable real earnings in 1978. In particular age and education are highly significant, while black and hispanic demographic variables seem to have little explanatory power. 

Interestingly, in the last specification we see how previous earnings actually have explanatoy power for earnings in 1978, something which was not observed in the jtrain2 results in question 1 */
*--------------------------------------*

*--------------------------------------*
*------------Question 2.f--------------*
use jtrain4.dta
global x_1 "train"
global x_2 "age educ black hisp"
global x_3 "re74 re75"

reg re78 $x_1 , vce(rob)
count if train==0
local n_ctrl = r(N)
count if train==1
local n_trt = r(N)
estimates store reg1
outreg2 [reg1] using "/Users/ariannadanese/Desktop/Micrometrics/Table_2", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') ctitle (Observational Control 1) append dta

reg re78 $x_1 $x_2 , vce(rob)
count if train==0
local n_ctrl = r(N)
count if train==1
local n_trt = r(N)
estimates store reg2
outreg2 [reg2] using "/Users/ariannadanese/Desktop/Micrometrics/Table_2", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') ctitle (Observational Control 2) append dta

reg re78 $x_1 $x_2 $x_3 , vce(rob)
count if train==0
local n_ctrl = r(N)
count if train==1
local n_trt = r(N)
estimates store reg3
outreg2 [reg3] using "/Users/ariannadanese/Desktop/Micrometrics/Table_2", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') ctitle (Observational Control 3) append dta

use "/Users/ariannadanese/Desktop/Micrometrics/Table_2_dta"
export excel using "/Users/ariannadanese/Desktop/Micrometrics/Table_2", replace
/*We observe a strong treatment effect when we run the first regression, that is when we regress earnings in 78 on the actual treatment variable (train) which indicates whether the individual actually participated in the NSW training group. The treatment effect is highly significant, just like the one observed in columns (1) however it is of much bigger magnitude and of different sign: in column (1) the treatment effect is small and positive while the coefficient in column (7) suggests that participating in the NSW training program had a large and negative effect on real earnings in 1978.

By adding the first set of demographic controls, that is the variables age educ black hisp, we see that the magnitude of the train coefficient diminishes. All the controls added, except for hispanic, have a significant power in explaining the outcome variable. We find the same negative and significant effect of being african-american on earnings and the same positive effect of education of earnings. Age, on the other hand, has explanatory power only when considering the dataset made of 2,675 individuals.

Finally, moving onto the last specification, where we include also previous earnings in our regression, we see how all the explanatory power of the treatment allocation vanishes as the coefficient becomes extemely small and absolutely not significant. 
The coefficients on the demographic controls diminish as well and (a) in the case of age it changes sign, (b) in the case of black, it loses significance and (c) in the case of hispanic, it gains a significance star, becoming therefore significant at the 90% level.

The coefficients on earnings in 1974 and 1975 are highly significant and positive. They seem to suggest that previous earnings have great explanatory power when it comes to predicting earnings in 1978

These results and the differences between the jtrain2 results and the new results are not unexpected: the jtrain2 dataset reports results of an experimental setting, whereas the jtrain3 dataset contrasts the 185 individuals on the NSW program with 2490 individuals who are not the experimental control group.
*/
*--------------------------------------*

*----------------------------------------------------------------*
**************************---QUESTION 3---************************
*----------------------------------------------------------------*
*--------------------------------------*
*------------Question 3.a--------------*
ssc install lassopack
use "/Users/ariannadanese/Desktop/Micrometrics/files/jtrain2.dta", replace

rlasso re78 age educ black hisp re74 re75

display e(selected)
display e(lambda)
ereturn list

*The default rlasso procedure heavily penalizes the independent variables and ends up selecting only the constant. This excessive penalization prevents us from running the subsequent OLS regression because the model lacks sufficient predictors. Given the issue, we have two main routes to proceed: (1) manually adjust the rlasso parameters by fine-tuning penalty settings to avoid over-shrinking the predictors (2) switch to a linear lasso regression (using 'lasso linear' instead of the data-driven 'rlasso')

** FIRST METHOD: Fine-tuning the rlasso parameters
* Initially, the default rlasso excessively penalized our predictors, leaving only the constant in the model. To address this, after an ispection of the 'rlasso' documentation, we made several adjustments. First of all, we decided to remove the constant: using the 'noconstant' option to prevent the constant from absorbing too much of the penalty. Additionally, we standardized the data through the 'prestd' option, ensuring that all variables were on the same scale. Since penalization was too heavy, we adjusted the penalty structure, indeed, by adding lalternative (which changes the lambda function's form) and setting gamma(0.05), we moderated the penalty's aggressiveness. We tuned the penalty strength in a consistent way between the first iteration and the subsequent ones, lowering both c and c0 from the default 1.1 to 0.9. Lastly, we ensured the control for heteroskedastic errors. Comparing both the fully tuned and simplified versions confirmed that the over-penalization was primarily driven by the constant. With these adjustments, the model now appropriately selects age and education as significant predictors of real earnings in 1978.

rlasso re78 age educ black hisp re74 re75, noconstant robust prestd lassopsi lalternative gamma(0.05) c(0.9) c0(0.9) supscore seed(12345)  displayallpostall ols verbose vverbose

rlasso re78 age educ black hisp re74 re75, sqrt noconstant robust prestd lassopsi lalternative gamma(0.05) c(0.9) c0(0.9) supscore seed(12345)  displayall postall ols verbose vverbose
rlasso re78 age educ black hisp re74 re75, noconstant robust prestd lassopsi supscore seed(12345)  displayall postall ols verbose vverbose

rlasso re78 age educ black hisp re74 re75, sqrt noconstant robust prestd lassopsi lalternative gamma(0.05) c(0.9) c0(0.9) supscore seed(12345)  displayall postall ols verbose vverbose

rlasso re78 age educ black hisp re74 re75, noconstant 

** SECOND METHOD: Using lasso linear
lasso linear re78 age educ black hisp re74 re75
lassocoef

**Lasso linear without fine tuning is too 'gentle' and does not shrink any coefficient to 0. Therefore, we use the variables selected by the rlasso after removing the constant and we perform an OLS regression of re78 on the treatment status, age, and education level.

reg re78 train age educ

**FINAL COMMENT: From the results, it appears that in this OLS regression only the treatment status and education are significant at 5% level. However, this result should be interpreted with caution as the variables were selected through a data-driven approach by the lasso regularization procedure. Traditional OLS inference (such as t-tests and p-values) assumes that the model is specified correctly, and all variables are fixed in advance. Since Lasso selects the variables based on the data, this violates the assumption of fixed predictors in the OLS regression, making the standard errors, p-values, and confidence intervals potentially misleading.
*--------------------------------------*

*--------------------------------------*
*------------Question 3.b--------------*
*------------Question 3.b1--------------*
ssc install pystacked
ssc install pdslasso
ssc install ivreg2
ssc install ranktest
pdslasso re78 train (age educ black hisp re74 re75)
rlasso train age educ black hisp re74 re75
rlasso re78 age educ black hisp re74 re75

**COMMENT
*The results confirm a similar penalization to the one we analyzed in point 3a. In fact, also in this case, through the automatic double selection procedure, the constant is attributed all the explanatory power, and all variables are excluded from the model in both parts of the double selection (for re78 and train). This selection is surprising and might indicate that the Lasso procedure did not find any significant relationship between the predictors and the outcome or treatment. This could be due to high regularization (which can shrink all coefficients) or other factors, such as multicollinearity or weak relationships between the predictors and the outcome. As for the manual double selection procedure through 'rlasso', we cansee that in both regressions (train and re78), only the constant is selected, which means that the Lasso procedure did not select any of the predictors as important for explaining either train or re78. Consequently, in the final OLS regression, only the variable 'train' is included, and its coefficient is 1.794343. This suggests that, based on the double selection procedure, the effect of train on re78 is significant. However, since no other variables were selected, the model does not control for any other covariates like age, educ, or black, etc.

*------------Question 3.b2--------------*
sum age, detail

levelsof age, local(agevals) // Get unique values of age
foreach a of local agevals {
    gen age_`a' = (age == `a')
}

sum educ, detail

levelsof educ, local(educvals) // Get unique values of age
foreach e of local educvals {
    gen educ_`e' = (educ == `e')
}

pdslasso re78 train (black hisp re74 re75 i.age_* i.educ_*)

rlasso train black hisp re74 re75 i.age_* i.educ_*

rlasso re78 black hisp re74 re75 i.age_* i.educ_*

reg re78 train educ_14

**COMMENT
*With respect to the Lasso regularization performed in point 3a, this double selection procedures ensures a more accurate shrinkage, which leads to a lower bias in the OLS results. The main difference is that the double selection does not consider 'age' as a predictor of the outcome variable, however, we should look at these results carefully. In both pdslasso and rlasso, the variable i.educ_14 is selected for predicting re78. No other predictors (like age, black, hisp, re74, re75) were selected in pdslasso and rlasso for the outcome variable, which is impacted only by the constant, educ_14, and the treatment status. To improve the analysis and achieve better results, it might be asvisable to fine-tune parameters or remove the constant, as the excessive shrinkage and selection of just one category for the years of education indicate. Lastly, as for 'train', both pdslasso and rlasso do not find any significant predictor, this may indicate that the treatment and control groups are balanced in terms of age, education, ethnicity and earnings.
*--------------------------------------*


*----------------------------------------------------------------*
**************************---QUESTION 4---************************
*----------------------------------------------------------------*
*--------------------------------------*
*------------Question 4.a--------------*
*guardare sezione 4.2*

*--------------------------------------*

*--------------------------------------*
*------------Question 4.b--------------*
**NEYMAN INFERENCE: Differently from classical inference, Fisher's randomization inference provides a way to assess whether an observed realization of a statistic, such as treatment effect estimate, is likely to be observed by chance and, thus, if it is statistically significant. Neyman’s inference produces the distribution of a test statistic under a specified null hypothesis, enabling researchers to evaluate whether the observed realization of the statistic is "extreme" and thus whether the null hypothesis should be rejected. This approach does not depend on assumptions about sample size, the accuracy of the data-generating model, or the distribution of error terms. Unlike asymptotic inference, which assumes that each observation is drawn from a probability distribution, Neyman’s inference treats the set of study subjects as fixed and considers only the treatment assignment as a random variable.

use "/Users/ariannadanese/Desktop/Micrometrics/files/jtrain2.dta", replace
*tabstat re78, by(train) stats(mean sd)
*reg re78 train
*ssc install ritest

ritest train _b[train]: ///
	reg re78 train

* running same test with 10,000 permutations
ritest train _b[train], reps(10000): ///
	reg re78 train


ritest train _b[train]/_se[train]: ///
	reg re78 train


ritest train _b[train]/_se[train], reps(2000): ///
	reg re78 train
	
**By conducting multiple permutations of the treatment assignment with varying numbers of replications (10,000, 15,000, and 20,000), we obtained p-values ranging between 0.0041 and 0.0061. These results closely align with the p-value of 0.0044 reported by Athey and Imbens, leading us to the same conclusion: the null hypothesis of no treatment effect on real earnings should be rejected. In his paper, Simon Heß provides numerous examples highlighting the differences between using permute and ritest as randomization commands. Notably, ritest offers greater flexibility in specifying complex resampling structures compared to permute. This flexibility can influence the precision of p-value estimates. Therefore, without access to the specific code and options used by Athey and Imbens in their randomization inference, and given the arbitrary nature of the randomized assignment, slight variations in p-values may arise in our analysis.


**

*--------------------------------------*
*------------Question 4.c--------------*
*In LaLonde (1986), the randomization procedure for the NSW program was designed so that eligible  applicants were designed to treatment and control groups randomly. However, this randomization happened at the city level, as the program was implemented across ten different sites in the United States. These sites may differ from each other along some socio-economic variables. Moreover, each site operated independently, and while the sites followed the overall goal of the program, the type of work experiences and work environments they provided varied across and within cities.
*Athey and Imbens (2017) seek to apply Fisherian inference to the NSW data: they want to test the sharp null hypothesis that the treatment had no effect. They reassign the treatment randomly across the sample, while keeping the total number of treated and control units at 185 and 240 respectively. However, they ignore the randomization at the city level that happened when the NSW program was taking place.  Furthermore, LaLonde (1986) explicitly states that treatment experiences varied both across and within sites, meaning that treatment effects may not have been exactly homogeneous, which goes against the assumptions necessary for Fisher’s sharp null hypothesis. Both of these oversights could lead to potential bias in the measurement of treatment effects and the calculation of p-values.
*--------------------------------------*

*--------------------------------------*
*------------Question 4.d--------------*
**--SUBPOINT 1--**
In STATA, the standard method for calculating heteroscedasticity-consistent standard errors (HC1) estimates the asymptotic variance of OLS coefficients by replacing the variance-covariance matrix of the error term, σ^2, with the diagonal matrix of squared residuals, S^2. The result is then multiplied by a correction factor, n−k, which accounts for degrees of freedom in the residual vector. HC1 ensures consistency by using S^2 as a consistent estimator for the error term's variance, σ^2.
Alternatively, let X be the matrix of covariates and define the projection matrix as:H=X(X′X)^(−1)X′where h(ii) represents the ith diagonal entry of H. The HC3 method refines standard error estimation by adjusting each squared residual, 
S^2(e(i)^2), by the factor 1/(1−h(ii))^2. This adjustment reduces the influence of outliers and enhances standard error estimation, particularly in smaller samples (n ≤ 250) and in the presence of pronounced heteroskedasticity.

**--SUBPOINT 2--**
*HC1
use "/Users/ariannadanese/Desktop/Micrometrics/files/jtrain2.dta", replace
global x_1 "train"
global x_2 "age educ black hisp"
global x_3 "re74 re75"

reg re78 $x_1 , vce(rob)
count if train==0
local n_ctrl = r(N)
count if train==1
local n_trt = r(N)
estimates store reg1
outreg2 [reg1] using "/Users/ariannadanese/Desktop/Micrometrics/Table_4", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') ctitle (SE hc1 1) replace dta

reg re78 $x_1 $x_2 , vce(rob)
count if train==0
local n_ctrl = r(N)
count if train==1
local n_trt = r(N)
estimates store reg2
outreg2 [reg2] using "/Users/ariannadanese/Desktop/Micrometrics/Table_4", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') ctitle (SE hc1 2) append dta

reg re78 $x_1 $x_2 $x_3 , vce(rob)
count if train==0
local n_ctrl = r(N)
count if train==1
local n_trt = r(N)
estimates store reg3
outreg2 [reg3] using "/Users/ariannadanese/Desktop/Micrometrics/Table_4", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') ctitle (SE hc1 3) append dta

use "/Users/ariannadanese/Desktop/Micrometrics/Table_4_dta"
export excel using "/Users/ariannadanese/Desktop/Micrometrics/Table_4", replace


use "/Users/ariannadanese/Desktop/Micrometrics/files/jtrain2.dta", replace

reg re78 $x_1 $x_2 $x_3

dfbeta, stub(dfbeta)

gen influence_train = dfbeta1

sort(influence_train)

preserve
keep if _n < _N - 3  & _n > 2
reg re78 $x_1 $x_2 $x_3, vce(robust)  
outreg2 using "/Users/ariannadanese/Desktop/Micrometrics/Table_5", ctitle (HC1 Removing highest and lowest 3) append dta
restore 

preserve
keep if _n < _N - 5  & _n > 4
reg re78 $x_1 $x_2 $x_3, vce(robust)   
outreg2 using "/Users/ariannadanese/Desktop/Micrometrics/Table_5", ctitle (HC1 Removing highest and lowest 5) append dta
restore

preserve
keep if _n < _N - 10  & _n > 9
reg re78 $x_1 $x_2 $x_3, vce(robust) 
outreg2 using "/Users/ariannadanese/Desktop/Micrometrics/Table_5", ctitle (HC1 Removing highest and lowest 10) append dta
restore

use "/Users/ariannadanese/Desktop/Micrometrics/Table_5_dta", replace
export excel using "/Users/ariannadanese/Desktop/Micrometrics/Table_5", replace


*HC3
use "/Users/ariannadanese/Desktop/Micrometrics/files/jtrain2.dta", replace
global x_1 "train"
global x_2 "age educ black hisp"
global x_3 "re74 re75"

reg re78 $x_1, vce(hc3)
count if train==0
local n_ctrl = r(N)
count if train==1
local n_trt = r(N)
estimates store reg1
outreg2 [reg1] using "/Users/ariannadanese/Desktop/Micrometrics/Table_4", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') ctitle (SE hc3 1) append dta

reg re78 $x_1 $x_2, vce(hc3)
count if train==0
local n_ctrl = r(N)
count if train==1
local n_trt = r(N)
estimates store reg2
outreg2 [reg2] using "/Users/ariannadanese/Desktop/Micrometrics/Table_4", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') ctitle (SE hc3 2) append dta

reg re78 $x_1 $x_2 $x_3, vce(hc3)
count if train==0
local n_ctrl = r(N)
count if train==1
local n_trt = r(N)
estimates store reg3
outreg2 [reg3] using "/Users/ariannadanese/Desktop/Micrometrics/Table_4", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') ctitle (SE hc3 3) append dta

use "/Users/ariannadanese/Desktop/Micrometrics/Table_4_dta"
export excel using "/Users/ariannadanese/Desktop/Micrometrics/Table_4", replace


use "/Users/ariannadanese/Desktop/Micrometrics/files/jtrain2.dta", replace

reg re78 $x_1 $x_2 $x_3

dfbeta, stub(dfbeta)

gen influence_train = dfbeta1

sort(influence_train)

*First we remove the most influential individuals (the ones in which the influence of training was the highest)
gen row_num_opposite = 446 - _n
reg re78 $x_1 $x_2 $x_3 if row_num_opposite != 10 & row_num_opposite != 5 & row_num_opposite != 3, vce(hc3)
*Then we remove the least influential individuals (the ones in which the influence of training was the lowest)
gen row_num = _n
reg re78 $x_1 $x_2 $x_3 if row_num != 10 & row_num != 5 & row_num != 3, vce(hc3)
*Then we remove the most influential individuals (the ones in which the influence of training was the lowest and highest)
reg re78 $x_1 $x_2 $x_3 if row_num != 10 & row_num != 5 & row_num != 3 & row_num_opposite != 10 & row_num_opposite != 5 & row_num_opposite != 3, vce(hc3)

*alternative method
preserve
keep if _n < _N - 3  & _n > 2
reg re78 $x_1 $x_2 $x_3, vce(hc3)  
outreg2 using "/Users/ariannadanese/Desktop/Micrometrics/Table_5", ctitle (HC3 Removing highest and lowest 3) append dta
restore 

preserve
keep if _n < _N - 5  & _n > 4
reg re78 $x_1 $x_2 $x_3, vce(hc3)   
outreg2 using "/Users/ariannadanese/Desktop/Micrometrics/Table_5", ctitle (HC3 Removing highest and lowest 5) append dta
restore

preserve
keep if _n < _N - 10  & _n > 9
reg re78 $x_1 $x_2 $x_3, vce(hc3) 
outreg2 using "/Users/ariannadanese/Desktop/Micrometrics/Table_5", ctitle (HC3 Removing highest and lowest 10) append dta
restore

use "/Users/ariannadanese/Desktop/Micrometrics/Table_5_dta", replace
export excel using "/Users/ariannadanese/Desktop/Micrometrics/Table_5", replace



**--SUBPOINT 3--**

*bootstrapping
*/Bootstrapping is a non-parametric method for estimating standard errors by resampling from a given sample. It assumes that the sample accurately represents the population in terms of the distribution of the variable of interest. 
By repeatedly drawing resamples on which the statistic of interest is computed, bootstrapping generates an empirical approximation of the sampling distribution. The variance and standard deviation of this distribution provide a reliable estimate of the statistic’s variability. 
If the sample closely reflects the population, the standard deviation of the bootstrap distribution serves as a valid estimate of the standard error.*/

use "/Users/ariannadanese/Desktop/Micrometrics/files/jtrain2.dta", replace
global x_1 "train"
global x_2 "age educ black hisp"
global x_3 "re74 re75"

reg re78 $x_1, vce(bootstrap, reps(1000))
count if train==0
local n_ctrl = r(N)
count if train==1
local n_trt = r(N)
estimates store reg1
outreg2 [reg1] using "/Users/ariannadanese/Desktop/Micrometrics/Table_4", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') ctitle (SE bootstrap 1) append dta

reg re78 $x_1 $x_2, vce(bootstrap, reps(1000))
count if train==0
local n_ctrl = r(N)
count if train==1
local n_trt = r(N)
estimates store reg2
outreg2 [reg2] using "/Users/ariannadanese/Desktop/Micrometrics/Table_4", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') ctitle (SE bootstrap 2) append dta

reg re78 $x_1 $x_2 $x_3, vce(bootstrap, reps(1000))
count if train==0
local n_ctrl = r(N)
count if train==1
local n_trt = r(N)
estimates store reg3
outreg2 [reg3] using "/Users/ariannadanese/Desktop/Micrometrics/Table_4", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') (SE bootstrap 3) append dta


use "/Users/ariannadanese/Desktop/Micrometrics/Table_4_dta"
export excel using "/Users/ariannadanese/Desktop/Micrometrics/Table_4", replace

use "/Users/ariannadanese/Desktop/Micrometrics/files/jtrain2.dta", replace

reg re78 $x_1 $x_2 $x_3

dfbeta, stub(dfbeta)

gen influence_train = dfbeta1

sort(influence_train)


*First we remove the most influential individuals (the ones in which the influence of training was the highest)
gen row_num_opposite = 446 - _n
reg re78 $x_1 $x_2 $x_3 if row_num_opposite != 10 & row_num_opposite != 5 & row_num_opposite != 3, vce(bootstrap)
*Then we remove the least influential individuals (the ones in which the influence of training was the lowest)
gen row_num = _n
reg re78 $x_1 $x_2 $x_3 if row_num != 10 & row_num != 5 & row_num != 3, vce(bootstrap)
*Then we remove the most influential individuals (the ones in which the influence of training was the lowest and highest)
reg re78 $x_1 $x_2 $x_3 if row_num != 10 & row_num != 5 & row_num != 3 & row_num_opposite != 10 & row_num_opposite != 5 & row_num_opposite != 3, vce(bootstrap)

*alternative method

preserve
keep if _n < _N - 3  & _n > 2
reg re78 $x_1 $x_2 $x_3, vce(bootstrap, reps(1000))  
outreg2 using "/Users/ariannadanese/Desktop/Micrometrics/Table_5", ctitle (BOOTSTRAP Removing highest and lowest 3) append dta
restore 

preserve
keep if _n < _N - 5  & _n > 4
reg re78 $x_1 $x_2 $x_3, vce(bootstrap, reps(1000))   
outreg2 using "/Users/ariannadanese/Desktop/Micrometrics/Table_5", ctitle (BOOTSTRAP Removing highest and lowest 5) append dta
restore

preserve
keep if _n < _N - 10  & _n > 9
reg re78 $x_1 $x_2 $x_3, vce(bootstrap, reps(1000)) 
outreg2 using "/Users/ariannadanese/Desktop/Micrometrics/Table_5", ctitle (BOOTSTRAP Removing highest and lowest 10) append dta
restore

use "/Users/ariannadanese/Desktop/Micrometrics/Table_5_dta", replace
export excel using "/Users/ariannadanese/Desktop/Micrometrics/Table_5", replace




**--SUBPOINT 4--**

*--------------------------------------*

