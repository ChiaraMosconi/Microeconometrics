clear all 

cd "/Users/chiaramosconi/Downloads/"
*----------------------------------------------------------------*
 **************************QUESTION 1************************
*----------------------------------------------------------------*

*Question 1.a*
clear all 

cd "/Users/chiaramosconi/Downloads/"
use "/Users/chiaramosconi/Downloads/files 2/jtrain2.dta", replace
*----------------------------------------------------------------*
 **************************QUESTION 1************************
*----------------------------------------------------------------*
*Question 1.a*
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
	cd "/Users/chiaramosconi/Downloads"
	save balcheck.dta, replace
	
	
	putexcel set "/Users/chiaramosconi/Downloads/Table_1.xlsx", replace
	
	putexcel A1=matrix(balcheck), names nformat(number_d2)
	putexcel (A2:A8), overwr bold border(right thick) 
	putexcel (B1:G1), overwr bold border(bottom thick) 

*Three Unbalanced: Black, Hispanic and No Degree - this is not what we would expect since this is supposed to be a Randomized Experiment

##BALANCED OR UNBALANCED?
*Question 1.b*
reg re78 train, vce(robust)
matrix table= r(table)
matrix list table
scalar train=table[1,1]
scalar list train
*this is the treatment effect of being in the treatment group on real earnings in 1978: we interpret the coefficient as follows being assigned to the treatment group and therefore undergoing the training program increases your earnings by 1790US$*
scalar train_se=table[2,1]
scalar list train_se 

*Question 1.c*
global x_1 "train"
global x_2 "age educ black hisp"
global x_3 "re74 re75"

reg re78 $x_1
count if train==0
local n_ctrl = r(N)
count if train==1
local n_trt = r(N)
estimates store reg1
outreg2 [reg1] using "/Users/chiaramosconi/Downloads/Table_2", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') append dta

reg re78 $x_1 $x_2
count if train==0
local n_ctrl = r(N)
count if train==1
local n_trt = r(N)
estimates store reg2
outreg2 [reg2] using "/Users/chiaramosconi/Downloads/Table_2", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') append dta

reg re78 $x_1 $x_2 $x_3
count if train==0
local n_ctrl = r(N)
count if train==1
local n_trt = r(N)
estimates store reg3
outreg2 [reg3] using "/Users/chiaramosconi/Downloads/Table_2", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') append dta

use "/Users/chiaramosconi/Downloads/Table_2_dta"
export excel using "/Users/chiaramosconi/Downloads/Table_2", replace

*Question 1.d*
use "/Users/chiaramosconi/Downloads/files 2/jtrain2.dta", replace
reg re78 train age educ black hisp re74 re75
dfbeta, stub(dfbeta)
gen influence_train=dfbeta1

sort influence_train
hist influence_train 

gen rownum = _n 
sum rownum, d
gen rownum_opposite=446-rownum
sum rownum_opposite, d
reg re78 train age educ black hisp re74 re75
reg re78 train age educ black hisp re74 re75 if rownum_opposite != 3 & rownum_opposite != 5 & rownum_opposite != 10 /*first we remove the most influential individuals*/
reg re78 train age educ black hisp re74 re75 if rownum != 3 & rownum != 5 & rownum != 10
/*then we remove the least influential individuals*/
/*we remove both*/
reg re78 train age educ black hisp re74 re75 if rownum_opposite != 3 & rownum_opposite != 5 &rownum_opposite != 10 & rownum != 3 & rownum != 5 & rownum != 10

*----------------------------------------------------------------*
 **************************QUESTION 2************************
*----------------------------------------------------------------*
*Question 2.a*
use "/Users/chiaramosconi/Downloads/files 2/jtrain3.dta", replace

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
	cd "/Users/chiaramosconi/Downloads/"
	save balcheck1.dta, replace
	use balcheck.dta, clear
	matrix balcheck_final = balcheck, balcheck1
	save balcheck_final, replace
	use balcheck_final, clear
	putexcel set "/Users/chiaramosconi/Downloads/Table_1.xlsx", replace
	
	putexcel A1=matrix(balcheck_final), names nformat(number_d2)
	putexcel (A2:A8), overwr bold border(right thick) 
	putexcel (B1:G1), overwr bold border(bottom thick) 


*Question 2b*
* generate empty variable to fill with 0s and 1s 
gen treated =.

* fix a seed to make realization of your random variables unique *
* generate random numbers and order them *
gen random=uniform() 
set seed 12345
sort random
* create a rank from 1 to N based on the random_order *
egen random_order=rank(random)
* assign the first N/2 obs to treatment and the rest to control * 
qui sum random
gen N =r(N)
replace treat=0 if random_order <=(N/2)
replace treat=1 if random_order >(N/2) & random_order <=N


*Question 2c*
ssc install randtreat
randtreat, generate (treated_2) misfits(missing)
pwcorr treated treated_2, sig 
pwcorr treated treated_2, sig star(.05)
*the correlation coefficient is not significant: *

