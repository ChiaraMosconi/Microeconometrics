
* QUESTION 1

*ssc install estout, replace
*ssc install randomizr, replace
*ssc install ritest, replace
*ssc install outreg2, replace
cd "/Users/ariannadanese/Desktop/Micrometrics"
use "/Users/ariannadanese/Desktop/Micrometrics/files/jtrain2.dta", replace
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

**# Bookmark #1


* Black, hispanic, and no degree are unbalanced because their difference in standard deviation is statistically significant. We did not expect this because in a ramdomized experiment there should not be systematic differences between treatment and control (selection bias).

reg re78 train, vce(rob)
matrix table = r(table)
matrix list table
scalar train_coef = table[1,1]
scalar list train_coef
scalar train_se = table[2,1]
scalar list train_se

* Being in the treatment increases the real earnings of around 1800 dollars (in 1978)

global x_1 "train"
global x_2 "age educ black hisp"
global x_3 "re74 re75"

reg re78 $x_1
count if train==0
local n_ctrl = r(N)
count if train==1
local n_trt = r(N)
estimates store reg1
outreg2 [reg1] using "/Users/ariannadanese/Desktop/Micrometrics/Table_2", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') append dta

reg re78 $x_1 $x_2
count if train==0
local n_ctrl = r(N)
count if train==1
local n_trt = r(N)
estimates store reg2
outreg2 [reg2] using "/Users/ariannadanese/Desktop/Micrometrics/Table_2", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') append dta

reg re78 $x_1 $x_2 $x_3
count if train==0
local n_ctrl = r(N)
count if train==1
local n_trt = r(N)
estimates store reg3
outreg2 [reg3] using "/Users/ariannadanese/Desktop/Micrometrics/Table_2", addstat("Number Treated",`n_trt', "Number Control",`n_ctrl') append dta


use "/Users/ariannadanese/Desktop/Micrometrics/Table_2_dta"
export excel using "/Users/ariannadanese/Desktop/Micrometrics/Table_2", replace

use "/Users/ariannadanese/Desktop/Micrometrics/files/jtrain2.dta", replace

reg re78 $x_1 $x_2 $x_3
dfbeta, stub(dfbeta)

gen influence_train = dfbeta1

sort(influence_train)
*First we remove the most influential individuals (the ones in which the influence of training was the highest)
gen row_num_opposite = 446 - _n
reg re78 $x_1 $x_2 $x_3 if row_num_opposite != 10 & row_num_opposite != 5 & row_num_opposite != 3
*Then we remove the least influential individuals (the ones in which the influence of training was the lowest)
gen row_num = _n
reg re78 $x_1 $x_2 $x_3 if row_num != 10 & row_num != 5 & row_num != 3
*Then we remove the most influential individuals (the ones in which the influence of training was the lowest and highest)
reg re78 $x_1 $x_2 $x_3 if row_num != 10 & row_num != 5 & row_num != 3 & row_num_opposite != 10 & row_num_opposite != 5 & row_num_opposite != 3

* QUESTION 2a

use "/Users/ariannadanese/Desktop/Micrometrics/files/jtrain3.dta", replace
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
	putexcel set "/Users/ariannadanese/Desktop/Micrometrics/Table_6.xlsx", replace
	
	putexcel A1=matrix(balcheck_final), names nformat(number_d2)
	putexcel (A2:A8), overwr bold border(right thick) 
	putexcel (B1:G1), overwr bold border(bottom thick)
	
use "/Users/ariannadanese/Desktop/Micrometrics/files/jtrain3.dta", replace	
gen treated=.
    set seed 63285
    gen random=uniform()
    sort random
	egen random_order=rank(random)
	qui sum random 
	gen N =r(N)
	replace treated=0 if random_order<=(N/2)
	replace treated=1 if random_order>(N/2) & random_order<=N

ssc install randtreat
help randtreat
