clear all 

cd "/Users/chiaramosconi/Downloads/"
*Question 1.a*
matrix ex_1a =(.,.,.,.,.,.)

local i=1

foreach var of varlist age educ black hisp re74 re75 nodegree {

	qui ttest `var', by(train) unequal 
	
	matrix ex_1a[`i',1]=r(mu_2)
	matrix ex_1a[`i',2]=r(sd_2) 
	matrix ex_1a[`i',3]=r(mu_1)
	matrix ex_1a[`i',4]=r(sd_1) 
	matrix ex_1a[`i',5]= r(mu_1)-r(mu_2)
	matrix ex_1a[`i',6]=r(se) 
	matrix list ex_1a
		local i=`i'+1
	if `i'<=7 matrix ex_1a=(ex_1a \ .,.,.,.,.,.) 
	
}


matrix rownames ex_1a= "Age" "Education" "Black" "Hispanic" "Real Earn 74" "Real Earn 75" "No Degree"
matrix colnames ex_1a= "Mean Trt (1)" "StDev Trt" "Mean Ctrl (2)" "StDev Ctrl" "(1)-(2)" "StDev (1)-(2)"

local rows = rowsof(ex_1a)
local cols = colsof(ex_1a)

*Loop through each element of the matrix and round it to two decimal places
forval i = 1/`rows' {
    forval j = 1/`cols' {
        matrix ex_1a[`i',`j'] = round(ex_1a[`i',`j'], 0.001)
    }
}

	
	matrix list ex_1a, f(%9.3f) title("Balance check")
	
	putexcel set "/Users/chiaramosconi/Downloads/Table_1.xlsx", replace
	
	putexcel A1=matrix(ex_1a), names nformat(number_d2)
	putexcel (A2:A8), overwr bold border(right thick) 
	putexcel (B1:G1), overwr bold border(bottom thick) 

##BALANCED OR UNBALANCED?
*Question 1.b*
reg re78 train, vce(robust)
matrix table= r(table)
matrix list table
scalar train=coeffs[1,1]
scalar list train
*this is the treatment effect of being in the treatment group on real earnings in 1978: we interpret the coefficient as follows being assigned to the treatment group and therefore undergoing the training program increases your earnings by 1790US$*
scalar train_se=coeffs[2,1]
scalar list train_se 


