********************************************
* Assignment 1
* Alfonso Rojas-Alvarez
* Research Design, by Dr. Paul Von Hippel
* Fall 2017
********************************************

set seed 3347
clear all

******************
* Question 2
******************

log close _all
log using "/Users/Alfonso/Google Drive/UT/Fall 2017/RD/assignment1", replace

******************
* Question 3
******************

use "/Users/Alfonso/Google Drive/UT/Fall 2017/RD/STAR_students.dta"

drop if missing(gkschid)

tempfile STAR_kindergarteners
save `STAR_kindergarteners'

******************
* Question 4
******************

* quietly sum _all

******************
* Question 5
******************

* describe
*** 6325 kids

tab flagsgk
tab flagsg1
tab flagsg2
tab flagsg3

sum gktreadss

**** Population
**** Mean 436.72 , SD 31.70, CI: 435.90 - 437.54

******************
* Question 6
******************

* a. 

**** Sample 1

sample 160, count

* i. 

mean gktreadss

**** Mean 433.9187 , SD 2.366

* ii. 
gen meanN = 436.72
gen meann = 433.9187
gen diffmeans = meanN - meann

**** Difference of means 2.8013

* iii.

**** It falls inside our 95% CI for the population mean previously estimated

* iv.
**** It is significantly different from 440 at the 95% confidence level 

**** b. Sample II

use `STAR_kindergarteners', replace

sample 160, count

mean gktreadss

*** 436.21

gen meann2 = 436.21

*** Sample variation: 436.21 - 433.9187 = 2.2913, it is very similar to the
*** estimated standard error
*** The population mean is covered, it is actually very very similar.

log close
