********************************************
* Assignment 2
* Alfonso Rojas-Alvarez
* Research Design, by Dr. Paul Von Hippel
* Fall 2017
********************************************

clear all

******************
* Question 3
******************

log close _all
log using "/Users/Alfonso/Google Drive/UT/Fall 2017/RD/assignment2", replace
set seed 3347

******************
* Question 3
******************

use "/Users/Alfonso/Google Drive/UT/Fall 2017/RD/STAR_students.dta"

drop if missing(gktreadss)

mean(gktreadss)

egen students_in_school = count(stdntid), by(gkschid)

tab students_in_school
summarize students_in_school

**** The standard deviation is 25.96 students.

tempfile students_with_counts
save `students_with_counts'

******************
* Question 4
******************

* a.

gsample 2, strata(gkschid) wor

svyset, clear
svyset [pweight=students_in_school], strata(gkschid)
svy: mean gktreadss

use `students_with_counts', clear

gsample 2, strata(gkschid) wor

svyset, clear
svyset [pweight=students_in_school], strata(gkschid)
svy: mean gktreadss

* b.

use `students_with_counts', clear

gsample 10, cluster(gkschid) wor
gsample 16, strata(gkschid) wor

svyset, clear
svyset [pweight=students_in_school], psu(gkschid)
svy: mean gktreadss

use `students_with_counts', clear

gsample 10, cluster(gkschid) wor
gsample 16, strata(gkschid) wor

svyset, clear
svyset [pweight=students_in_school], psu(gkschid)
svy: mean gktreadss

log close
