********************************************
* Assignment 4
* Alfonso Rojas-Alvarez
* Research Design, by Dr. Paul Von Hippel
* Fall 2017
********************************************

clear all

log close _all
log using "/Users/Alfonso/Google Drive/UT/Fall 2017/RD/assignment4.log", replace
set seed 3347

use "/Users/Alfonso/Google Drive/UT/Fall 2017/RD/STAR_students.dta"

**************
* Question 1
**************

* a.

reg gktreadss ib2.gkclasstype

* b. 

reg gktreadss ib2.gkclasstype i.gkschid

* c.

xtset gkschid
xtreg gktreadss ib2.gkclasstype, fe

**************
* Question 2
**************

* a. 

regress gktreadss gktyears ib2.gkclasstype

* b.

xtreg gktreadss gktyears ib2.gkclasstype, fe
