********************************************
* Assignment 3
* Alfonso Rojas-Alvarez
* Research Design, by Dr. Paul Von Hippel
* Fall 2017
********************************************

clear all

log close _all
log using "/Users/Alfonso/Google Drive/UT/Fall 2017/RD/assignment3.log", replace
set seed 3347

use "/Users/Alfonso/Google Drive/UT/Fall 2017/RD/STAR_students.dta"

**************
* Question 1
**************

* a.i.

tab gkclasstype
tab gkclasstype, nolabel

***********
* They were assigned to a class about 1/3 of the time, and the classroom type feels sufficiently balanced.
***********

* b.i. 

sum gktreadss

**** The sd is 31.70

* b.ii.

tabstat gktreadss, by(gkclasstype)

* b. iv.

regress gktreadss ib2.gkclasstype

* c. 

regress gktreadss gktyears
