########################################
# Assignment 2
# Alfonso Rojas-Alvarez
# Research Design, by Paul Von Hippel
# Fall 2017
#######################################

set.seed(3347)
library(ggplot2)
library(foreign)
library(Hmisc)
library(grid)
library(gridExtra)
library(easyGgplot2)
library(splitstackshape)
library(dplyr)
library(plyr)
library(survey)
library(sampling)
rm(list = ls())

######## MODIFY THIS TO YOUR PATH
setwd("/Users/Alfonso/Google Drive/UT/Fall 2017/RD")

##### Load Dataset
students <- read.dta("STAR_students.dta")

#### Load Variable Labels
var.labels <- attr(students, "var.labels")
data.key <- data.frame(var.name=names(students), var.labels)

#### Clear Mising gktreadss
students <- students[!(is.na(students$gktreadss)),]

#### Remind yourself of population means

mu <- mean(students$gktreadss)
mu
sigma <- sd(students$gktreadss)
sigma

#### Generate number of students in each school

agg <- aggregate(data=students, students$stdntid ~ students$gkschid, function(x) length(unique(x)))
names(agg)[2] <- paste("sis")
names(agg)[1] <- paste("gkschid")
combined <- join(students, agg, by="gkschid", type='left', match = 'all')
mean(combined$sis)
sd(combined$sis)

#### Take stratified sample

strat_sample1 <- stratified(combined, c("gkschid"), size = 2) 

#### Create a boxplot just to take a look at them

boxplot(strat_sample1$gktreadss ~ strat_sample1$gkschid, ylab = "Mean Reading Score", xlab = "School ID")

#### Set Survey Design Parameters for Stratified Sample

### First Sample

design_sample1 <- svydesign(id = ~1, strata = ~gkschid, data = strat_sample1, weights= ~sis)

svymean(~gktreadss, design = design_sample1)
confint(svymean(~gktreadss, design = design_sample1))

### Second Sample

strat_sample2 <- stratified(combined, c("gkschid"), size = 2) 

design_sample2 <- svydesign(id = ~1, strata = ~gkschid, data = strat_sample2, weights= ~sis)

svymean(~gktreadss, design = design_sample2)
confint(svymean(~gktreadss, design = design_sample2))

#### Cluster Madness

cl <- cluster(combined, clustername=c("gkschid"), size = 10)
sample3 <- getdata(combined, cl)

strat_sample3 <- stratified(sample3, c("gkschid"), size = 16) 
design_sample3 <- svydesign(id = ~1, strata = ~gkschid, data = strat_sample3, weights= ~sis)

svymean(~gktreadss, design =design_sample3)
confint(svymean(~gktreadss, design =design_sample3))
