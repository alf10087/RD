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


