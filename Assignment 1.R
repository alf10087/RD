########################################
# Assignment 1
# Alfonso Rojas-Alvarez
# Research Design, by Paul Von Hippel
# Fall 2017
#######################################

library(ggplot2)
library(foreign)
library(Hmisc)
library(grid)
library(gridExtra)
library(easyGgplot2)
rm(list = ls())

######## MODIFY THIS TO YOUR PATH
setwd("/Users/Alfonso/Google Drive/UT/Fall 2017/RD")

##### Load Dataset
students <- read.dta("students.dta")

#### Load Variable Labels
var.labels <- attr(students, "var.labels")
data.key <- data.frame(var.name=names(students), var.labels)

#### Clear Mising gkschid
STAR_kindergarteners <- students[!(is.na(students$gkschid)),]

#### Population Parameters
mu <- mean(STAR_kindergarteners$gktreadss)
mu
sigma <- sd(STAR_kindergarteners$gktreadss)
sigma

n <- 160

###################
# Take Sample 1
###################

sample1 <- STAR_kindergarteners[sample(nrow(STAR_kindergarteners), n),]

xbar1 <- mean(sample1$gktreadss)
xbar1
sigma1 <- sd(sample1$gktreadss)
sigma1

###### Sampling Error
diff1 <- mu - xbar1
diff1

#### Confidence Interval

c1 <- xbar1 - qnorm(0.975) * sigma1/sqrt(n)
c2 <- xbar1 + qnorm(0.975) * sigma1/sqrt(n)

sample1_CI <- c(c1, c2)
sample1_CI

#### Hypothesis test

z1 <- (xbar1 - mu)/(sigma1/sqrt(n))
z1
alpha <- 0.05
critical_values <- c(-qnorm(1-alpha/2), qnorm(1-alpha/2))
reject1 <- isTRUE(!-qnorm(1-alpha/2) <  z1 & z1 < qnorm(1-alpha/2))
reject1

###################
# Take Sample 2
###################

sample2 <- STAR_kindergarteners[sample(nrow(STAR_kindergarteners), n),]

xbar2 <- mean(sample2$gktreadss)
xbar2
sigma2 <- sd(sample2$gktreadss)
sigma2

##### Sampling Error
diff2 <- mu - xbar2
diff2

#### Sample Variation
sample_variation <- xbar1 - xbar2
sample_variation

#### Confidence Interval

c1 <- xbar2 - qnorm(0.975) * sigma2/sqrt(n)
c2 <- xbar2 + qnorm(0.975) * sigma2/sqrt(n)

sample2_CI <- c(c1, c2)
sample2_CI

#### Hypothesis test

z2 <- (xbar2 - mu)/(sigma2/sqrt(n))
z2
alpha <- 0.05
critical_values <- c(-qnorm(1-alpha/2), qnorm(1-alpha/2))
reject2 <- isTRUE(!-qnorm(1-alpha/2) <  z2 & z2 < qnorm(1-alpha/2))
reject2

#########################################
# Bootstrap Method
#########################################

repetition <- function(i) {
  sample <- STAR_kindergarteners[sample(nrow(STAR_kindergarteners), n),]
  xbar <- mean(sample$gktreadss)
  sigma <- sd(sample$gktreadss)
  diff <- mu - xbar
  c1 <- xbar - qnorm(0.975) * sigma/sqrt(n)
  c2 <- xbar + qnorm(0.975) * sigma/sqrt(n)
  sample_CI <- c(c1, c2)
  z <- (xbar - mu)/(sigma/sqrt(n))
  alpha <- 0.05
  critical_values <- c(-qnorm(1-alpha/2), qnorm(1-alpha/2))
  reject <- isTRUE(!-qnorm(1-alpha/2) <  z & z < qnorm(1-alpha/2))
  results <- c(xbar, sigma, diff, sample_CI, z, reject)
}

bs25 <- data.frame(t(sapply(1:25, repetition)))
bs100 <-  data.frame(t(sapply(1:100, repetition)))
bs1000 <-  data.frame(t(sapply(1:1000, repetition)))
bs5000 <-  data.frame(t(sapply(1:5000, repetition)))

#### N = 25

meana <-  mean(bs25$X1)
meana <- round(meana, digits=2)
texta <- paste ("x=", meana)
a <- ggplot(data=bs25, aes(bs25$X1)) + 
  geom_histogram(aes(y =..density..), 
                 fill="red",
                 alpha = .5) 

a <- a + geom_density(col =1) + 
    labs(title="n = 25") +
    labs(x=NULL, y=NULL) +
  geom_vline(aes(xintercept=mean(X1)),
             color="dark green", linetype="dashed", size=0.5) +
  geom_vline(aes(xintercept=mu),
             color="BLACK", size=0.5) +
  xlim(430, 445) +
  ylim(0, 0.33) +
  annotate("text", x = 443, y = 0.3, label = texta)

a <- a + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

#### N = 100

meanb <-  mean(bs100$X1)
meanb <- round(meanb, digits=2)
textb <- paste ("x=", meanb)
specify_decimal(textb, 2)
b <- ggplot(data=bs100, aes(bs100$X1)) + 
  geom_histogram(aes(y =..density..), 
                 fill="red",
                 alpha = .5) 

b <- b + geom_density(col =1) + 
  labs(title="n = 100") +
  labs(x=NULL, y=NULL) +
  geom_vline(aes(xintercept=mean(X1)),
             color="dark green", linetype="dashed", size=0.5) +
  geom_vline(aes(xintercept=mu),
             color="BLACK", size=0.5) +
  xlim(430, 445) +
  ylim(0, 0.3) +
  annotate("text", x = 443, y = 0.3, label = textb)

b <- b + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#### N = 1000

meanc <-  mean(bs1000$X1)
meanc <- round(meanc, digits=2)
textc <- paste ("x=", meanc)
c <- ggplot(data=bs1000, aes(bs1000$X1)) + 
  geom_histogram(aes(y =..density..), 
                 fill="red",
                 alpha = .5) 

c <- c + geom_density(col =1) + 
  labs(title="n = 1000") +
  labs(x=NULL, y=NULL) +
  geom_vline(aes(xintercept=mean(X1)),
             color="dark green", linetype="dashed", size=0.5) +
  geom_vline(aes(xintercept=mu),
             color="BLACK", size=0.5) +
  xlim(430, 445) +
  annotate("text", x = 443, y = 0.15, label = textc)

c <- c + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#### N = 5000

meand <-  mean(bs5000$X1)
meand <- round(meand, digits=2)
textd <- paste ("x=", meand)
d <- ggplot(data=bs5000, aes(bs5000$X1)) + 
  geom_histogram(aes(y =..density..), 
                 fill="red",
                 alpha = .5) 

d <- d + geom_density(col =1) + 
  labs(title="n = 5000") +
  labs(x=NULL, y=NULL) +
  geom_vline(aes(xintercept=mean(X1)),
             color="dark green", linetype="dashed", size=0.5) +
  geom_vline(aes(xintercept=mu),
             color="BLACK", size=0.5) +
  annotate("text", x = 443, y = 0.15, label = textd)
  
d <- d + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
d  

ggplot2.multiplot(a,b,c, d, cols=2)
