########################################
# Assignment 1
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
rm(list = ls())

######## MODIFY THIS TO YOUR PATH
setwd("/Users/Alfonso/Google Drive/UT/Fall 2017/RD")

##### Load Dataset
students <- read.dta("STAR_students.dta")

#### Load Variable Labels
var.labels <- attr(students, "var.labels")
data.key <- data.frame(var.name=names(students), var.labels)

#### Clear Mising gkschid
STAR_kindergarteners <- students[!(is.na(students$gkschid)),]

#### Number of students 

nrow(STAR_kindergarteners)

#### Population Parameters
gktreadss <- na.omit(STAR_kindergarteners$gktreadss)
mu <- mean(gktreadss)
mu
sigma <- sd(gktreadss)
sigma

n <- 160

###################
# Take Sample 1
###################

sample1 <- STAR_kindergarteners[sample(nrow(STAR_kindergarteners), n),]

gktreadss_sample1 <- na.omit(sample1$gktreadss)
xbar1 <- mean(gktreadss_sample1)
xbar1
sigma1 <- sd(gktreadss_sample1)
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

gktreadss_sample2 <- na.omit(sample2$gktreadss)
xbar2 <- mean(gktreadss_sample2)
xbar2
sigma2 <- sd(gktreadss_sample2)
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
  gktreadss_sample <- na.omit(sample$gktreadss)
  xbar <- mean(gktreadss_sample)
  sigma <- sd(gktreadss_sample)
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

n <- 15
bs15 <-  data.frame(t(sapply(1:1000, repetition)))

n <- 40
bs40 <-  data.frame(t(sapply(1:1000, repetition)))

n <- 160
bs160 <-  data.frame(t(sapply(1:1000, repetition)))

n <- 500
bs500 <-  data.frame(t(sapply(1:1000, repetition)))

#### N = 15

sda <-  sd(bs15$X1)
sda <- round(sda, digits=3)
texta <- paste ("sd =",sda)
a <- ggplot(data=bs15, aes(bs15$X1)) +
  geom_histogram(aes(y =..density..),
                 fill="red",
                 alpha = .5)

a <- a + geom_density(col =1) +
  labs(title="n = 15") +
  labs(x=NULL, y=NULL) +
  geom_vline(aes(xintercept=mean(X1)),
             color="dark green", linetype="dashed", size=0.5) +
  geom_vline(aes(xintercept=mu),
             color="BLACK", size=0.5) +
  xlim(418, 455) +
  ylim(0, 0.06) +
  annotate("text", x = 448.5, y = 0.055, label = texta, size = 3)

a <- a + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

#### N = 40

sdb <-  sd(bs40$X1)
sdb <- round(sdb, digits=3)
textb <- paste ("sd =",sdb)
b <- ggplot(data=bs40, aes(bs40$X1)) +
  geom_histogram(aes(y =..density..),
                 fill="red",
                 alpha = .5)


b <- b + geom_density(col =1) +
  labs(title="n = 40") +
  labs(x=NULL, y=NULL) +
  geom_vline(aes(xintercept=mean(X1)),
             color="dark green", linetype="dashed", size=0.5) +
  geom_vline(aes(xintercept=mu),
             color="BLACK", size=0.5) +
  xlim(418, 455) +
  ylim(0, 0.095) +
  annotate("text", x = 448, y = 0.085, label = textb, size = 3)

b <- b + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#### N = 160

sdc <-  sd(bs160$X1)
sdc <- round(sdc, digits=3)
textc <- paste ("sd =",sdc)
c <- ggplot(data=bs160, aes(bs160$X1)) +
  geom_histogram(aes(y =..density..),
                 fill="red",
                 alpha = .5)

c <- c + geom_density(col =1) +
  labs(title="n = 160") +
  labs(x=NULL, y=NULL) +
  geom_vline(aes(xintercept=mean(X1)),
             color="dark green", linetype="dashed", size=0.5) +
  geom_vline(aes(xintercept=mu),
             color="BLACK", size=0.5) +
  xlim(428, 445) +
  annotate("text", x = 442, y = 0.15, label = textc, size=3)

c <- c + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#### N = 500

sdd <-  sd(bs500$X1)
sdd <- round(sdd, digits=3)
textd <- paste ("sd =",sdd)
d <- ggplot(data=bs500, aes(bs500$X1)) +
  geom_histogram(aes(y =..density..),
                 fill="red",
                 alpha = .5)

d <- d + geom_density(col =1) +
  labs(title="n = 500") +
  labs(x=NULL, y=NULL) +
  geom_vline(aes(xintercept=mean(X1)),
             color="dark green", linetype="dashed", size=0.5) +
  geom_vline(aes(xintercept=mu),
             color="BLACK", size=0.5) +
  xlim(430, 445) +
  annotate("text", x = 442.5, y = 0.22, label = textd, size = 3)
  
d <- d + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

ggplot2.multiplot(a,b,c, d, cols=2)
