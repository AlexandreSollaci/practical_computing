#################################
# Practical COmputing Lecture # 1

rm(list=ls()) # same as clear all
set.seed(1702) # sets seed for random numbers

library(ggplot2)
library(AER)
library(mnormt)
library(xtable)
library(stargazer)
library(texreg)
library(data.table)
# and so on. Just copy it from the files

# to install packages: install.packages("packagename")

data(CPS1988) #load data
cps <- data.table(CPS1988) #define data table; also <- and = are the same
names(cps) # rename the data frame as 'cps'

head(cps, 10) #see only the first 10 entries of data set
summary(cps) # show summary statistics

stargazer(cps, type = "text") # latex table w/ summary statistics ("text" makes a little table, easier to see)

#stargazer(cps, type = "text", summary = F) # this would print a table with the entire dataset

xtable(iris) # same: little table with tex output

mincer1 = lm(wage ~ education + experience, cps) #mincer regression, using cps data

mincer1_summary = summary(mincer1) # stata-like summary for regression

# same as mincer1_summary - summary(lm(wage ~ education + experience, data = cps))

# Use TAB to cmplete commands

# Figure out what is saved: names(mincer1_summary)
# call stuff: mincer_summary$r.squared

mincer2 = lm(wage ~ education + experience + I(experience^2)
             , data = cps) # can just break lines for free

stargazer(mincer1, mincer2, type = "text") # type = "latex", out = "test.tex") saves the whole ting in a file

reglist = list(mincer1, mincer2) # just collect objects together. doesn't matter what these objects are: numbers,
#outputs, figures, etc.

texreg(reglist) # same table as before, giving out a latex table

mincer3 = lm(wage ~ education + experience + I(experience^2) + region - 1
             , data = cps) # region is a string, so R introduces dummies. The -1 (or +0) will drop the constant.

#####################
# Section 2
####################

draws = rnorm(25)
draws_m = matrix(draws, 5, 5) #puts data into matix. Is COLUMN oriented: fills up columns first

# get elements
draws[10]
draws[1:5] # 1 throigh 5
draws[-1] #all but the first
draws[c(1,5,10)]
# draws[10] = draws.m[5,2]
draws_m[4:5,] #rown 4-5, all columns

#################

a = c(2,3) # atomic vector
b = c(4,5)
d = c(7)

c(a,b) # just puts them together
cbind(a,b) # column bind
rbind(a,b) #row bind

cbind(a,d) # dimensions don't match, so it just repeats the number

e = rep(10,7) # create a vector of 10's with dimension 7
f = cbind(e, c(b, rep(0, length(e) - length(b))))

# TRUE / FALSE
draws > 0
draws[draws > 0]
which(draws > 0) #find the positions of those entries
which(draws_m > 0, arr.ind = T) #returns stuff as matrix positions

all(draws > 0)
all.equal(c(2,2),c(2,3))

as.character(draws) #give me back stugg as strings

# Lists

color = c("red", "blue", "green")
class(color)
length(color)

mylist = list(draws, draws_m, color)
length(mylist) #will equal 3 too, not 53, which is the sum of the number of things in there

mylist[[3]] #returns color
mylist[[3]][2] #returns blue
color = c(r = "red", b = "blue", g = "green")
color["r"]

# data frames
# basically, same as matrix, but columns can be whatever kind of data. also, columns can be different kinds of data

#===========================
# Section 3: OLS and functions
#===========================

GenerateDatSample = function(n=100) { #having n=100 here sets a default value, so GenerateDataSample() returns n=100
  # generate output from linear model
  # args: n - the number of obs to output
  # output: a list containing Y and X
  if (is.numeric(n) == F | length(n) != 1){
    stop("n is not numeric or is too long")
  }
  x1 = rbinom(n, 20, .5)
  x2 = rnorm(n, 20, 10)
  X = cbind(const = 1, x1, x2) #const is naming the variable
  eps = rnorm(n, 0, 2)
  bet = matrix( c(2, 3, 1.4), 3, 1)#avoid using beta because this is a function in R
  Y = X %*% bet + eps #%*% is for matrix multiplication
  return(data.frame(Y,X))
}


data = GenerateDatSample(100)
reg1 = lm(Y ~ x1 + x2, data = data)

regsum = summary(reg1)
plot(reg1)
abline(reg1) #identifies outliers too

linearHypothesis(reg1, c("x1=0","x2=0"))

# Probit
Ybin = data$Y>44 #transform into binary data
data$Ybin = Ybin #put this variable into dataset
# equivalent: data$Ybin = (data$Y>44)

probit1 = glm(Ybin ~ x1 + x2, data = data, family=binomial(link = "probit"))


# Function Approximation

library(splines)
reg2_lm = lm(Y ~ x1, data = data)
reg2_poly = lm(Y ~ poly(x1,4), data = data) #polynomial
reg2_bs = lm(Y ~ bs(x1,4), data = data) #b spline
reg2 = lm(Y ~ x1 + I(x1^2) + I(sin(x1)) + I(x1^3), data = data )


# Last thing: plots. See code used in class.

#=================================================================

rm(list = ls())
library(dplyr)

data(CPS1988)
library(Hmisc)
describe(CPS1988) #useful for finding mistakes in data

CPS1988
CPS = tbl_df(CPS1988) #better visualization of data

# filter - select rows based on logical operators
filter(CPS, ethnicity == "cauc" , parttime == "no" | smsa == "yes")

#slice - selects rows
slice(CPS, 1:10)

#arrange - rearranges order in which things show up
arrange(CPS, wage, experience, education)

#select - grab specific columns
select(CPS, wage, education, experience)
select(CPS, contains("a")) #every variable that has an "a" in it
select(CPS, ethnicity:region #everything between ethnicity and region
       
#rename a variable
rename(CPS, urban = smsa)

#distinct - find unique elements
distinct(CPS)

#add new variables
mutate(CPS, avg_wage = mean(wage))

#transmute - returns only new variables
transmute(CPS, avr_wage = mean(wage), med_ed = median(education))

#summarize
summarize(CPS, 
          avg_wage = mean(wage),
          med_ed = median(education),
          var_wage = var(wage),
          distinct_edu = n_distinct(education),
          missing_wage = sum(is.na(wage))) %>% t #this last part use to transpose the table

# sample
sample_n(CPS, 10)
sample_frac(CPS, .1)
#selects subsamples: good for bootstrapping

#====================

CPSregion = group_by(CPS, region)
summarize(CPSregion, 
          avg_wage = mean(wage),
          med_ed = median(education),
          var_wage = var(wage),
          distinct_edu = n_distinct(education),
          missing_wage = sum(is.na(wage))) %>% t



#===============
# Chaining
#===============

CPS %>% group_by(parttime) %>% 
   filter(education>=12) %>% 
   summarize(., 
                  avg_wage = mean(wage),
                  med_ed = median(education),
                  var_wage = var(wage),
                  distinct_edu = n_distinct(education),
                  missing_wage = sum(is.na(wage))) %>% t
# take CPS data, group by parttime, filter out education and then summarize it





