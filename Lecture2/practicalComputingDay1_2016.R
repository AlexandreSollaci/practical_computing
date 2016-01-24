#===========================================
#  TITLE:  Rapid Introduction to R (day 1)
#  AUTHOR: John Eric Humphries
#  (this code draws from a pset I assigned in econ 21410 as well as oliver Browne's solutions, and "Advanced R" by Hadley Wickham)
#  abstract: an applied introductino to R.
#
#  Date: 2015-01-08
#================================


#========================
# Section 0: setup
#========================

#setwd("/mnt/ide0/home/johneric/sbox/projects/neighborhoodVision/")
rm(list=ls())           # Clear the workspace
set.seed(907)           # set my random seed
library(ggplot2)
library(mnormt)
library(sandwich)
library(car)
library(xtable)
library(AER)
library(systemfit)
library(MASS)
library(stargazer)
library(texreg)
library(data.table)



#==================================================================
#
#
## Section 1: Getting started with Real Data right away.
#===================================================================

# Reading in 1988 CPS data
data(CPS1988)
CPS <- data.table(CPS1988) # Data tables versus data frames (just mention briefly)
# mention fread.
# mention plyr
names(CPS)
head(CPS)
summary(CPS)


# Data set summaries in latex
stargazer(CPS)
xtable(CPS)
xtable(summary(CPS))


# Quickly running some simple mincer regressions
mincer1 <- lm(wage ~ education + experience + I(experience^2), data=CPS)
mincer2 <- lm(wage ~ education + experience + I(experience^2) + region, data=CPS)

# regression summaries in latex
xtable(mincer1) # see examples here: http://cran.r-project.org/web/packages/xtable/vignettes/xtableGallery.pdf
stargazer(mincer1, mincer2) # http://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf
texreg(list(mincer1, mincer2)) # http://www.jstatsoft.org/v55/i08/paper




# Getting started
draws <- rnorm(25)
summary(draws)
class(draws)

#Making and manipulating matricies
draws.m <- matrix(draws,5,5)
class(draws.m)
draws.m[1,1]
draws.m[7]
draws.m[4,]
diag(draws.m)
a = c(2,3)
b = c(4,5)
d = c(7)
c(a,b)
cbind(a,b)
rbind(a,b)

c(a,d)
# broadcasting!
cbind(a,d)

#logical statements on matricies
draws[draws>0]
draws>0
which(draws>0)
any(draws>0)
all(draws>0)
all.equal(c(2,2),c(2,2))
as.character(draws)

# Getting to know lists
colors <- c("red", "blue", "yello")
mylist <- list(draws,draws.m,colors)
mylist
mylist[[1]]
mylist[[2]]
mylist[[3]]
mylist[[3]][1]

#============================
# Section 1.5 Create data generating functions and generate data
#=============================

#generate data and learn about control-flow.

GenerateDataSimple = function(n){
  # Generates data with from a simple linear model
  # args: n - number of observations to generate
  # output: A list containing a Y vector and an X matrix
  x1     <- rbinom(n,10,.5)
  x2     <- rnorm(n,20,10)
  X      <- cbind(1,x1,x2)
  theta2 <- 5
  eps    <- rnorm(n,0,sqrt(4))
  beta   <- matrix(cbind(2,3,1.4),3,1)
  Y   <-  X %*% beta + eps
  colnames(X) <- c("const", "x1","x2")
  colnames(Y) <- c("Y")
  return(list(Y,X))
}

# Simulating simple data
data <- GenerateDataSimple(100)
x <- data[[2]]
y <- data[[1]]



#=======================
# Section 1.75: OLS with pre-built commands
#=======================

#-------------------------
# standard linear model
#--------------------------
reg1 = lm( y ~ x -1) # weights and na.action options can be important
# playing around
reg1
names(reg1)
regout1 <- summary(reg1)
regout1
names(regout1)
regout1$coefficients
xtable(regout1)
#plot(reg1)
abline(reg1)

#could also run:
summary( lm( y ~ x + I(x^2) -1  )) # I() means take input "as is" , ie, don't overlaod to equatiol notaion meaning of "^"



#----------------------------
# Testing linear hypotheses
#-----------------------------
linearHypothesis(reg1, c("xx1=0", "xx2=0"))

#-----------------------
# fitted values
#-----------------------
reg1.hat        <- fitted(reg1)
reg1.resids     <- residuals(reg1)

#------------------------
# Dummy regression with "factor" variable types (i.e., categorical variables)
#------------------------

types <- c("blue", "green","red")
type  <- sample(types,length(y),replace=T)
reg1.dummies <- lm(y ~ x[,2:dim(x)[2]] + type -1)
summary(reg1.dummies)
reg1.dummies2 <- lm(y ~ x[,2:dim(x)[2]] + type +  x[,2]*type -1)
summary(reg1.dummies2)



# avPlots(reg1, id.n=2,id.cex=.7) # partial regression / influence plots  # First option chooses the number of people to label, second number is font sizi
# qqPlot(reg1, id.n=2)
# influenceIndexPlot(reg1, id.n=2)
# influencePlot(reg1, id.n=2)
ncvTest(reg1) # test for heteroskedasticity (homosk data)
ncvTest(reg1) # test for heteroskedasticity (heterok data)

# A  real quick probit and a logit
Ybin <- y>44
probit1 <- glm(Ybin ~ x - 1, family = binomial(link="probit"))
summary(probit1)
confint(probit1)
logLik(probit1)

logit1 <- glm(Ybin ~ x - 1, family = binomial(link="logit"))
summary(logit1)

# generate new data to predict on
data2 <- GenerateDataSimple(100)
X2 <- data2[[2]]
colnames
Y2 <- data2[[1]]
Y2bin <- Y2>44
# predicted values:
data2.predvalues = predict(probit1, newdata=data2, type = "response") #returns probability of success, can add standard error with se.fit = T
cbind(Y2bin,round(data2.predvalues,4))

#=====================================================
#
#
# SECTION 2: partial linear regression: fitting a function
#====================================================
GenerateDataNonlinear = function(n) {
  x1     <- rnorm(n,6,2)
  X      <- cbind(1,x1)
  theta2 <- 5
  eps    <- rnorm(n,0,sqrt(140))
  Y   <-  as.matrix(5 + 3*x1  + 1.1 * x1^2  + 30* sin(x1) - .15 * x1^3  + eps )
  colnames(X) <- c("const", "x1")
  colnames(Y) <- c("Y")
  return(list(Y,X))
}


library(splines)
data3 <- data.frame(GenerateDataNonlinear(1000))
reg3.lm   <- lm(Y ~ x1 , data=data3)
reg3.poly <- lm(Y ~ poly(x1, 4)  , data=data3)
reg3.bs   <- lm(Y ~  bs(x1, df = 5) , data=data3)
reg3.true <- lm( Y ~ x1  + I(x1^3) +  I( sin(x1))  + I(x1^3) , data=data3)


# cross validate the two models (plus data frames!:
data4 <- data.frame(GenerateDataNonlinear(5000))
data4$yhat.lm <- predict(reg3.lm, newdata= data4)
data4$yhat.poly <- predict(reg3.poly, newdata= data4)
data4$yhat.bs <- predict(reg3.bs, newdata= data4)
data4$yhat.true <- predict(reg3.true, newdata= data4)

# Plot showing fit.
plot(Y ~ jitter(x1, factor = 3), pch=18, col=rgb(0.5, 0.5, 0.5, alpha=0.2), data=data4, ylim=c(-80,80), xlim = c(0,12))
vals <- cbind(data4$yhat.lm,data4$x1)
v2   <- vals[order(vals[,2]),]
lines(v2[,1] ~ v2[,2], col="red")
vals <- cbind(data4$yhat.poly,data4$x1)
v2   <- vals[order(vals[,2]),]
lines(v2[,1] ~ v2[,2], col="blue")
vals <- cbind(data4$yhat.bs,data4$x1)
v2   <- vals[order(vals[,2]),]
lines(v2[,1] ~ v2[,2], col="green")
vals <- cbind(data4$yhat.true,data4$x1)
v2   <- vals[order(vals[,2]),]
lines(v2[,1] ~ v2[,2], col="black")
# y ~ (a + b + c)^2 would model all two-way interactions


#===========================================
#
#  computational economics:  2 Schelling Segregation
#
# SECTION 3
#===========================================



#setwd("/mnt/ide0/home/johneric/sbox/projects/neighborhoodVision/")
rm(list=ls())           # Clear the workspace
set.seed(777)
#library(compiler)
#enableJIT(1)
library(knitr)
library(ggplot2)
library(mvtnorm)
library(reshape2)




SchellingGrid <- function(gs = 100, probabilities = c(.495,.495,.1) , stop.val = .99 , happy.val = .4) {
    values = c(1,-1,0)
    grid.size = gs
    values.mat = matrix(sample(values, grid.size^2, replace = T, prob = probabilities), grid.size, grid.size )
    # starting plot
    p <- (qplot(x=Var1, y=Var2, data=melt(values.mat), fill=value, geom="tile", color = "white", main="SCHELLING GRID: 0")
          +  scale_fill_gradient2(low = "lightgreen", mid = "white", high = "steelblue")  ) + theme(legend.position = "none")
    print(p)
    values.happy = matrix(NA, grid.size, grid.size)
    values.same  = matrix(NA, grid.size, grid.size)
    ratio = happy.val
    stop.ratio = stop.val
    i = 0
    while ( sum(values.happy, na.rm=T) / (sum(values.mat!=0, na.rm=T)) < stop.ratio) {
        i = i + 1
        for (row in sample(1:grid.size)) {
            for (col in sample(1:grid.size)) {
                nbrs = c(rep(NA,8))
                if( row>1 & col>1)                  nbrs[1] <- values.mat[row -1, col -1]
                if( row>1)                          nbrs[2] <- values.mat[row -1, col   ]
                if( row>1 & col<grid.size)          nbrs[3] <- values.mat[row -1, col +1]
                if( col>1)                          nbrs[4] <- values.mat[row   , col -1]
                if( col<grid.size)                  nbrs[5] <- values.mat[row   , col +1]
                if( row<grid.size & col>1)          nbrs[6] <- values.mat[row +1, col -1]
                if( row<grid.size )                 nbrs[7] <- values.mat[row +1, col   ]
                if( row<grid.size & col<grid.size)  nbrs[8] <- values.mat[row +1, col +1]
                # checking if they want to move, and if so, moving at random
                val = values.mat[row,col]
                if (val != 0) {
                    if (sum(nbrs==val ,na.rm=T) / sum(!is.na(nbrs))  < ratio )  { # if not happy
                        values.mat[row,col] = 0
                        newhome = sample(which(values.mat==0),1)
                        values.mat[newhome] = val
                        values.happy[newhome] =0
                        values.happy[row, col] = NA
                    }
                    if (sum(nbrs==val ,na.rm=T) / sum(!is.na(nbrs))  >= ratio ) { # if happy
                        values.happy[row, col] =1
                        values.same[row,col] = sum(nbrs==val ,na.rm=T) / sum(!is.na(nbrs))
                    }
                }
            } # end column loop
        } # end row loop
        print(paste("Percent Happy:", 100 * sum(values.happy, na.rm=T) / (sum(values.mat!=0)), "(iteration", i, ")" )) # Printing percent happy
        p <- (qplot(x=Var1, y=Var2, data=melt(values.mat), fill=value, geom="tile", color = "white", main= paste("SCHELLING GRID:",i))
              +  scale_fill_gradient2(low = "lightgreen", mid = "white", high = "steelblue")  ) + theme(legend.position = "none")
        if (i %% 5 == 0) print(p)  # printing intermediatne plot every so many iterations
    } # end while statement
    # Printing final figure
    p <- (qplot(x=Var1, y=Var2, data=melt(values.mat), fill=value, geom="tile", color = "white", main= "SCHELLING GRID: (final)")
          +  scale_fill_gradient2(low = "lightgreen", mid = "white", high = "steelblue")  ) + theme(legend.position = "none")
    print(p)
    return(c(mean(values.happy, na.rm=T),mean(values.same, na.rm=T), i,p))
}


# Running the function
results.schelling = SchellingGrid(gs = 100, probabilities = c(.4955,.4955,.01) , stop.val = .995 , happy.val = .51)


#=================================
# dplyr and data.table
#
# SECTION 4
#
#====================================


rm(list=ls())           # Clear the workspace
library(parallel)
library(dplyr)
library(AER)
library(data.table)


#===============================
# Generating some fake data (and real data)
#================================

data(CPS1988)
names(CPS1988)

library(Hmisc)
describe(CPS1988)


CPS1988 <- tbl_df(CPS1988)
CPS1988

#---------------------------
# "Vocabulary" for dplyr
#---------------------------

# filter - select rows based on logical operations
filter(CPS1988, ethnicity == "cauc", parttime == "no") # use comma for "and" and | for "or"
# slice - select rows based on row numbers
slice(CPS1988,1:10) #  note it takes a vector of row numbers which can be useful when prograpmming
# arrange - rearrange the data
arrange(CPS1988,wage,education,experience,parttime) # NOTE this is not permanant, Need to reassign if i want it to "stick"
# select - subset the columns of your data
select(CPS1988,wage,education,experience,parttime)
select(CPS1988,contains("a")) # many 'helper' functions starts_with, ends_with, contains, matches()
select(CPS1988,ethnicity:region)
# rename - rename variables
rename(CPS1988, urban=smsa)
# distinct - return only unique variables
distinct(FD)  # only the unique variables from our simulated data
# mutate - add new variables
mutate(CPS1988,
       avg_wag = mean(wage),
       median_educ = median(education))
# transmute - only keeps the new variables
transmute(CPS1988,
          avg_wag = mean(wage),
          median_educ = median(education))
# summarize - create summary data-frames
summarize(CPS1988,
          avg_wage = mean(wage),
          median_wage = median(wage),
          var_wage = var(wage),
          distinct_edu = n_distinct(education),
          observations = n(),
          missing_wage         = sum(is.na(wage)))
# sample_n
sample_n(FD,10)
sample_n(FD,10,replace = TRUE) # useful for bootstap
# sample_frac
sample_frac(FD,.001)
sample_frac(FD,1) # a bootstrap sample

#------------------------
# Adding group_by commands.
#-----------------------
CPSregion <- group_by(CPS1988,region)
CPSregion
# changes how many commands work:
summarize(CPSregion,
          avg_wage = mean(wage),
          median_wage = median(wage),
          var_wage = var(wage),
          distinct_edu = n_distinct(education),
          observations = n(),
          missing_wage         = sum(is.na(wage)))
sample_n(CPSregion,5)
slice(CPSregion,1:5)


#---------------------------
# Now the awesome part: chaining
#---------------------------
CPS1988 %>%
    group_by(parttime) %>%
    filter(education>=12) %>%
    summarise( avg_wage = mean(wage),
               median_wage = median(wage),
               var_wage = var(wage),
               distinct_edu = n_distinct(education),
               observations = n(),
               missing_wage         = sum(is.na(wage)))


CPS1988 %>%
    filter(parttime=="no") %>%
    lm(wage ~ education + experience,data=.) %>%
    summary()

dropouts <- CPS1988 %>%
    filter(education<12) %>%
    summarise( "Avg Wage"= mean(wage),
               "Med Wage" = median(wage),
               "Wage Variance" = var(wage),
               "distinct_edu" = n_distinct(education),
               observations = n(),
               missing_wage         = sum(is.na(wage))) %>%
    t()

grads <- CPS1988 %>%
    filter(education>=12) %>%
    summarise( "Avg Wage"= mean(wage),
               "Med Wage" = median(wage),
               "Wage Variance" = var(wage),
               "distinct_edu" = n_distinct(education),
               observations = n(),
               missing_wage         = sum(is.na(wage))) %>%
    t()

library(stargazer)
sumTable <- cbind(dropouts,grads)
colnames(sumTable) <- c("Dropouts","Graduates")
stargazer(sumTable ,type="text",title="Summary Statistic (Dropouts vs HS Grads)", colnames = T
          ,notes="Notes: Graduates are those with 12 or more years of schooling")


# BUT WE CAN DO BETTER! -------------------------------
sumTable <- CPS1988 %>%
    mutate(grad=education<12) %>%
    group_by(grad) %>%
    summarise( "Avg Wage"= mean(wage),
               "Med Wage" = median(wage),
               "Wage Variance" = var(wage),
               "distinct_edu" = n_distinct(education),
               observations = n(),
               missing_wage         = sum(is.na(wage))) %>%
    t()
colnames(sumTable) <- c("Dropouts","Graduates")
stargazer(sumTable ,type="text",title="Summary Statistic (Dropouts vs HS Grads)", colnames = T
          ,notes="Notes: Graduates are those with 12 or more years of schooling")


#------------------------------------------------------
# OTHER NICE FEATURES:
#1 its fast
#2 it can be used to call SQL databases
#3 experimental work on tbl_cube.
#4 provides C++ versions of many of the functions to use when writing your own C++ code.




#---------------------------
# "Vocabulary" for ddata.table
#---------------------------
data(CPS1988)
CPS1988 <- data.table(CPS1988)
# filter - select rows based on logical operations
CPS1988[ethnicity == "cauc" & parttime =="no"]
# slice - select rows based on row numbers
CPS1988[1:10]
# arrange - rearrange the data(
setorder(CPS, c(...))
# select - subset the columns of your data
CPS1988[, .(wage,experience)] # NOTE this is not permanant, Need to reassign if i want it to "stick"
CPS1988[, c(1,2,3), with =F]
# rename - rename variables
setnames(CPS1988,"smsa","urban")
# distinct - return only unique variables
unique(CPS1988)
# mutate - add new variables
CPS1988[, list(avg_wage = mean(wage), median_educ = median(education))]

# transmute - only keeps the new variables
CPS1988[, c("avg_wage","median_educ") := list(mean(wage),median(education))]
# summarize - create summary data-frames
CPS1988[, list(
    avg_wage = mean(wage),
    median_wage = median(wage),
    var_wage = var(wage),
    distinct_edu = length(unique(education)),
    observations = .N,
    missing_wage         = sum(is.na(wage)))]
# sample_n

#------------------------
# Adding group_by commands.
#-----------------------

# changes how many commands work:
CPS1988[, list(
    avg_wage = mean(wage),
    median_wage = median(wage),
    var_wage = var(wage),
    distinct_edu = length(unique(education)),
    observations = .N,
    missing_wage         = sum(is.na(wage))), by=region]



#---------------------------
# Now the awesome part: chaining
#---------------------------
CPS1988[education>=12, list(
    avg_wage = mean(wage),
    median_wage = median(wage),
    var_wage = var(wage),
    distinct_edu = length(unique(education)),
    observations = .N,
    missing_wage         = sum(is.na(wage))), by=parttime]




CPS1988 %>%
    filter(parttime=="no") %>%
    lm(wage ~ education + experience,data=.) %>%
    summary()

lm(wage ~ education + experience, data = CPS1988[parttime=="no"])

dropouts <- CPS1988[education<12, list(
    "Avg Wage"= mean(wage),
    "Med Wage" = median(wage),
    "Wage Variance" = var(wage),
    "distinct_edu" = length(unique(education)),
    observations = .N,
    missing_wage         = sum(is.na(wage))
)]

grads <- CPS1988[education<12, list(
    "Avg Wage"= mean(wage),
    "Med Wage" = median(wage),
    "Wage Variance" = var(wage),
    "distinct_edu" = length(unique(education)),
    observations = .N,
    missing_wage         = sum(is.na(wage))
)]

CPS1988[, graduate := education>=12]
combo = CPS1988[ , list(
    "Avg Wage"= mean(wage),
    "Med Wage" = median(wage),
    "Wage Variance" = var(wage),
    "distinct_edu" = length(unique(education)),
    observations = .N,
    missing_wage         = sum(is.na(wage))), by = graduate
    ]



library(stargazer)
sumTable <- cbind(t(dropouts),t(grads))
colnames(sumTable) <- c("Dropouts","Graduates")
stargazer(sumTable ,type="text",title="Summary Statistic (Dropouts vs HS Grads)", colnames = T, summary=F
          ,notes="Notes: Graduates are those with 12 or more years of schooling")


# BUT WE CAN DO BETTER! -------------------------------
sumTable <- t(combo)
colnames(sumTable) <- c("Dropouts","Graduates")
stargazer(sumTable[-1,] ,type="text",title="Summary Statistic (Dropouts vs HS Grads)", colnames = T, summary=F
          ,notes="Notes: Graduates are those with 12 or more years of schooling")


#------------------------------------------------------
# OTHER NICE FEATURES:
#1 its fast
#2 it can be used to call SQL databases
#3 experimental work on tbl_cube.
#4 provides C++ versions of many of the functions to use when writing your own C++ code.

