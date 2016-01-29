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
library(dplyr)


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



#==============================================
# WRITING FUNCTIONS:
#
# AN EXAMPLE WITH OLS
#
#===============================================


#====================
# Section 1: generating data
#====================
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



#=========================
# Section 2: Defining sub functions
#=========================

SimpleOLS <- function(y=y,x=x) {
    beta_hat1 = solve(t(x) %*% x) %*% (t(x)%*% y)
    se1       = sqrt(diag((1/(length(y) - length(beta_hat1))) * (t(y - x%*%beta_hat1) %*% (y-x%*%beta_hat1))[1] * solve(t(x) %*% x)))
    out = list(t(beta_hat1),se1)
    return(out)
}



SumOfSquares = function(b,x=X,y=Y)
{
    #Sum of squares function for numerical optimizer
    sum((y - x%*%b)^2)
}

Llik = function(pars,x=X,y=Y)
{
    #Log Likelihood function for normal OLS with numerical optimizer
    b = pars[1:3]
    t2 = pars[4]
    n  = length(y)
    if (t2 <=.000001) value = 1000000000000000000000
    else  value = -1 * (-n / 2 * log(2*pi) - n/2* log(t2) - 1/(2* t2)* (t(y - x%*%b) %*% (y - x%*%b)))
    value
}

OLS.gradient <- function(b,x=X,y=Y){
    #Returns the analytic gradient of OLS
    return(-2*t(X)%*%(Y - X%*%b))
}

lik_bootstrap = function(pars = c(1,1,1,1000), x=X,y=Y,bsnum=bsnum) {
    bs_estimates = matrix(NA,bsnum, (dim(x)[2] +1) )
    n = length(y)
    for (i in 1:bsnum) {
        samp  = sample(n,n,replace=T)
        Xboot = x[samp,]
        Yboot = y[samp]
        bs_estimates[i,] <- optim(par=pars, Llik,  x=Xboot, y=Yboot )$par
    }
    standard_errors = apply(bs_estimates,2,sd)
    return (standard_errors)
}



OLS = function(x=X,y=Y,method=1,optim.method ="BFGS",gradient=NULL,hessian=FALSE,bootstrap=FALSE)
{
    #Calculates OLS using one of four methods:
    # Method == 0 uses the standard lm function
    # Method == 1 Calculates OLS algebraically
    # Method == 2 Uses an optimizer to minimize the sum of squares
    # Method == 1 Uses an optimizer to maximize a likelihood
    if(method==0){
        result = lm(y ~ x -1 )
        beta_hat0 = result$coefficients
        print(summary(result))
        return(beta_hat0)
    }else if(method==1){
        beta_hat1 = solve(t(x) %*% x) %*% (t(x)%*% y)
        return(t(beta_hat1))

    }else if(method==2){
        beta_hat2 <- optim( c(0,0,0), SumOfSquares,method = optim.method, x=x, y=y, hessian=hessian)
        if(hessian==TRUE){
            mle_se = sqrt(diag(solve(beta_hat2$hessian)))
            return(list(beta_hat2$par,mle_se))
        }else{
            return(beta_hat2$par)
        }
    }else if(method==3){
        beta_hat3 = optim( c(1,1,1,1000), Llik, method = optim.method,  x=x, y=y, gr=gradient)$par
        if (bootstrap==T) {
            beta_hat3 = optim( c(1,1,1,1000), Llik, method = optim.method,  x=x, y=y)$par
            se3 = lik_bootstrap(pars = c(1,1,1,1000), x=X,y=Y,bsnum=50)
            out = list(beta_hat3[1:3],se3[1:3])
            return(out)
        } else {
            return(beta_hat3[1:3])
        }
    } else {
        return("Error! Method not found")
    }
}

# Simulating simple data
data <- data.frame(GenerateDataSimple(100))
attach(data)
X = cbind(const,x1,x2)

OLS(x=X,y=Y,method=0)
OLS(x=X,y=Y,method=3,bootstrap=T)
OLS(x=X,y=Y,method=2,bootstrap=F,hessian=T,gradient =OLS.gradient)
OLS(x=X,y=Y,method=2,bootstrap=F,hessian=T)

#==========================================
#
# Random Coefficients Treatment Effects Model
#
#
#============================================


#====================================================
# Simulating data
#=====================================================

# Variables

n = 30000
x = runif(n)*5
z = runif(n)*3
theta = rnorm(n)

# Parameters
beta1 = 1.5
beta0 = 1.4
alpha1 = 1.2
alpha0 = -2
gamma = c(.5,-1)
true_par = c(beta1,beta0,gamma,alpha1,alpha0)
mu = c(0,0,0)
sigma = diag(c(1,1,1))


# Simulating outcomes.
errors = rmvnorm(n,mu,sigma)
matrix = cbind(x,z,theta,errors)
colnames(matrix) = c("x","z","theta","e1","e0","eD")

d_star = colSums(c(gamma,1,1)*t(matrix[,c(1:3,6)]))
y1_star = colSums(c(beta1,alpha1,1)*t(matrix[,c(1,3,4)]))
y0_star = colSums(c(beta0,alpha0,1)*t(matrix[,c(1,3,5)]))

d = d_star > 0
y1 = y1_star
y0 = y0_star

y = d*y1+(1-d)*y0


#------------------------------------------------------
# Function: Return likelihood with known theta
#------------------------------------------------------

f.value1 = function(par=par,data=data){
    # calculates the log likelihood assuming theta is known.
    beta1 = par[1]
    beta0 = par[2]
    gamma = par[3:4]
    alpha1 = par[5]
    alpha0 = par[6]

    likelihood =
        pnorm(colSums(c(gamma,1)*t(data[,3:5])))^d*
        dnorm(y - colSums(c(beta1,alpha1)*t(data[,c(3,5)])))^d*
        (1-pnorm(colSums(c(gamma,1)*t(data[,c(3:5)]))))^(1-d)*
        dnorm(y - colSums(c(beta0,alpha0)*t(data[,c(3,5)])))^(1-d)

    ll = sum(log(likelihood))

    return(-ll)
}

#------------------------------------------------
# Function: Return likelihood with unknown theta
#------------------------------------------------
f.value2 = function(par=par,data=data){
    # Calculates the log likelihood omitting theta
    beta1 = par[1]
    beta0 = par[2]
    gamma = par[3:4]

    likelihood =
        pnorm(1/sqrt(2)*colSums(c(gamma)*t(data[,3:4])))^d*
        dnorm(1/sqrt(5)*(y-colSums(c(beta1)*t(data[,c(3)]))))^d*
        (1-pnorm(1/sqrt(2)*colSums(c(gamma)*t(data[,3:4]))))^(1-d)*
        dnorm(1/sqrt(2)*(y-colSums(c(beta0)*t(data[,c(3)]))))^(1-d)

    ll = sum(log(likelihood))

    return(-ll)
}

f.value2.alt = function(par=par,data=data){
    # Calculates the log likelihood omitting theta
    beta1 = par[1]
    beta0 = par[2]
    gamma = par[3:4]
    s     = par[5]
    s1    = par[6]
    s2    = par[7]

    likelihood =
        pnorm(colSums(c(gamma)*t(data[,3:4])),sd=s)^d*
        dnorm((y-colSums(c(beta1)*t(data[,c(3)]))),sd=s1)^d*
        (1-pnorm(colSums(c(gamma)*t(data[,3:4])),sd=s))^(1-d)*
        dnorm((y-colSums(c(beta0)*t(data[,c(3)]))),sd=s2)^(1-d)

    ll = sum(log(likelihood))

    return(-ll)
}

#----------------------------------------------------------------
# Function: Return likelihood with theta as a random coefficient
#----------------------------------------------------------------
f.value3 = function(par=par,data=data){
    # Calculates the log likelihood integrating out theta
    data_orig = data
    n = dim(data)[1]

    beta1 = par[1]
    beta0 = par[2]
    gamma = par[3:4]
    alpha1 = par[5]
    alpha0 = par[6]

    q_num = 18
    quad = gauss.quad.prob(q_num,dist="normal",mu=0,sigma=1)

    values = matrix(0,q_num,n)
    for(i in 1:q_num){
        theta = rep(quad$nodes[i],n)
        data = cbind(data_orig,theta)

        likelihood =
            pnorm(colSums(c(gamma,1)*t(data[,3:5])))^d*
            dnorm(y - colSums(c(beta1,alpha1)*t(data[,c(3,5)])))^d*
            (1-pnorm(colSums(c(gamma,1)*t(data[,c(3:5)]))))^(1-d)*
            dnorm(y - colSums(c(beta0,alpha0)*t(data[,c(3,5)])))^(1-d)

        values[i,] = likelihood
    }
    combined=colSums(quad$weights*values)

    return(-sum(log(combined)))
}




#================================================
# Some Faster Functions:
#================================================

f.value3.cpp = function(par=par,data_orig=data){
    # Calculates the log likelihood integrating out theta
    q_num = 18
    quad = gauss.quad.prob(q_num,dist="normal",mu=0,sigma=1)
    likloop(quad$nodes,quad$weights,data_orig,par)
}





registerDoMC(6)
f.value3.par = function(par=par,data=data){
    # let it know to use 8 cores

    # Calculates the log likelihood integrating out theta
    d <- data[,1]
    y <- data[,2]
    data_orig = data
    n = dim(data)[1]

    beta1 = par[1]
    beta0 = par[2]
    gamma = par[3:4]
    alpha1 = par[5]
    alpha0 = par[6]

    q_num = 18
    quad = gauss.quad.prob(q_num,dist="normal",mu=0,sigma=1)

    combined <- foreach(i = 1:q_num, .combine = '+', .inorder = F) %dopar% {
        theta = rep(quad$nodes[i],n)
        data = cbind(data_orig,theta)
        library(statmod)
        likelihood =
            pnorm(colSums(c(gamma,1)*t(data[,3:5])))^d*
            dnorm(y - colSums(c(beta1,alpha1)*t(data[,c(3,5)])))^d*
            (1-pnorm(colSums(c(gamma,1)*t(data[,c(3:5)]))))^(1-d)*
            dnorm(y - colSums(c(beta0,alpha0)*t(data[,c(3,5)])))^(1-d)
        (likelihood * quad$weights[i])
    }
    return(-sum(log(combined)))
}





#===============================================
# Solving the model under various assumptions.
#===============================================

#----------------------------
# Data observed by researcher
#----------------------------
matrix2 = cbind(d,y,y1,y0,d_star,matrix)
write.csv(matrix2,file="dataset_a.csv",row.names=FALSE)

#----------------------------
# Suppose we observe everything
#-----------------------------
data1 = matrix2[,c(1,2,6,7,8)]
param1 = true_par
result.all = optim(par=param1, fn=f.value1,data=data1)
true_par
result.all$par

#----------------------------
# What happens if we just omit the factor?
#---------------------------
data2 = matrix2[,c(1,2,6,7)]
param2 = true_par[1:4]
result.omit = optim(par=param2, fn=f.value2,data=data2)
true_par
result.omit$par

result.omit = optim(par=c(param2,1,1,1), fn=f.value2.alt,data=data2)


#-------------------------------------------
# What happens if we integrate out the factor?
#-------------------------------------------
#result.int = optim(par=param1, fn=f.value3,data=data2)
#true_par
#result.all$par
#result.omit$par
#result.int$par

# show timing for various sample sized
# show how bias increases when alphas are larger.


#------------------------------------------
# Timing our new functions
#------------------------------------------
# Rcpp::sourceCpp('sbox/websites/2015-02-28johnerichumphries.github.io/teaching/code/RandomCoefficientsModel.cpp')
#
# system.time(result.int <- optim(par=param1, fn=f.value3,data=data2))
# system.time(result.int.par <- optim(par=param1, fn=f.value3.par,data=data2))
# system.time(result.int.cpp <- optim(par=param1, fn=f.value3.cpp,data=data2))
