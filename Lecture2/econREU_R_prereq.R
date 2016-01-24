#=======================================================
#
# Economics REU: Applied Micro and R Prerequisites (2015)
# Author: John Eric Humphries
# Date: 06-23-2015
#
#
#========================================================


#=============
# Installing packages (checks if they are installed first), then loading packages
#=============
mPackages <- installed.packages()
# Details of installed packages
stInstalled <- rownames( mPackages )
# Isolate thep package names
stRequired <- c( 'ggplot2', 'reshape2', 'knitr', 'mvtnorm','data.table','dplyr','AER')
#  The required packages

for ( stName in stRequired ){
    if ( !( stName %in% stInstalled ) ){
        cat('****************** Installing ', stName, '****************** \n')
        install.packages( stName )
    }
    library( stName, character.only=TRUE )
}

#==========
# Simply checking the work makes senes
#==========
x = 2
y = 3
z = x + y
print(z)

#==========
# Checking libraries worked
#==========

draws <- rmvnorm(200,mean=c(0,0), sigma = diag(c(1,1)))

#========
# Checking plots
#========
plot(draws)
qplot(x=draws[,1], y = draws[,2])