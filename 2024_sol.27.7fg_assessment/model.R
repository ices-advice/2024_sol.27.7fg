## Run analysis, write model results


## Before: sol7fg_data.RData (SAM data object)
## After: SAM_fit_sol_7fg.RData (SAM fit object)


mkdir("model")

## Read SAM data object
load('data/sol7fg_data.RData')
dat.sol7fg$years

## Parametrisation

# Create a default parameter configuration object
conf <- defcon(dat.sol7fg)

# //1//biomass or catch survey for the tuning series
conf$keyBiomassTreat <- c(-1,-1,0,0,0,0,0)

# //2//define the fbar range
conf$fbarRange       <- c(3,8)

# //3//correlation between F-at-age
# Correlation of fishing mortality across ages (0 independent, 1 compound symmetry, or 2 AR(1)
conf$corFlag         <- 2          

# //4//number of parameters describing F-at-age
conf$keyLogFsta[1,] <- c(0, 1, 2, 3, 3, 3, 4, 4, 5, 5)

# //5//number of parameters in the suryey processes
conf$keyLogFpar[2,] <- c(0, 1, 2, 3, 3, -1, -1, -1, -1, -1)

# //6//variance of parameters on F
# use a single parameter!!!
# coupling of process variance parameters for log(F)-process (nomally only first row is used)   
conf$keyVarF[1,]   

# //7//variance parameters on the observations
conf$keyVarObs[1,1:2]   <- 0
conf$keyVarObs[1,3:10]  <- 1
conf$keyVarObs[2,1:5]   <- max(conf$keyVarObs[1,]) + c(1,2,2,3,3)                # max(conf$keyVarObs[1,]) + 1 #c (5,6,6,8,8) #
conf$keyVarObs[3:7,1]   <- (max(conf$keyVarObs[2,]) +1) : (max(conf$keyVarObs[2,]) + 5)

# //8//correlation at age between observations
conf$obsCorStruct    <- factor(c("AR","ID","ID","ID","ID","ID","ID"), levels = c("ID","AR","US")) 

# Coupling of correlation parameters can only be specified if the AR(1) structure is chosen above.
# NA's indicate where correlation parameters can be specified (-1 where they cannot).
conf$keyCorObs[1,]   <- 0 #NA #c(0,0,0,1,1,1,1,1,1)

## Fit the model
par                  <- defpar(dat.sol7fg , conf)
SAM_fit_sol_7fg      <- sam.fit(dat.sol7fg ,conf , par)

# Convergence checks
SAM_fit_sol_7fg$opt$convergence #zero
SAM_fit_sol_7fg$opt$message #"relative convergence (4)"

# Save SAM fit object to the model directory
save(SAM_fit_sol_7fg, file="model/SAM_fit_sol_7fg.RData")


