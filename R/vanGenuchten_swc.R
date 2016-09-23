################################################################################
# Name:         van Genuchten model for SWC and SWP
# Author(s):    Johannes Brenner
#                   original code inside DataBaseAlpEnvEURAC R pkg
#               Andrea Balotti - balotti.and at gmail.com
#                   inversion and integration of geotop range limit
#
# Copyright (c) 2016 - Andrea Balotti
################################################################################

### NOTE
# van Genuchten inversion taken from GEOtop model "pedo.funct.c" function
# https://github.com/geotopmodel
# By default the function compute SWC from SWP, using "inv=TRUE" will be computed
# the opposite.
# Saturation condition is considered same as unsatured in this script because
# otherwise specific storativity (Ss) parameter is also required.
# Conversion between column water and pressure:
#   1 mm = 0.0980665 hPa / 0.00980665 kPa
#   1 cm = 0.980665 hPa / 0.0980665 kPa

### VARIABLE (the measure unit are to check !!!)
# swc           SWC (water content)     [cm^3/cm^3] [0 < x < 1] -> non-% value
# psi           SWP (pressure head)     [cm]        [<= 0] -> forced to absolute value in computation
# alpha         air entry suction       [cm^-1]     [> 0]
# n             pore-size distribution  [-]         [> 1]
# theta_res     residual water content  [cm^3/cm^3] [> 0]
# theta_sat     saturated water content [cm^3/cm^3] [> 0]

vanGenuchten_swc <- function(psi=NULL, swc=NULL, alpha, n, theta_res, theta_sat, inv = FALSE)
{
    # # convert input in numeric array
    # for (i in c(psi, swc, alpha, n, theta_res, theta_sat)) {
    #     if (!is.null(i)) as.numeric(i)
    #     # print(str(i))
    # }
    # 
    print(c(alpha,n,theta_res,theta_sat))
    
    # constant
    m <- 1-(1/n)        # utility variable
    pmin <- -1*10^10    # minimum SWP value [Pa] (10^10 [Pa] = 9806650 [cm])
    ss <- 10^-6         # specific storativity (correction for saturated case),
                        # maybe, could be omitted in our case
    
    if (inv) {
    # compute SWP from SWC
        
        # check SWC data quality
        if (is.null(swc)) {
            stop("SWC must be provided to compute SWP.")
        } else {
            # SWC must be non-percent value
            if (!mean(swc,na.rm = T) < 1) {
                swc <- swc/100
                warning("SWC provided as % values. Has been rescaled in [0-1] range.")
            }
            
            # force soil ice content as zero
            ice=0
            
            # utility variable [dimensionless]
            # minimum value
            TETA_min <- 1 / ( 1+(alpha*abs(pmin))^n )^m
            # unsaturated case
            TETA <- (swc-theta_res)/(theta_sat-theta_res)
            # dry case
            TETA[TETA<TETA_min] <- TETA_min
            
            # INVERSION FORMULA
            # geotop inversion (return negative SWP) [cm]
            # (does square of negative value (NaN) if SWC is a percent value!!!)
            out <- ( ( (TETA^(-1/m))-1 )^(1/n) )*(-1/alpha)

            # force lower SWP value 
            out[TETA > 1-10^(-6)] <- 0
            
            ### TO CHECK AND REMOVE ######################
            # if (geotop) {
                # geotop inversion (return negative SWP)
                # out <- ( ( (TETA^(-1/m))-1 )^(1/n) )*(-1/alpha)
            # } else {
                # handmade inversion (return positive SWP) - DOESN'T WORK WELL
                # out <- ( ( (exp(log(TETA)/m)-1)/alpha )*(1/n) )
            # }
            ##############################################
        }

    } else {
    # compute SWC from SWP
        
        if (is.null(psi)) {
            stop("SWP must be provided to compute SWC.")
        } else {
            # SWP must be negative values
            if (!mean(psi,na.rm = T) < 0) {
                psi <- -psi
                warning("SWP provided as positive values. Has been converted to negative values.")
            }
            
            # utility variable
            # minimum SWP value
            psi[psi<pmin] <- pmin
            # unsaturated case
            TETA <- 1 / (1 + (alpha * abs(psi))^n )^m
            # dry case
            TETA[psi>-1*10^(-6)] <- 0
            
            # VAN GENUCHTEN FORMULA
            # usual formula (return positive SWC in range [0-1])
            out <- theta_res + (theta_sat - theta_res) * TETA
        }
    }
    
    return(out)
}

### TEST CODE
alpha = c(0.005,1.333047,1.266704)
n = c(2,1.388409,1.398214)
theta_res = c(0.1,0.05,0.05)
theta_sat = c(0.5,0.5481991,0.5482869)
swc <- runif(1000,0.1784,0.5) # put same limit as out_swc
psi <- runif(1000,min=0,max=1000)
k=1
out_swc = vanGenuchten_swc(psi = psi, alpha = alpha[k], n = n[k],
                       theta_sat = theta_sat[k], theta_res = theta_res[k])
out_psi = vanGenuchten_swc(swc = swc, alpha = alpha[k], n = n[k],
                        theta_sat = theta_sat[k], theta_res = theta_res[k], inv = T)
plot(abs(psi),out_swc)
plot(abs(out_psi),swc)
# plot(psi)
# plot(swc)
# # plot pedofunction
plot(swc,abs(psi),log="y")
plot(out_swc,abs(out_psi),log="y")
# # plot pF between 0-7
# plot(swc,log((abs(psi)),base = 10),ylim=c(0,7))
###