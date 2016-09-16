################################################################################
# Name:         van Genuchten model for SWC and SWP
# Author(s):    Johannes Brenner
#                   original code inside DataBaseAlpEnvEURAC R pkg
#               Andrea Balotti - balotti.and at gmail.com
#                   inversion and integration of geotop range limit
#
# Copyright (c), 2016 - Andrea Balotti
################################################################################

### NOTE
# van Genuchten inversion taken from GEOtop model "pedo.funct.c" function
# https://github.com/geotopmodel
# Saturation condition is considered same as unsatured in this script because
# otherwise specific storativity (Ss) parameter is also required. 

### VARIABLE
# swc           SWC (water content)     [m^3/m^3]   [0 < x < 1] -> non-% value
# psi           SWP (suction pressure)  [cm]        [<= 0]
# alpha         air entry suction       [cm^-1]     [> 0]
# n             pore-size distribution  [-]         [> 1]
# theta_res     residual water content  [m^3/m^3]   [> 0]
# theta_sat     saturated water content [m^3/m^3]   [> 0]

vanGenuchten_swc <- function(psi=NULL, swc=NULL, alpha, n, theta_res, theta_sat, inv = FALSE)
{
    m <- 1-(1/n)        # utility variable
    pmin <- -1*10^10    # minimum SWP value [Pa]
    
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
            # geotop inversion (return negative SWP) [Pa/hPa/cm ???]
            # (does square of negative value (NaN) if SWC is a percent value!!!)
            out <- ( ( (TETA^(-1/m))-1 )^(1/n) )*(-1/alpha)
            # force lower SWP value 
            out[TETA > 1-10^(-6)] <- 0
            
            # if (geotop) {
                # geotop inversion (return negative SWP)
                # out <- ( ( (TETA^(-1/m))-1 )^(1/n) )*(-1/alpha)
            # } else {
                # handmade inversion (return positive SWP) - DOESN'T WORK WELL
                # out <- ( ( (exp(log(Theta)/m)-1)/alpha )*(1/n) )
            # }
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
# alpha = c(1.333047,1.266704)
# n = c(1.388409,1.398214)
# theta_res = c(0.05,0.05)
# theta_sat = c(0.5481991,0.5482869)
# psi <- c(seq(1,100000,5),seq(1,100000,5))
# psi <- -psi+sin(rnorm(length(psi)))
# k=1
# swc = vanGenuchten_swc(psi = psi, alpha = alpha[k], n = n[k],
#                        theta_sat = theta_sat[k], theta_res = theta_res[k])
# psi2 = vanGenuchten_swc(swc = swc, alpha = alpha[k], n = n[k],
#                         theta_sat = theta_sat[k], theta_res = theta_res[k], inv = T)
# plot(log(abs(psi),base = 10),swc)
# plot(log(abs(psi2),base = 10),swc)
###