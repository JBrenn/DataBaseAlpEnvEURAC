# van Genuchten model
# van Genuchten inversion taken from GeoTop model "pedo.funct.c" function

# swc           SWC (water content)     [m^3/m^3]   [> 0]
# psi           SWP (suction pressure)  [cm]        [<= 0]
# alpha         air entry suction       [cm^-1]     [> 0]
# n             pore-size distribution  [-]         [> 1]
# theta_res     residual water content  [m^3/m^3]   [> 0]
# theta_sat     saturated water content [m^3/m^3]   [> 0]

vanGenuchten_swc <- function(psi=NULL, swc=NULL, alpha, n, theta_res, theta_sat, inv = FALSE)
{
    # utility variable
    m = 1-1/n
    
    if (inv) {
    # compute SWP from SWC
        if (is.null(swc)) stop("SWC must be provided to compute SWP.")
        out <- ( ( (swc^-1/m)-1 )^1/n )*-1/alpha

    } else {
    # compute SWC from SWP
        if (is.null(psi)) stop("SWP must be provided to compute SWC.")
        out <-  theta_res + (theta_sat - theta_res) / (1 + (alpha * abs(psi))^n )^m
    }
    
    return(out)
}

# plot(psi,swc,xlim = c(max(psi),min(psi)))
# plot(psi2,swc,xlim = c(max(psi2),min(psi2)))