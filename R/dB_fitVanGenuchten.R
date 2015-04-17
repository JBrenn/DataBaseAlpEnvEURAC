# function to fit vanGenuchten Model - SoilWaterRetention Curve to observed data

dB_fitVanGenuchten <- function(data, par0, theta_res, theta_sat)
{
  
  rmse_optim <- function(data, par, theta_res, theta_sat)
  {
    swc_vG <- vanGenuchten_swc(par = par, psi=data[,2], theta_res = theta_res, theta_sat = theta_sat)
    
    rmse <- sqrt( sum((data[,1]-swc_vG)^2) / length(data[,1]) )
  }
  
  optim(par = par0, fn = rmse_optim, theta_res=theta_res, theta_sat=theta_sat, data=data, method = "L-BFGS-B", 
        lower=c(0.01,0.01), upper=c(1.5,9.5))
}

