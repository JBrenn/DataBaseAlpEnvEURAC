# van Genuchten model

vanGenuchten_swc <- function(par, psi, theta_res, theta_sat)
{
  swc <- theta_res + (theta_sat - theta_res) / (1 + (par["alpha"] * abs(psi))^par["n"] )^(1-1/par["n"]) 
  return(swc)
}