cvar_calculate <- function(data, p, tau = 'tau', sw='sw', method_if = cvar_if1){
  tau_ref = data |> pull(tau)
  sw_ref = data |> pull(sw)
  q <- wtdquantile(tau_ref, sw_ref, p)
  # print("IF")
  IF = method_if(data, p, q, tau=tau)
  cvar = IF * sw_ref
  CVaR = mean(cvar, na.rm=T)
  CVaR.se = sd(cvar, na.rm=T) / sqrt(nrow(data))
  return(tibble(CVaR, CVaR.se, p = p, q = q))
}