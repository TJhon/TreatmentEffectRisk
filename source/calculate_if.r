
cvar_if1 <- function(
    data, p, q, mu1_col = "mu1", mu0_col="mu0", A_col="A", 
    ipw_col="ipw", Y_col="Y", tau_col="tau") {
  # Intermediate calculations
  # print(q)
  difference <- data[[mu1_col]] - data[[mu0_col]]
  weighted_difference <- 
    (2 * data[[A_col]] - 1) * 
    data[[ipw_col]] * (data[[Y_col]] - data[[A_col]] * data[[mu1_col]] - 
                         (1 - data[[A_col]]) * data[[mu0_col]])
  # print(data[[tau_col]])
  condition <- data[[tau_col]] <= q
  
  # Calculation
  result <- q + (difference + weighted_difference - q) * (condition) / p
  
  return(result)
}



cvar_if_plugin1 <- function(data, p, q, tau='tau') {
  # Intermediate calculations
  tau <- data |> pull(tau)
  condition <- tau <= q
  
  # Calculation
  result <- q + (tau - q) * (condition) / p
  
  return(result)
}

cvar_if_bad <- function(q, mu1, mu0, A, ipw, Y, p, tau_bad) {
  # Intermediate calculations
  difference <- mu1 - mu0
  weighted_difference <- (2 * A - 1) * ipw * (Y - A * mu1 - (1 - A) * mu0)
  condition <- tau_bad <= q
  
  # Calculation
  result <- q + (difference + weighted_difference - q) * (condition) / p
  
  return(result)
}

cvar_if_bad1 <- function(data, p, q, tau = "tau.bad", mu1='mu1', mu0='mu0', A='A', ipw='ipw', Y='Y') {
  # Intermediate calculations
  mu1 <- data |> pull(mu1)
  mu0 <- data |> pull(mu0)
  A <- data |> pull(A)
  ipw <- data |> pull(ipw)
  Y <- data |> pull(Y)
  
  # 
  tau <- data |> pull(tau)
  print(tau |> head())
  # 
  
  difference <- mu1 - mu0
  weighted_difference <- (2 * A - 1) * ipw * (Y - A * mu1 - (1 - A) * mu0)
  condition <- tau <= q
  
  # Calculation
  result <- q + (difference + weighted_difference - q) * (condition) / p
  
  return(result)
}

cvar_if_tauate <- function(q, p, mu1, mu0, A, ipw, Y, tau) {
  # Intermediate calculations
  difference <- mu1 - mu0
  weighted_difference <- (2 * A - 1) * ipw * (Y - A * mu1 - (1 - A) * mu0)
  condition <- tau <= q
  
  # Calculation
  result <- q + (difference + weighted_difference) * ((condition) / p - 1) - q * (condition) / p
  
  return(result)
}
cvar_if_tauate1 <- function(data, p, q, mu1='mu1', mu0='mu0', A='A', ipw='ipw', Y='Y', tau='tau') {
  
  mu1 <- data |> pull(mu1)
  mu0 <- data |> pull(mu0)
  A <- data |> pull(A)
  ipw <- data |> pull(ipw)
  Y <- data |> pull(Y)
  tau <- data |> pull(tau)
  
  # Intermediate calculations
  difference <- mu1 - mu0
  weighted_difference <- (2 * A - 1) * ipw * (Y - A * mu1 - (1 - A) * mu0)
  condition <- tau <= q
  
  # Calculation
  result <- q + (difference + weighted_difference) * ((condition) / p - 1) - q * (condition) / p
  
  return(result)
}



cvar_if_bbouns_ate <- function(q, p, mu1, mu0, A, ipw, Y, tau, varsum01, rho, sdprod01) {
  # Intermediate calculations
  difference <- mu1 - mu0
  weighted_difference <- (2 * A - 1) * ipw * (Y - A * mu1 - (1 - A) * mu0)
  condition <- tau - q <= q
  sqrt_term <- sqrt((tau - q)^2 + varsum01 - 2 * rho * sdprod01)
  
  # Calculation
  term1 <- -weighted_difference
  term2 <- (tau - q - sqrt_term) / (2 * p)
  term3 <- (1 - (tau - q) / sqrt_term) * weighted_difference / (2 * p)
  
  result <- term1 + q + term2 + term3
  
  return(result)
}

cvar_if_bbouns_ate1 <- function(
    data, p, q, 
    mu1 = 'mu1', 
    mu0 = 'mu0', 
    A = 'A', 
    ipw = 'ipw', 
    Y = 'Y', 
    tau = 'tau', 
    varsum01 = 'varsum01', 
    rho = 'rho', 
    sdprod01 = 'sdprod01'
    ) {
  # Intermediate calculations
  
  mu1 <- data |> pull(mu1)
  mu0 <- data |> pull(mu0)
  A <- data |> pull(A)
  ipw <- data |> pull(ipw)
  Y <- data |> pull(Y)
  tau <- data |> pull(tau)
  varsum01 <- data |> pull(varsum01)
  rho <- data |> pull(rho)
  sdprod01 <- data |> pull(sdprod01)
  
  difference <- mu1 - mu0
  weighted_difference <- (2 * A - 1) * ipw * (Y - A * mu1 - (1 - A) * mu0)
  condition <- tau - q <= q
  sqrt_term <- sqrt((tau - q)^2 + varsum01 - 2 * rho * sdprod01)
  
  # Calculation
  term1 <- -weighted_difference
  term2 <- (tau - q - sqrt_term) / (2 * p)
  term3 <- (1 - (tau - q) / sqrt_term) * weighted_difference / (2 * p)
  
  result <- term1 + q + term2 + term3
  
  return(result)
}


summarise_IF <- function(data, p) {
  data %>%
    summarise(
      p = p,
      CVaR = mean(sw * IF, na.rm = TRUE),
      CVaR.se = sd(sw * IF, na.rm = TRUE) / sqrt(n()))
}

# groups

cvar_bbound_mate <- function(q, p, mu1, mu0, A, ipw, Y, tau, b) {
  weighted_difference = (2 * A - 1) * ipw * (Y - A * mu1 - (1 - A) * mu0)
  mu = mu1 - mu0
  condition = tau - b <= q

  result <- -(mu + weighted_difference) +
    q +
    (mu + weighted_difference - q - b) * (condition) / (2 * p) +
    (mu + weighted_difference - q + b) * (condition) / (2 * p)
  
  return(result)
}
