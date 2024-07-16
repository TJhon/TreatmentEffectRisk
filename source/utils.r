make.cvgroup <- function(n, K, right = TRUE) {
  split <- runif(n)
  return(as.numeric(cut(split, quantile(split, probs = seq(0, 1, 1 / K)), include.lowest = TRUE, right = right)))
}

make.cvgroup.balanced <- function(data, K, form_t) {
  cvgroup <- numeric(nrow(data))
  cvgroup[data[[form_t]] == 1] <- make.cvgroup(sum(data[[form_t]] == 1), K, right = TRUE)
  cvgroup[data[[form_t]] == 0] <- make.cvgroup(sum(data[[form_t]] == 0), K, right = FALSE)
  return(cvgroup)
}
wtdquantile <- function(y, w, g) {
  if (g >= 1) {
    max(y)
  } else {
    o <- order(y)
    y[o[which(cumsum(w[o]) >= sum(w) * g)[1]]]
  }
}
