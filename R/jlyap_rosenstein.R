#' Largest joint Lyapunov exponent (Rosenstein)
#'
#' @param Z Joint embedded trajectory
#' @param theiler Theiler window
#' @param k_max Max forward steps
#' @param fit_range Range used for slope fit
#' @return List with lambda and divergence curve
#' @export
jlyap_rosenstein <- function(Z, theiler = 10,
                             k_max = 50,
                             fit_range = 5:30) {

  n <- nrow(Z)

  dist_ij <- function(i, j)
    sqrt(sum((Z[i,] - Z[j,])^2))

  nn <- rep(NA_integer_, n)
  for (i in 1:(n - k_max)) {
    dmin <- Inf
    for (j in setdiff(1:(n - k_max),
                      (i-theiler):(i+theiler))) {
      d <- dist_ij(i, j)
      if (d < dmin) { dmin <- d; nn[i] <- j }
    }
  }

  div <- rep(NA, k_max)
  for (k in 1:k_max) {
    vals <- c()
    for (i in 1:(n - k_max)) {
      j <- nn[i]
      if (!is.na(j)) {
        d <- dist_ij(i + k, j + k)
        if (d > 0) vals <- c(vals, log(d))
      }
    }
    div[k] <- mean(vals, na.rm = TRUE)
  }

  fit <- stats::lm(div[fit_range] ~ fit_range)
  list(lambda = stats::coef(fit)[2],
       divergence = data.frame(k = 1:k_max, log_d = div),
       fit = fit)
}

