#' Joint recurrence quantification analysis
#'
#' @param x,y Numeric vectors
#' @param m Embedding dimension
#' @param tau Delay
#' @param eps Radius
#' @param theiler Theiler window
#' @return List with joint recurrence matrix and metrics
#' @export
jrqa_compute <- function(x, y, m = 2, tau = 1, eps, theiler = 0) {

  embed_ts <- function(z, m, tau) {
    n <- length(z)
    n_emb <- n - (m - 1) * tau
    out <- matrix(NA, n_emb, m)
    for (k in 1:m) out[, k] <- z[(1:n_emb) + (k - 1) * tau]
    out
  }

  X <- embed_ts(x, m, tau)
  Y <- embed_ts(y, m, tau)

  dist_mat <- function(M) {
    n <- nrow(M)
    D <- matrix(0, n, n)
    for (i in 1:n)
      for (j in i:n) {
        d <- sqrt(sum((M[i,] - M[j,])^2))
        D[i,j] <- d
        D[j,i] <- d
      }
    D
  }

  Rx <- dist_mat(X) <= eps
  Ry <- dist_mat(Y) <= eps
  R  <- Rx & Ry

  if (theiler > 0) {
    for (i in 1:nrow(R)) {
      j0 <- max(1, i - theiler)
      j1 <- min(nrow(R), i + theiler)
      R[i, j0:j1] <- FALSE
    }
  }

  list(R = R, metrics = jrqa_metrics(R))
}
