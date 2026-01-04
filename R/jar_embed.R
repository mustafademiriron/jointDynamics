#' Joint attractor reconstruction
#'
#' @param x,y Numeric vectors
#' @param m Embedding dimension
#' @param tau Delay
#' @param method concat or multivar
#' @return Joint embedded trajectory
#' @export
jar_embed <- function(x, y, m = 2, tau = 1,
                      method = c("concat", "multivar")) {

  method <- match.arg(method)

  embed_one <- function(z) {
    n <- length(z)
    n_emb <- n - (m - 1) * tau
    out <- matrix(NA, n_emb, m)
    for (k in 1:m) out[, k] <- z[(1:n_emb) + (k - 1) * tau]
    out
  }

  X <- embed_one(x)
  Y <- embed_one(y)
  n <- min(nrow(X), nrow(Y))
  X <- X[1:n, ]; Y <- Y[1:n, ]

  if (method == "concat") {
    cbind(X, Y)
  } else {
    Z <- matrix(NA, n, 2 * m)
    for (k in 1:m) {
      Z[, 2*k - 1] <- X[, k]
      Z[, 2*k]     <- Y[, k]
    }
    Z
  }
}
