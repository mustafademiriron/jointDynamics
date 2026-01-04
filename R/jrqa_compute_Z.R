#' Joint recurrence quantification analysis on a joint attractor
#'
#' Computes a recurrence matrix from a joint embedded trajectory Z
#' (e.g., output of jar_embed()) and returns JRQA metrics.
#'
#' @param Z Numeric matrix, joint embedded trajectory (rows = time, cols = dims)
#' @param eps Numeric, radius threshold for recurrence
#' @param theiler Integer, Theiler window to exclude near-diagonal recurrences
#' @param lmin Integer, minimum diagonal line length for DET-related metrics
#' @param norm Character, distance norm: "euclidean" or "max" (Chebyshev)
#' @return List with recurrence matrix R and JRQA metrics
#' @export
jrqa_compute_Z <- function(Z, eps, theiler = 0, lmin = 2,
                           norm = c("euclidean", "max")) {

  stopifnot(is.matrix(Z))
  if (!is.numeric(eps) || length(eps) != 1 || eps <= 0) {
    stop("`eps` must be a single positive number.")
  }
  norm <- match.arg(norm)

  n <- nrow(Z)

  # Distance between two rows
  dist_ij <- function(i, j) {
    d <- abs(Z[i, ] - Z[j, ])
    if (norm == "euclidean") sqrt(sum(d^2)) else max(d)
  }

  # Build recurrence matrix
  R <- matrix(FALSE, n, n)
  for (i in 1:n) {
    R[i, i] <- TRUE
    if (i < n) {
      for (j in (i + 1):n) {
        is_rec <- (dist_ij(i, j) <= eps)
        R[i, j] <- is_rec
        R[j, i] <- is_rec
      }
    }
  }

  # Apply Theiler window
  if (theiler > 0) {
    for (i in 1:n) {
      j0 <- max(1, i - theiler)
      j1 <- min(n, i + theiler)
      R[i, j0:j1] <- FALSE
    }
  }

  list(R = R, metrics = jrqa_metrics(R, lmin = lmin))
}
