#' JRQA metrics from a joint recurrence matrix
#'
#' @param R Logical recurrence matrix
#' @param lmin Minimum diagonal line length
#' @return Named list of JRQA metrics
#' @export
jrqa_metrics <- function(R, lmin = 2) {
  stopifnot(is.matrix(R))
  R <- (R != 0)
  n <- nrow(R)

  RR <- sum(R) / (n * n)

  diag_runs <- function(mat) {
    lens <- integer(0)
    for (k in -(n - 1):(n - 1)) {
      v <- mat[row(mat) - col(mat) == k]
      if (length(v) == 0) next
      r <- rle(v)
      lens <- c(lens, r$lengths[r$values])
    }
    lens
  }

  dl <- diag_runs(R)
  dl_det <- dl[dl >= lmin]

  DET <- if (sum(dl) == 0) NA else sum(dl_det) / sum(dl)
  Lmax <- if (length(dl_det) == 0) NA else max(dl_det)
  Lmean <- if (length(dl_det) == 0) NA else mean(dl_det)

  ENT <- NA
  if (length(dl_det) > 0) {
    p <- table(dl_det) / sum(table(dl_det))
    ENT <- -sum(p * log(p))
  }

  list(RR = RR, DET = DET, Lmax = Lmax, Lmean = Lmean, ENT = ENT)
}
