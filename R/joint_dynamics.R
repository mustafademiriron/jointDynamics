#' Integrated joint dynamics pipeline
#'
#' @param x,y Numeric vectors
#' @param m,tau Embedding parameters
#' @param eps Radius for recurrence (NULL skips JRQA)
#' @param jrqa_space Where to compute JRQA: "xy" (separate + AND) or "Z" (joint attractor)
#' @param theiler Theiler window for JRQA
#' @param lmin Minimum diagonal line length for JRQA metrics
#' @param norm Distance norm for jrqa_space="Z": "euclidean" or "max"
#' @return List with joint embedding, JRQA, and JLE
#' @export
joint_dynamics <- function(x, y,
                           m = 2, tau = 1,
                           eps = NULL,
                           jrqa_space = c("xy", "Z"),
                           theiler = 0,
                           lmin = 2,
                           norm = c("euclidean", "max")) {

  jrqa_space <- match.arg(jrqa_space)
  norm <- match.arg(norm)

  Z <- jar_embed(x, y, m = m, tau = tau)

  jrqa_out <- NULL
  if (!is.null(eps)) {
    if (jrqa_space == "xy") {
      jrqa_out <- jrqa_compute(x, y, m = m, tau = tau, eps = eps, theiler = theiler)
      # jrqa_compute uses jrqa_metrics(R) with default lmin=2; if you want lmin here,
      # we can add it to jrqa_compute too. For now, keep jrqa_compute as-is.
    } else {
      jrqa_out <- jrqa_compute_Z(Z, eps = eps, theiler = theiler, lmin = lmin, norm = norm)
    }
  }

  jle_out <- jlyap_rosenstein(Z)

  list(
    Z = Z,
    jrqa = jrqa_out,
    jle = jle_out
  )
}
