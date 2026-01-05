# jointDynamics

<!-- badges: start -->
[![R-CMD-check](https://github.com/mustafademiriron/jointDynamics/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mustafademiriron/jointDynamics/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`jointDynamics` provides tools for analyzing **joint nonlinear dynamics** in coupled time series, including:

- Joint Recurrence Quantification Analysis (JRQA) in joint state space (`"xy"`) or reconstructed joint attractor space (`"Z"`)
- Joint attractor reconstruction utilities (embedding)
- Joint Lyapunov exponent estimation (Rosenstein-style)

## Installation

You can install the development version from GitHub:

```r
install.packages("devtools")
devtools::install_github("mustafademiriron/jointDynamics")

```

## Example

library(jointDynamics)

set.seed(1)
x <- rnorm(300)
y <- x + rnorm(300, 0.3)

res <- joint_dynamics(
  x, y,
  eps = 1,
  jrqa_space = "Z",
  theiler = 10
)

res$jrqa$metrics
res$jle$lambda


res$jrqa$metrics
res$jle$lambda


