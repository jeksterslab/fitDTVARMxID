# Check Model Convergence

Evaluate whether OpenMx fit has converged successfully.

## Usage

``` r
converged(object, ...)

# S3 method for class 'dtvarmxid'
converged(
  object,
  grad_tol = 0.01,
  hess_tol = 1e-08,
  vanishing_theta = TRUE,
  theta_tol = 0.001,
  prop = FALSE,
  ...
)
```

## Arguments

- object:

  An object of class `dtvarmxid`.

- ...:

  Passed to and/or used by methods.

- grad_tol:

  Numeric scalar. Tolerance for the maximum absolute gradient.

- hess_tol:

  Numeric scalar. Tolerance for Hessian eigenvalues; eigenvalues must be
  strictly greater than this value.

- vanishing_theta:

  Logical. Test for measurement error variance going to zero.

- theta_tol:

  Numeric. Tolerance for vanishing theta test.

- prop:

  Logical. If `prop = FALSE`, a named logical vector indicating, for
  each individual fit, whether the convergence criteria are met. If
  `prop = TRUE`, the proportion of cases that converged.

## Value

For the `dtvarmxid` method: If `prop = FALSE`, a named logical vector
indicating, for each individual fit, whether the convergence criteria
are met. If `prop = TRUE`, the proportion of cases that converged.

## Details

Convergence is defined by three criteria:

1.  Status code equals `0L`.

2.  The maximum absolute gradient is below `grad_tol`.

3.  The Hessian is positive definite with all eigenvalues greater than
    `hess_tol`.

4.  If `vanishing_theta = TRUE`, the model additionally checks that the
    diagonal elements of the measurement error covariance matrix
    (\\\Theta\\) are not vanishingly small, where “small” is defined by
    `theta_tol`.

## Author

Ivan Jacob Agaloos Pesigan
