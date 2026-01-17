# Parameter Estimates

Parameter Estimates

## Usage

``` r
# S3 method for class 'dtvarmxid'
coef(
  object,
  mu_eta = TRUE,
  alpha = TRUE,
  beta = TRUE,
  nu = TRUE,
  psi = TRUE,
  theta = TRUE,
  converged = TRUE,
  grad_tol = 0.01,
  hess_tol = 1e-08,
  vanishing_theta = TRUE,
  theta_tol = 0.001,
  ...
)
```

## Arguments

- object:

  Object of class `dtvarmxid`.

- mu_eta:

  Logical. If `mu_eta = TRUE`, include estimates of the `mu_eta` vector,
  if available. If `mu_eta = FALSE`, exclude estimates of the `mu_eta`
  vector.

- alpha:

  Logical. If `alpha = TRUE`, include estimates of the `alpha` vector,
  if available. If `alpha = FALSE`, exclude estimates of the `alpha`
  vector.

- beta:

  Logical. If `beta = TRUE`, include estimates of the `beta` matrix, if
  available. If `beta = FALSE`, exclude estimates of the `beta` matrix.

- nu:

  Logical. If `nu = TRUE`, include estimates of the `nu` vector, if
  available. If `nu = FALSE`, exclude estimates of the `nu` vector.

- psi:

  Logical. If `psi = TRUE`, include estimates of the `psi` matrix, if
  available. If `psi = FALSE`, exclude estimates of the `psi` matrix.

- theta:

  Logical. If `theta = TRUE`, include estimates of the `theta` matrix,
  if available. If `theta = FALSE`, exclude estimates of the `theta`
  matrix.

- converged:

  Logical. Only include converged cases.

- grad_tol:

  Numeric scalar. Tolerance for the maximum absolute gradient if
  `converged = TRUE`.

- hess_tol:

  Numeric scalar. Tolerance for Hessian eigenvalues; eigenvalues must be
  strictly greater than this value if `converged = TRUE`.

- vanishing_theta:

  Logical. Test for measurement error variance going to zero if
  `converged = TRUE`.

- theta_tol:

  Numeric. Tolerance for vanishing theta test if `converged` and
  `theta_tol` are `TRUE`.

- ...:

  additional arguments.

## Value

Returns a list of vectors of parameter estimates.

## Author

Ivan Jacob Agaloos Pesigan
