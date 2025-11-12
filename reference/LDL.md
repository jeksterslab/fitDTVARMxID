# LDL' Decomposition of a Symmetric Positive-Definite Matrix

Performs an LDL' factorization of a symmetric positive-definite matrix
\\X\\, such that \$\$X = L D L^\top,\$\$ where \\L\\ is unit
lower-triangular (ones on the diagonal) and \\D\\ is diagonal.

## Usage

``` r
LDL(x)
```

## Arguments

- x:

  Numeric matrix. Must be symmetric positive-definite.

## Value

A list with components:

- l_mat_unit:

  Unit lower-triangular matrix \\L\\.

- l_mat_strict:

  Strictly lower-triangular part of \\L\\.

- d_mat:

  Diagonal matrix \\D\\.

- d_vec:

  Vector of diagonal entries of \\D\\.

- d_uc:

  Unconstrained vector with \\\mathrm{softplus}(d\\uc) = d\\vec\\.

- x:

  Original input matrix.

- y:

  Reconstructed matrix \\L D L^\top\\.

- diff:

  Difference `x - y`.

## Details

This function returns both the unit lower-triangular factor \\L\\ and
the diagonal factor \\D\\. The strictly lower-triangular part of \\L\\
is also provided for convenience. The function additionally computes an
unconstrained vector `d_uc` such that `softplus(d_uc) = d_vec`, using
\\\mathrm{softplus}^{-1}(y) = \log(\exp(y) - 1)\\ for stable
back-transformation.

## See also

Other DTVAR Functions:
[`FitDTVARMxID()`](https://github.com/jeksterslab/fitDTVARMxID/reference/FitDTVARMxID.md),
[`Softplus()`](https://github.com/jeksterslab/fitDTVARMxID/reference/Softplus.md)

## Examples

``` r
set.seed(123)
A <- matrix(rnorm(16), 4, 4)
S <- crossprod(A) + diag(1e-6, 4) # SPD
out <- LDL(S)
max(abs(out$diff))
#> [1] 4.440892e-16
```
