# Softplus and Inverse Softplus Transformations

The softplus transformation maps unconstrained real values to the
positive real line. This is useful when parameters (e.g., variances)
must be strictly positive. The inverse softplus transformation recovers
the unconstrained value from a positive input.

## Usage

``` r
Softplus(x)

InvSoftplus(x)
```

## Arguments

- x:

  Numeric vector or matrix. Input values to be transformed.

## Value

- `Softplus()`: numeric vector or matrix of strictly positive values.

- `InvSoftplus()`: numeric vector or matrix of unconstrained values.

## Details

- `Softplus(x) = log(1 + exp(x))`

- `InvSoftplus(x) = log(exp(x) - 1)`

For numerical stability, these functions use
[`log1p()`](https://rdrr.io/r/base/Log.html) and
[`expm1()`](https://rdrr.io/r/base/Log.html) internally.

## See also

Other DTVAR Functions:
[`FitDTVARMxID()`](https://github.com/jeksterslab/fitDTVARMxID/reference/FitDTVARMxID.md),
[`LDL()`](https://github.com/jeksterslab/fitDTVARMxID/reference/LDL.md)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
# Apply softplus to unconstrained values
x <- c(-5, 0, 5)
y <- Softplus(x)

# Recover unconstrained values
x_recovered <- InvSoftplus(y)

y
#> [1] 0.006715348 0.693147181 5.006715348
x_recovered
#> [1] -5  0  5
```
