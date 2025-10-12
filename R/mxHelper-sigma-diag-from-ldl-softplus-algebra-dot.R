.MxHelperSigmaDiagFromLDLSoftplusAlgebra <- function(column,
                                                     name) {
  OpenMx::mxAlgebraFromString(
    algString = paste0(
      "vec2diag(mxRobustLog(1 + exp(",
      column,
      ")) + 1e-8)"
    ),
    name = name
  )
}
