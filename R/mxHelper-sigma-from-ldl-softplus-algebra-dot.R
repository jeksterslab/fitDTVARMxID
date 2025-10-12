.MxHelperSigmaFromLDLSoftplusAlgebra <- function(sdiag,
                                                 column,
                                                 iden,
                                                 name) {
  OpenMx::mxAlgebraFromString(
    algString = paste0(
      "(",
      sdiag,
      "+",
      iden,
      ")",
      "%*%",
      "(vec2diag(mxRobustLog(1 + exp(",
      column,
      ")) + 1e-8))",
      "%*%",
      "t(",
      sdiag,
      "+",
      iden,
      ")"
    ),
    name = name
  )
}
