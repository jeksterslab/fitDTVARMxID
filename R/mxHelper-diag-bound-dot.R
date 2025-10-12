.MxHelperDiagBound <- function(p,
                               val) {
  diag(
    .MxHelperFullBound(
      m = p,
      n = p,
      val = val
    )
  )
}
