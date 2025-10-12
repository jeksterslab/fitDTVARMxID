.MxHelperDiagValues <- function(p,
                                val) {
  diag(
    .MxHelperFullValues(
      m = p,
      n = p,
      val = val
    )
  )
}
