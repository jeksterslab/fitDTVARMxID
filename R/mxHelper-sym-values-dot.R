.MxHelperSymValues <- function(p,
                               val) {
  out <- .MxHelperFullValues(
    m = p,
    n = p,
    val = val
  )
  out[upper.tri(out)] <- t(out)[upper.tri(out)]
  out
}
