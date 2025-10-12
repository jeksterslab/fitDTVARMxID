.MxHelperSymBound <- function(p,
                              val) {
  out <- .MxHelperFullBound(
    m = p,
    n = p,
    val = val
  )
  out[upper.tri(out)] <- t(out)[upper.tri(out)]
  out
}
