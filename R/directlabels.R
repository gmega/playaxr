#' Bump labels up, but drops some if too crammed
#'
#' Label positioning strategy similar to \code{\link{bumpup}}, but with a cap
#' on the maximum vertical displacement that can be applied to a label. If
#' the required displacement overshoots the cap, the label is dropped. This
#' creates an effect similar to the labelling strategy seen in Tableau,
#' except with better results.
#'
#' @param d the directlabels positioning dataframe.
#'
#' @param max_bump the maximum bump factor allowed before a label is dropped.
#'                 Expressed as a factor of the label's height.
#'
#' @export
bumpup_bounded <- function(d, max_bump = 1.1, ...) {
  d <- calc.boxes(d)[order(d$y),]

  # keeps track of cumulative nudge
  cumulative_nudge <- 0
  drop <- c()
  j <- 1

  # scans from the bottom up
  # TODO this does not check if boxes overlap horizontally or not. Like bumpup,
  #      we have to check for that and leave them alone in these cases.
  for (i in 2:nrow(d)) {
    nudge_limit <- d$h[i] * max_bump
    nudge <- d$bottom[i] - d$top[j]

    if (nudge < 0) {
      # total nudge is current plus whatever we already
      # applied in the boxes below
      cumulative_nudge <- cumulative_nudge + abs(nudge)
    } else {
      nudge <- 0
    }

    # If the cumulative nudge gets too large, drops labels until
    # we get back to an acceptable nudge.
    if (cumulative_nudge > nudge_limit) {
      drop <- c(drop, i)
      cumulative_nudge <- cumulative_nudge - abs(nudge)
      next
    }
    # Otherwise, applies nudge.
    d$bottom[i] <- d$bottom[i] - nudge
    d$top[i] <- d$top[i] - nudge
    d$y[i] <- d$y[i] - nudge
    j <- i
  }

  d[!(1:nrow(d) %in% drop),]
}
