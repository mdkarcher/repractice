enum_stack = function(x, count) {
  data.frame(x = rep(x, count), Count = 1:count)
}

hist_to_stacks = function(hist_out) {
  result = NULL
  for (index in seq_along(hist_out$mids)) {
    if (hist_out$counts[index] > 0) {
      result = rbind(result, enum_stack(hist_out$mids[index], hist_out$counts[index]))
    }
  }
  return(result)
}

hist_to_dotplot = function(hist_out, xlim=NULL, ylim=NULL, ...) {
  stacks = hist_to_stacks(hist_out)
  if (is.null(xlim)) {
    xlim = range(hist_out$breaks)
  }
  if (is.null(ylim)) {
    ylim = c(0, max(hist_out$counts)+1)
  }
  plot(stacks, xlim=xlim, ylim=ylim, ...)
}

#' Dot plot (histograms)
#'
#' @param x vector of values to be plotted.
#' @param breaks passed through to [hist()].
#' @param xlim,ylim the range of x and y values to visualize.
#' @param plot logical if `TRUE` (default) the dot plot is plotted.
#' @param xlab,ylab axis labels.
#' @param ... further arguments passed through to [plot()].
#'
#' @return histogram object underlying the dot plot.
#' @export
#'
#' @examples
#' dotplot(c(1,2,3,3,5,5,5,5,6,7,7,9))
dotplot = function(x, breaks="Sturges", xlim=NULL, ylim=NULL, plot=TRUE, xlab=NULL, ylab="Count", ...) {
  hist_out = graphics::hist(x, breaks=breaks, plot=FALSE)
  stacks = hist_to_stacks(hist_out)
  if (is.null(xlim)) {
    xlim = range(hist_out$breaks)
  }
  if (is.null(ylim)) {
    ylim = c(0, max(hist_out$counts)+1)
  }
  if (is.null(xlab)) {
    xlab = hist_out$xname
  }
  if (plot) {
    plot(stacks, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, ...)
  }
  result = tibble::lst(!!!hist_out, stacks)
  return(result)
}
