#' Shortcut for LaTeX not equal character
#'
#' @return string `r"(\neq)"`
#' @export
#'
#' @examples
#' compile_text(r"($x <<neq()>> 0$)")
neq = function() {
  r"(\neq)"
}

#' Shortcut for LaTeX vertical spacing
#'
#' @param v string to insert into `\vspace{...}`.
#'
#' @return string for a LaTeX vertical space `\vspace{...}`
#' @export
#'
#' @examples
#' vspace("30pt")
vspace = function(v) {
  compile_text(r"(\vspace{<<v>>})")
}

#' Check for negative values
#'
#' @param x vector of values to check for negativeness. Run through [as.numeric] first.
#'
#' @return logcical vector of `TRUE`s for negative values and `FALSE`s for nonnegative values.
#' @export
#'
#' @examples
#' is_neg(-1)
#'
#' is_neg(c(10, -10))
is_neg = function(x) {
  as.numeric(x) < 0
}

#' Wrap in delimiters under certain conditions
#'
#' Conditional wrapping function. Main use case is enclosing negative values in parentheses.
#'
#' @param x object under consideration.
#' @param cond function returning a logical value.
#' @param left string left delimiter.
#' @param right string right delimiter.
#'
#' @return string containing the potentially wrapped input.
#' @export
#'
#' @examples
#' wrapif(5)
#' wrapif(-5)
wrapif = function(x, cond = is_neg, left = '(', right = ')') {
  if (cond(x)) {
    return(paste0(left, x, right))
  }
  return(x)
}

#' Expand inequality symbol to word
#'
#' @param gtlt ">" or "<"
#' @param capitalize logical whether to capitalize the first letter.
#'
#' @return string containing "greater" or "less", potentially capitalized.
#'
#' @examples
#' greater_less("<")
greater_less = function(gtlt, capitalize=FALSE) {
  if (!(gtlt %in% c(">", "<")))
    stop("Argument gtlt not one of '>' or '<'.")
  if (gtlt == ">") {
    result = "greater"
  } else {
    result = "less"
  }
  if (capitalize)
    result = stringr::str_to_title(result)
  return(result)
}

#' Concatenate a list with commands and a concluding "and"
#'
#' @param x vector string-coercible objects.
#' @param oxford logical whether to include the Oxford comma.
#'
#' @return string of comma concatenated (string-coerced) objects.
#' @export
#'
#' @examples
#' comma_and(1:3)
comma_and = function(x, oxford = TRUE) {
  last = if (oxford) {", and "} else {" and "}
  return(glue::glue_collapse(x, sep = ", ", last = last))
}



#' Expand inequality symbol to word
#'
#' @param ineq string ">" or "<"
#'
#' @return string "less" if `ineq` is "<", "greater" if `ineq` is ">", "different" if `ineq` is anything else.
#' @export
#'
#' @examples
#' lgd("<")
#' lgd(">")
#' lgd(neq())
lgd = function(ineq) {
  result = "different"
  if (ineq == "<") {
    result = "less"
  } else if (ineq == ">") {
    result = "greater"
  }
  return(result)
}

#' Compare values and summarize as (in)equality symbols
#'
#' @param a,b values to compare.
#'
#' @return character "=" if equal, ">" if `a` is greater than `b`, "<" if `a` is less than `b`.
#' @export
#'
#' @examples
#' comp(1,1)
#' comp(1,2)
#' comp(2,1)
comp = function(a, b) {
  dplyr::case_when(
    a == b ~ "=",
    a > b ~ ">",
    a < b ~ "<"
  )
}


points_block = function(pts) {
  total = sum(pts)
  qnums = as.character(1:length(pts))
  pts_tib = tibble::tibble("Question" = c(qnums, "Total"), "Points Possible" = c(pts, total), "Points Received" = "")
  knitr::kable(pts_tib, align = 'c')
}





