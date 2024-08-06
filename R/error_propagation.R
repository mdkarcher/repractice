#' Ceiling and floor with different precision
#'
#' @description
#' `ceilingk` rounds a numeric vector `x` up to `k` decimal places.
#'
#' @description
#' `floork` rounds a numeric vector `x` down to `k` decimal places.
#'
#' @param x a numeric vector of values to be rounded up or down.
#' @param k integer indicating the number of decimal places to be rounded to. Default of 0
#'
#' @return a numeric vector of rounded values.
#' @export
#'
#' @examples
#' ceilingk(3.14159, 2)
#' floork(3.14159, 2)
ceilingk = function(x, k=0) {
  if (is.infinite(k)) {
    return(x)
  } else {
    return(ceiling(x*10^k)/10^k)
  }
}

#' @rdname ceilingk
#' @export
floork = function(x, k=0) {
  if (is.infinite(k)) {
    return(x)
  } else {
    return(floor(x*10^k)/10^k)
  }
}


eprop = function(params, code, digits=Inf, interpolate=1) {
  interpolations = matrix(NA, nrow=interpolate, ncol=ncol(params))
  colnames(interpolations) = names(params)
  for (i in 1:ncol(params)) {
    interpolations[,i] = utils::tail(utils::head(seq(min(params[,i]), max(params[,i]), length.out=interpolate+2),-1),-1)
  }
  interp_params = dplyr::bind_rows(params, tibble::as_tibble(interpolations))

  expanded_params = tidyr::expand_grid(!!!interp_params)

  all_results = rep(NA, nrow(expanded_params))
  for (i in 1:nrow(expanded_params)) {
    all_results[i] = do.call(code, expanded_params[i,])
  }

  results = c(all_results[1], floork(min(all_results), digits), ceilingk(max(all_results), digits))

  return(results)
}
