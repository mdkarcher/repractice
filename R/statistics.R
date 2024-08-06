#' Generate independent two-way tables
#'
#' @param n integer sample size.
#' @param rprobs,cprobs vectors of row- and column-probabilities.
#' @param dnames list of dimension names. See [dimnames()].
#' @param totals logical (default `TRUE`) whether to include row and column totals.
#'
#' @return matrix containing a two way table.
#' @export
#'
#' @examples
#' indep_tab(100, c(0.5,0.5), c(0.3,0.4,0.3),
#'   dnames=list(Sex=c('M','F'), class=c('A','B','C')), totals=TRUE)
indep_tab = function(n, rprobs, cprobs, dnames=NULL, totals=TRUE) {
  pvec = as.vector(rprobs %o% cprobs)
  counts = as.integer(stats::rmultinom(n=1, size=n, prob=pvec))
  dim(counts) = c(length(rprobs), length(cprobs))

  if (!is.null(dnames)) {
    dimnames(counts) = dnames
  }

  if (totals) {
    omat = stats::addmargins(counts, FUN = list(Total = sum), quiet = TRUE)
  } else {
    omat = counts
  }

  mode(omat) = "integer"

  return(omat)
}

#' Calculate p-value for a z-test.
#'
#' @param z numeric z-test statistic.
#' @param ineq string "<" gives a "less than" test, ">" gives a "greater than" test, anything else gives a "two-sided" test.
#'
#' @return numeric p-value for the z-test.
#' @export
#'
#' @examples
#' z_ineq_pval(0.5, ">")
#' z_ineq_pval(2.0, neq())
z_ineq_pval = function(z, ineq) {
  if (ineq == "<") {
    pval = stats::pnorm(z, lower.tail = TRUE)
  } else if (ineq == ">") {
    pval = stats::pnorm(z, lower.tail = FALSE)
  } else {
    pval = 2*stats::pnorm(abs(z), lower.tail = FALSE)
  }
  return(pval)
}

#' Calculate p-value for a t test.
#'
#' @param t numeric t test statistic.
#' @param df integer degrees of freedom.
#' @param ineq string "<" gives a "less than" test, ">" gives a "greater than" test, anything else gives a "two-sided" test.
#'
#' @return numeric p-value for the t test.
#' @export
#'
#' @examples
#' t_ineq_pval(0.5, 10, ">")
#' t_ineq_pval(2.0, 30, neq())
t_ineq_pval = function(t, df, ineq) {
  if (ineq == "<") {
    pval = stats::pt(t, df=df, lower.tail = TRUE)
  } else if (ineq == ">") {
    pval = stats::pt(t, df=df, lower.tail = FALSE)
  } else {
    pval = 2*stats::pt(abs(t), df=df, lower.tail = FALSE)
  }
  return(pval)
}

#' Determine whether a p-value calculation should use lower.tail.
#'
#' @param ineq string "<" gives a "less than" test, ">" gives a "greater than" test, anything else gives a "two-sided" test.
#' @param z numeric test statistic.
#'
#' @return logical whether p-value calls to [stats::pnorm()] or [stats::pt()] should use `lower.tail`.
#' @export
#'
#' @examples
#' lower_tail("<", 1.5)
#' lower_tail(neq(), -1.5)
lower_tail = function(ineq, z) {
  ((ineq == "<") || (ineq != ">" && z < 0))
}


#' Random finite probability distribution
#'
#' Generates a finite-dimensional probability distribution according to a symmetric [Dirichlet distribution](https://en.wikipedia.org/wiki/Dirichlet_distribution#Special_cases).
#'
#' @param k integer; the size of the probability distribution.
#' @param concentration numeric; acts as the concentration parameter to the symmetric Dirichlet distribution.
#'
#' @return numeric vector of probabilities.
#' @export
#'
#' @examples
#' rdist(5, 1)
rdist = function(k, concentration=1) {
  raw = stats::rgamma(n=k, shape=concentration)
  return(raw/sum(raw))
}
