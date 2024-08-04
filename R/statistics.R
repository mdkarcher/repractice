indep_tab = function(n, rprobs, cprobs, dnames=NULL, totals=TRUE) {
  pvec = as.vector(rprobs %o% cprobs)
  counts = as.integer(stats::rmultinom(n=1, size=n, prob=pvec))
  dim(counts) = c(length(rprobs), length(cprobs))

  if (!is.null(dnames)) {
    dimnames(counts) = dnames
  }

  if (totals) {
    omat = addmargins(counts, FUN = list(Total = sum), quiet = TRUE)
  } else {
    omat = counts
  }

  mode(omat) = "integer"

  return(omat)
}

z_ineq_pval = function(z, ineq) {
  if (ineq == "<") {
    pval = pnorm(z, lower.tail = TRUE)
  } else if (ineq == ">") {
    pval = pnorm(z, lower.tail = FALSE)
  } else {
    pval = 2*pnorm(abs(z), lower.tail = FALSE)
  }
  return(pval)
}

t_ineq_pval = function(t, df, ineq) {
  if (ineq == "<") {
    pval = pt(t, df=df, lower.tail = TRUE)
  } else if (ineq == ">") {
    pval = pt(t, df=df, lower.tail = FALSE)
  } else {
    pval = 2*pt(abs(t), df=df, lower.tail = FALSE)
  }
  return(pval)
}

lower_tail = function(ineq, z) {
  ((ineq == "<") || (ineq == NEQ && z < 0))
}


rdist = function(k, concentration=1) {
  raw = rgamma(n=k, shape=concentration)
  return(raw/sum(raw))
}
