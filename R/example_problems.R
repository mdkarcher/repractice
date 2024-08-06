
simple_prop_setup = function(avg_tot=30) {
  tibble::lst(
    total = stats::rpois(n = 1, lambda = avg_tot),
    successes = sample.int(n=total, size=1),
    failures = total - successes,
    proportion = round(successes / total, 3)
  )
}


#' Simple proportion/percent problem templates.
#'
#' Collection of simple example problems involving proportions and/or percents.
#'
#' @param seed (optional) integer; pseudorandom seed used with [withr::local_seed()] inside the problem.
#'
#' @return compiled problem
#' @export
#'
#' @examples
#' simple_prop(seed=1)
#' pet_prop(seed=2)
#' simple_percent(seed=3)
#' simple_prop_percent(seed=4)
#' simple_successes(seed=5)
#' simple_percent_successes(seed=6)
#' dotplot_visual_prop(seed=7)
simple_prop = function(seed = NULL)
{
  if (!is.null(seed)) {
    withr::local_seed(seed)
  }
  setup = simple_prop_setup()

  question_template = r"(In a survey of <<total>> people, <<successes>> people said "yes" to a particular question. What proportion of the people said "yes"?)"
  answer = with(setup, proportion)
  solution_template = r"(The proportion is $\frac{<<successes>>}{<<total>>} = <<round(proportion, 3)>>$.)"

  return(tibble::lst(
    setup,
    question = compile_text(question_template, setup),
    answer,
    solution = compile_text(solution_template, setup)
  ))
}

#' @rdname simple_prop
#' @export
pet_prop = function(seed = NULL)
{
  if (!is.null(seed)) {
    withr::local_seed(seed)
  }
  setup = simple_prop_setup()

  question_template = r"(In a survey of <<total>> people, <<successes>> people said they had a pet. What proportion of the people had a pet?)"
  answer = with(setup, proportion)
  solution_template = r"(The proportion of people with a pet is $\frac{<<successes>>}{<<total>>} = <<round(proportion, 3)>>$.)"

  return(tibble::lst(
    setup,
    question = compile_text(question_template, setup),
    answer,
    solution = compile_text(solution_template, setup)
  ))
}

#' @rdname simple_prop
#' @export
simple_percent = function(seed = NULL)
{
  if (!is.null(seed)) {
    withr::local_seed(seed)
  }
  setup = simple_prop_setup()

  question_template = r"(In a survey of <<total>> people, <<successes>> people said "yes" to a particular question. What percent of the people said "yes"?)"
  answer = with(setup, proportion*100)
  solution_template = r"(The percentage is $\frac{<<successes>>}{<<total>>}\cdot 100\% = <<round(proportion*100, 1)>>\%$.)"

  return(tibble::lst(
    setup,
    question = compile_text(question_template, setup),
    answer,
    solution = compile_text(solution_template, setup)
  ))
}

#' @rdname simple_prop
#' @export
simple_prop_percent = function(seed = NULL)
{
  if (!is.null(seed)) {
    withr::local_seed(seed)
  }
  setup = simple_prop_setup()

  question_template = r"(In a survey of <<total>> people, <<successes>> people said "yes" to a particular question.)"
  subquestion_templates = list(
    r"(What proportion of the people said "yes"?)",
    r"(What percent of the people said "yes"?)"
  )

  subsolution_templates = list(
    r"(The proportion is $\frac{<<successes>>}{<<total>>} = <<round(proportion, 3)>>$.)",
    r"(The percentage is $\frac{<<successes>>}{<<total>>}\cdot 100\% = <<round(proportion*100, 1)>>\%$.)"
  )

  return(compile_problem(
    setup = setup,
    question_template = question_template,
    subquestion_templates = subquestion_templates,
    subsolution_templates = subsolution_templates
  ))
}

#' @rdname simple_prop
#' @export
simple_successes = function(seed = NULL)
{
  if (!is.null(seed)) {
    withr::local_seed(seed)
  }
  setup = simple_prop_setup()

  question_template = r"(In a survey of <<total>> people, the proportion of the people who said "yes" to a particular question was <<proportion>>. How many of the people said "yes"?)"
  answer = with(setup, successes)
  solution_template = r"(The number of people who said "yes" is $<<total>> \cdot <<proportion>> = <<successes>>$.)"

  return(tibble::lst(
    setup,
    question = compile_text(question_template, setup),
    answer,
    solution = compile_text(solution_template, setup)
  ))
}

#' @rdname simple_prop
#' @export
simple_percent_successes = function(seed = NULL)
{
  if (!is.null(seed)) {
    withr::local_seed(seed)
  }
  setup = simple_prop_setup()

  question_template = r"(In a survey of <<total>> people, <<round(proportion*100, 1)>>% of the people said "yes" to a particular question. How many of the people said "yes"?)"
  answer = with(setup, successes)
  solution_template = r"(The number of people who said "yes" is $<<total>> \cdot <<proportion>> = <<successes>>$.)"

  return(tibble::lst(
    setup,
    question = compile_text(question_template, setup),
    answer,
    solution = compile_text(solution_template, setup)
  ))
}


dotplot_visual_prop_setup = function() {
  tibble::lst(
    n_samps = stats::rpois(1, 13)+2,
    mu = 20,
    s = 2,
    samp = stats::rnorm(n = n_samps, mean = mu, sd = s),
    breaks = seq(round(min(samp)/0.5)*0.5-0.5, round(max(samp)/0.5)*0.5+0.5, by = 0.5),
    cutoff = sample(breaks, 1),
    gtlt = sample(c(">", "<"), 1),
    successes = ifelse(gtlt == ">", sum(samp > cutoff), sum(samp < cutoff)),
    proportion = round(successes / n_samps, 3)
  )
}

dotplot_visual_prop_fig = function(setup) {
  hist_out = graphics::hist(setup$samp, breaks = setup$breaks, plot=FALSE)
  graphics::par(mar = c(3, 4, 1, 1)+0.1)
  hist_to_dotplot(hist_out, pch=19, xlab="")
  graphics::abline(v=setup$cutoff, col="black", lty=2)
}

dotplot_visual_prop_solfig = function(setup) {
  hist_out = graphics::hist(setup$samp, breaks = setup$breaks, plot=FALSE)
  graphics::par(mar = c(3, 4, 1, 1)+0.1)
  stacks = hist_to_stacks(hist_out)
  mask = stacks$x > setup$cutoff
  if (setup$gtlt == "<")
    mask = !mask
  col_mask = ifelse(mask, "red", "black")
  hist_to_dotplot(hist_out, pch=19, xlab="", col=col_mask)
  graphics::abline(v=setup$cutoff, col="black", lty=2)
}

#' @rdname simple_prop
#' @export
dotplot_visual_prop = function(seed = NULL)
{
  if (!is.null(seed)) {
    withr::local_seed(seed)
  }
  setup = dotplot_visual_prop_setup()

  question_template = r"(What proportion of the <<n_samps>> points shown are <<lgd(gtlt)>> than <<cutoff>> (dashed line)?)"
  answer = with(setup, proportion)
  solution_template = r"(We see that <<successes>> of the points (colored red here) are <<lgd(gtlt)>> than <<cutoff>>, so the proportion is $\frac{<<successes>>}{<<n_samps>>} = <<proportion>>$.)"
  figure_templates = list(
    dotplot_visual_prop_fig
  )
  sol_figure_templates = list(
    dotplot_visual_prop_solfig
  )

  return(tibble::lst(
    setup,
    question = compile_text(question_template, setup),
    answer,
    solution = compile_text(solution_template, setup),
    figures = sapply(figure_templates, function(x) {compile_figure(x, setup)}),
    sol_figures = sapply(sol_figure_templates, function(x) {compile_figure(x, setup)})
  ))
}


# proportion_template_list = list(
#   simple_prop,
#   dotplot_visual_prop
# )
#
# proportion_problemset = build_problemset(
#   template_list=proportion_template_list,
#   title="Calculating a Proportion",
#   label="Proportions"
# )












