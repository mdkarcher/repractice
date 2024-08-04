
#' LaTeX-friendly string interpolation
#'
#' Wrapper for the [glue::glue()] function with more LaTeX-friendly and
#' raw-string-friendly defaults.
#'
#' @param text string Input template string.
#' @param vars listish List or environment containing variables to make available for string interpolation. Defaults to `parent.frame()`.
#' @param open string Opening delimiter. Defaults to "<<".
#' @param close string Closing delimiter. Defaults to ">>".
#' @param trim logical Whether to trim `text` with [glue::trim()] or not. Unlike [glue::glue()], defaults to `FALSE`.
#'
#' @return A glue object.
#' @export
#'
#' @examples
#' details <- list(name = "Michael", subject="statistics")
#' compile_text("My name is <<name>>, and I teach <<subject>>.", vars = details)
#'
#' setup <- list(exponent = 3)
#' compile_text(r"(The derivative of $\pm C x^<<exponent>>$ is $\pm <<exponent>> C x^<<exponent-1>>$)", vars = setup)
compile_text = function(text, vars=parent.frame(), open="<<", close=">>", trim=FALSE) {
  return(glue(text, .envir = vars, .open = open, .close = close, .trim=trim))
}

compile_figure = function(func, vars) {
  return(function() {func(vars)})
}

compile_problem = function(
    setup,
    question_template = NULL,
    subquestion_templates = NULL,
    figures = NULL,
    tables = NULL,
    answer = NULL,
    subanswers = NULL,
    solution_template = NULL,
    subsolution_templates = NULL,
    sol_figures = NULL,
    sol_tables = NULL
) {
  lst(
    setup,
    question = compile_text(question_template, setup),
    subquestions = sapply(subquestion_templates, function(x) {compile_text(x, setup)}),
    figures,
    tables,
    solution = compile_text(solution_template, setup),
    subsolutions = sapply(subsolution_templates, function(x) {compile_text(x, setup)}),
    sol_figures,
    sol_tables,
  )
}

compile_problem_old = function(template) {
  if (is.null(template$args)) {
    vars = template$setup_fn()
  } else {
    vars = do.call(template$setup_fn, template$args)
  }

  result = list(
    question = template$question %>% compile_text(vars),
    answer = template$answer %>% compile_text(vars),
    solution = template$solution %>% compile_text(vars)
  )

  if (!is.null(template$figures)) {
    result$figures = sapply(template$figures, function(x) {compile_figure(x, vars)})
  }
  if (!is.null(template$sol_figures)) {
    result$sol_figures = sapply(template$sol_figures, function(x) {compile_figure(x, vars)})
  }

  if (!is.null(template$subquestions)) {
    result$subquestions = sapply(template$subquestions, function(x) {compile_text(x, vars)})
  }
  if (!is.null(template$subanswers)) {
    result$subanswers = sapply(template$subanswers, function(x) {compile_text(x, vars)})
  }
  if (!is.null(template$subsolutions)) {
    result$subsolutions = sapply(template$subsolutions, function(x) {compile_text(x, vars)})
  }
  return(result)
}

sample_problem = function(problemset) {
  template = sample(problemset$template_list, 1, prob=problemset$prob)[[1]]
  result = template()
  return(result)
}

# sample_problems = function(problemset, size, replace=FALSE) {
#   templates = sample(problemset$template_list, size = size, replace = replace, prob=problemset$prob)
#   result = lapply(templates, compile_problem)
#   return(result)
# }

build_problemset = function(template_list, prob=NULL, title="", label=title) {
  if (!is.null(prob) && length(prob) != length(template_list))
    stop("Incorrect number of probabilities")
  # if (is.null(label))
  #   label = title
  list(template_list = template_list, prob = prob, title = title, label = label)
}

all_questions = function(problem, listch = '1', prepend_question=TRUE, spacer="") {
  formatted_subquestions = lapply(
    problem$subquestions,
    function(sq) {
      sq = str_replace_all(sq, "\n", "\n    ")
      glue("{listch}. {sq}\n{spacer}")
    }
  )

  if (prepend_question) {
    formatted_subquestions = c(problem$question, formatted_subquestions)
  }

  result = paste0(formatted_subquestions, collapse = "\n\n")
  return(result)
}

all_solutions = function(problem, listch = '1', prepend_solution=TRUE) {
  formatted_subsolutions = lapply(
    problem$subsolutions,
    function(sq) {
      sq = str_replace_all(sq, "\n", "\n    ")
      glue("{listch}. {sq}")
    }
  )

  if (prepend_solution) {
    formatted_subsolutions = c(problem$solution, formatted_subsolutions)
  }

  result = paste0(formatted_subsolutions, collapse = "\n\n")
  return(result)
}

format_problem = function(prob, pts=NULL, spacer="") {
  for (fig_fn in prob$figures) {
    fig_fn()
  }
  for (table in prob$tables) {
    print(knitr::kable(table))
  }
  if (is.null(pts)) {
    cat(all_questions(prob, spacer = spacer))
  } else {
    cat(glue("({pts} points) "), all_questions(prob, spacer = spacer))
  }
}

format_solution = function(prob) {
  for (fig_fn in prob$sol_figures) {
    fig_fn()
  }
  for (table in prob$sol_tables) {
    print(knitr::kable(table))
  }
  cat(all_solutions(prob))
}

format_problems = function(probs, pts_list=NULL, show_sol=FALSE) {
  for (i in seq_along(probs)) {
    prob = probs[[i]]
    cat(glue("## Problem {i}\n\n"))
    if (is.null(pts_list)) {
      pts = NULL
    } else if (length(pts_list) == 1) {
      pts = pts_list[1]
    } else {
      pts = pts_list[i]
    }
    format_problem(prob, pts)
    if (show_sol) {
      cat("\n\n***\n\n### Solution\n\n")
      format_solution(prob)
    }
    cat("\n\n",r"(\newpage)","\n\n")
  }
}

