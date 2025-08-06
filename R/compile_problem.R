
#' LaTeX-friendly string interpolation
#'
#' Wrapper for the [glue::glue_data()] function with more LaTeX- and
#' raw-string-friendly defaults.
#'
#' @param text string Input template string.
#' @param vars listish List or environment containing variables to make
#'   available for string interpolation. Defaults to `parent.frame()`.
#' @param open string Opening delimiter. Defaults to "<<".
#' @param close string Closing delimiter. Defaults to ">>".
#' @param trim logical Whether to trim `text` with [glue::trim()] or not. Unlike
#'   [glue::glue()], defaults to `FALSE`.
#'
#' @return A glue object.
#' @export
#'
#' @examples
#' details <- list(name = "Michael", subject="statistics")
#' compile_text("My name is <<name>>, and I teach <<subject>>.", vars = details)
#'
#' setup <- list(ex = 3)
#' compile_text(
#'   r"(The derivative of $\pm C x^<<ex>>$ is $\pm <<ex>> C x^<<ex-1>>$)",
#'   vars = setup)
compile_text = function(text, vars=parent.frame(), open="<<", close=">>", trim=FALSE) {
  return(glue::glue_data(text, .x = vars, .open = open, .close = close, .trim=trim))
}


#' Pre-specify plotting function arguments
#'
#' @param func function Plotting function with single argument taking a list of
#'   plotting variables.
#' @param vars listish List or environment containing variables to make
#'   available for string interpolation. Defaults to `parent.frame()`.
#'
#' @returns A function with no arguments that uses the original function on the
#'   variables in vars.
#' @export
#'
#' @examples
#' vars <- list(x = 1:5, y = 5:1)
#' func <- function(args) plot(args$x, args$y, pch=19)
#' func_noargs <- compile_figure(func, vars)
#' func_noargs()
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
  tibble::lst(
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

# compile_problem_old = function(template) {
#   if (is.null(template$args)) {
#     vars = template$setup_fn()
#   } else {
#     vars = do.call(template$setup_fn, template$args)
#   }
#
#   result = list(
#     question = template$question %>% compile_text(vars),
#     answer = template$answer %>% compile_text(vars),
#     solution = template$solution %>% compile_text(vars)
#   )
#
#   if (!is.null(template$figures)) {
#     result$figures = sapply(template$figures, function(x) {compile_figure(x, vars)})
#   }
#   if (!is.null(template$sol_figures)) {
#     result$sol_figures = sapply(template$sol_figures, function(x) {compile_figure(x, vars)})
#   }
#
#   if (!is.null(template$subquestions)) {
#     result$subquestions = sapply(template$subquestions, function(x) {compile_text(x, vars)})
#   }
#   if (!is.null(template$subanswers)) {
#     result$subanswers = sapply(template$subanswers, function(x) {compile_text(x, vars)})
#   }
#   if (!is.null(template$subsolutions)) {
#     result$subsolutions = sapply(template$subsolutions, function(x) {compile_text(x, vars)})
#   }
#   return(result)
# }

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

#' Container for multiple problem templates
#'
#' @param template_list list of problem templates.
#' @param prob optional vector of weights for use when sampling problems from the problem set. Defaults to `NULL`, representing a uniform distribution.
#' @param title optional string used as a short title description of the whole set.
#' @param label optional shorts label for the problem set. Defaults to the same value as `title`.
#'
#' @return a problemset object.
#' @export
#'
#' @examples
#' percent_problems = build_problemset(list(
#'   simple_percent,
#'   simple_percent_successes
#' ), title="Percents")
build_problemset = function(template_list, prob=NULL, title="", label=title) {
  if (!is.null(prob) && length(prob) != length(template_list))
    stop("Incorrect number of probabilities")
  # if (is.null(label))
  #   label = title
  list(template_list = template_list, prob = prob, title = title, label = label)
}

#' Format a problem/solution into markdown
#'
#' @param problem a fully compiled problem.
#' @param listch string to prepend subquestions. Defaults to `"1."` (numbered list).
#' @param prepend_question,prepend_solution logical (default `TRUE`) whether to include the `question`/`solution` field before listing the `subquestions`/`subsolutions`.
#' @param spacer string (default `"\n"`) to append to subquestions.
#'
#' @return string containing the compiled problem/solution formatted into markdown.
#' @export
#'
#' @examples
#' all_questions(simple_prop(seed=1))
#' all_solutions(simple_prop(seed=1))
#'
#' all_questions(simple_prop_percent(seed=1))
#' all_solutions(simple_prop_percent(seed=1))
all_questions = function(problem, listch = '1.', prepend_question=TRUE, spacer="\n") {
  formatted_subquestions = lapply(
    problem$subquestions,
    function(sq) {
      sq = stringr::str_replace_all(sq, "\n", "\n    ")
      glue("{listch} {sq}{spacer}")
    }
  )

  if (prepend_question) {
    formatted_subquestions = c(problem$question, formatted_subquestions)
  }

  result = paste0(formatted_subquestions, collapse = "\n\n")
  return(result)
}

#' @rdname all_questions
#' @export
all_solutions = function(problem, listch = '1', prepend_solution=TRUE) {
  formatted_subsolutions = lapply(
    problem$subsolutions,
    function(sq) {
      sq = stringr::str_replace_all(sq, "\n", "\n    ")
      glue("{listch}. {sq}")
    }
  )

  if (prepend_solution) {
    formatted_subsolutions = c(problem$solution, formatted_subsolutions)
  }

  result = paste0(formatted_subsolutions, collapse = "\n\n")
  return(result)
}

#' Format a problem/solution into markdown with figures and tables
#'
#' @param prob a fully compiled problem.
#' @param pts optional number of points.
#' @param spacer string to be passed through to `all_questions()`/`all_solutions()`.
#'
#' @export
#'
#' @examples
#' format_problem(simple_prop(seed=1))
#' format_solution(simple_prop(seed=1))
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

#' @rdname format_problem
#' @export
format_solution = function(prob) {
  for (fig_fn in prob$sol_figures) {
    fig_fn()
  }
  for (table in prob$sol_tables) {
    print(knitr::kable(table))
  }
  cat(all_solutions(prob))
}

#' Format a list of problems and (optionally) solutions
#'
#' @param probs list of compiled problems.
#' @param pts_list optional vector of points.
#' @param show_sol logical (default `FALSE`) whether to show solutions.
#'
#' @export
#'
#' @examples
#' format_problems(list(simple_prop(seed=1), simple_percent(seed=2)))
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

