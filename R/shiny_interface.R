
problemUI = function(id, title="Problem") {
  ns = NS(id)

  tagList(
    withMathJax(),
    # section below allows in-line LaTeX via $ in mathjax.
    # rmdMathJax(),

    sidebarLayout(
      sidebarPanel(
        actionButton(ns("new_problem"), "New Problem"),
        checkboxInput(ns("show_solution"), "Show Solution", FALSE)
      ),

      mainPanel(
        # h3(title),
        h3("Problem"),
        uiOutput(ns("figs")),
        uiOutput(ns("tables")),
        htmlOutput(ns("prob")),
        conditionalPanel(
          "input.show_solution == true",
          ns=ns,
          hr(),
          h3("Solution"),
          uiOutput(ns("solfigs")),
          uiOutput(ns("soltables")),
          htmlOutput(ns("sol"))
        )
      )
    )
  )
}

problemServer = function(id, problemset) {
  moduleServer(
    id,
    function(input, output, session) {
      observe({
        updateCheckboxInput(session, "show_solution", value = FALSE)
      }) |> bindEvent(input$new_problem)

      problem <- reactive({
        sample_problem(problemset)
      }) |> bindEvent(input$new_problem, ignoreNULL = FALSE)

      observe({
        pr = problem()

        for (index in seq_along(pr$figures)) {
          output[[glue("figure{index}")]] = renderPlot({
            pr$figures[[index]]()
          })
        }

        for (index2 in seq_along(pr$tables)) {
          output[[glue("table{index2}")]] = renderTable(
            pr$tables[[index2]], rownames = TRUE
          )
        }

        for (index3 in seq_along(pr$sol_figures)) {
          output[[glue("sol_figure{index3}")]] = renderPlot({
            pr$sol_figures[[index3]]()
          })
        }

        for (index4 in seq_along(pr$sol_tables)) {
          output[[glue("sol_table{index4}")]] = renderTable(
            pr$sol_tables[[index4]], rownames = TRUE
          )
        }
      })

      output$figs <- renderUI({
        ns <- session$ns
        pr = problem()
        if (!(length(pr$figures) > 0)) return()

        plot_output_list = lapply(seq_along(pr$figures), function(index) {
          plotOutput(ns(glue("figure{index}")))#, height = '250px')
        })

        do.call(fluidRow, plot_output_list)
      })

      output$tables <- renderUI({
        ns <- session$ns
        pr = problem()
        # if (!(length(pr$tables) > 0)) return()

        tab_output_list = lapply(seq_along(pr$tables), function(index) {
          tableOutput(ns(glue("table{index}")))
        })

        do.call(fluidRow, tab_output_list)
      })

      output$prob <- renderUI({
        pr = problem()
        withMathJax(
          # HTML(markdown_html(all_questions(pr)))
          HTML(markdown::mark_html(text=all_questions(pr), options = "-standalone"))
        )
      })

      output$solfigs <- renderUI({
        ns <- session$ns
        pr = problem()
        if (!(length(pr$sol_figures) > 0)) return()

        plot_output_list = lapply(1:length(pr$sol_figures), function(index) {
          plotOutput(ns(glue("sol_figure{index}")))#, height = '250px')
        })

        do.call(fluidRow, plot_output_list)
      })

      output$soltables <- renderUI({
        ns <- session$ns
        pr = problem()
        # if (!(length(pr$sol_tables) > 0)) return()

        tab_output_list = lapply(seq_along(pr$sol_tables), function(index) {
          tableOutput(ns(glue("sol_table{index}")))
        })

        do.call(fluidRow, tab_output_list)
      })

      output$sol <- renderUI({
        # if (!input$show_solution) return()
        pr = problem()
        withMathJax(
          # HTML(markdown_html(all_solutions(pr)))
          HTML(markdown::mark_html(text=all_solutions(pr), options = "-standalone"))
        )
      })
    }
  )
}


make_tabbed_ui = function(problemset_list, title="Practice") {
  ui <- do.call(
    navbarPage,
    c(title=title,
      lapply(
        1:length(problemset_list),
        function(index) {
          tabPanel(
            title = problemset_list[[index]]$label,
            problemUI(
              id=glue("tab{index}"),
              title = problemset_list[[index]]$title
            )
          )
        }
      )
    )
  )
  return(ui)
}

make_tabbed_server = function(problemset_list, title="Practice") {
  server <- function(input, output, session) {
    lapply(
      X=1:length(problemset_list),
      FUN=function(index) {
        problemServer(
          id = glue("tab{index}"),
          problemset = problemset_list[[index]]
        )
      }
    )
  }
  return(server)
}

#' Tabbed Shiny interface for multiple problem sets
#'
#' @param problemset_list list of problemsets.
#'
#' @return A [shinyApp()] object.
#' @export
#'
#' @examples
#' prop_problems = build_problemset(list(
#'   simple_prop,
#'   pet_prop,
#'   simple_successes,
#'   dotplot_visual_prop
#' ), title="Proportions")
#'
#' percent_problems = build_problemset(list(
#'   simple_percent,
#'   simple_percent_successes
#' ), title="Percents")
#'
#' \dontrun{
#' repractice_shiny_tabbed(list(prop_problems, percent_problems))
#' }
repractice_shiny_tabbed <- function(problemset_list) {
  ui <- make_tabbed_ui(problemset_list)
  server <- make_tabbed_server(problemset_list)
  return(shinyApp(ui = ui, server = server))
}
