#' pathway_parameters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pathway_parameters_ui <- function(id){
  ns <- NS(id)
  tagList(
    mainPanel(
      fluidRow(
        column(11,
               br(),
               uiOutput(ns("help_data"))
        )),
      br(),
      sidebarPanel(width=12,
                   numericInput(ns("n_iter"), "Number of iterations",
                                value=1000,
                                min=1, step = 100, width = "25%")
      ),
      br(),
      uiOutput(ns("pars")),
      actionButton(ns("dist_done"), "Done", class="enable",
                   style='width:100px; font-size:17px')
    )
  )
}

#' pathway_parameters Server Functions
#'
#' @noRd
mod_pathway_parameters_server <- function(id, ntrade_data, nuts, values, 
                                          model_done, parameters){
  n_iter <- NULL
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$pars <- renderUI({
      pn <- parameters()
      n <- length(pn)
      sI <- lapply(1:n, function(i) {
        tagList(
          h4(withMathJax(pn[i]), style = "border-bottom:2px solid grey"),
          fluidRow(
            column(6,
                   selectInput(ns(paste0("dist",i)),
                               "Distribution:",
                               choices = distribution_names,
                               selected = "beta"),
                   distribution_panel(ns, i)
            ),
            column(6,
                   plotOutput(ns(paste0("plot_pars",i)), width = "75%", height = "250px"),
                   DT::dataTableOutput(ns(paste0("table_pars",i)))
            )
          )
        )
      })
    })

    dist_result <- eventReactive(input$dist_done,{
      pn <- parameters()
      parameter_samples <- list()
      for(i in 1:length(pn)){
        d <- input[[paste0("dist",i)]]
        pars <- as.character(input[[paste0("par_",d,"_",i)]])
        parameter_samples[[i]] <- n_from_dist(d, pars, input$n_iter)
      }
      names(parameter_samples) <- pn
      parameter_samples
    })
    
    dist_q <- eventReactive(input$dist_done,{
      pn <- parameters()
      parameter_q <- list()
      for(i in 1:length(pn)){
        d <- input[[paste0("dist",i)]]
        pars <- as.character(input[[paste0("par_",d,"_",i)]])
        parameter_q[[i]] <- q_from_dist(d, pars)
      }
      names(parameter_q) <- pn
      parameter_q 
    })
    
    observeEvent(input$dist_done,{
      parameter_samples <- dist_result()
      n <- length(parameter_samples)
      parameter_q <- dist_q()
      lapply(1:n, function(i){
        plotname <- paste0("plot_pars",i)
        tablename <- paste0("table_pars",i)
        d <- input[[paste0("dist",i)]]
        pars <- as.character(input[[paste0("par_",d,"_",i)]])
        output[[plotname]] <- renderPlot({
          hist(parameter_samples[[i]], main = paste0(d,"(",pars,")"), xlab = "",
               probability = TRUE, breaks = "fd", col = "lightblue")
          lines(density(parameter_samples[[i]]), col = "blue", lwd = 2)
        })
        output[[tablename]] <- DT::renderDataTable({
          DT::datatable(parameter_q[[i]], rownames = NULL,
                        options = list(dom = 't', ordering=F))
        })
      })
    })
    
    go_results_state <- reactiveValues(is_enabled = FALSE)
    
    observeEvent(input$dist_done,{
      go_results_state$is_enabled <- TRUE
    })

    observeEvent(c(ntrade_data(), nuts(), values(), model_done()),{
      go_results_state$is_enabled <- FALSE
    })
    
    output$help_data <- renderUI({
      if(go_results_state$is_enabled){
        text_parametersDone
      }else{
        text_parameters
      }
    })
    
    par_settings <- eventReactive(input$dist_done,{
      distrib <- c()
      for(i in seq_along(dist_result())){
        d <- input[[paste0("dist",i)]]
        pars <- as.character(input[[paste0("par_",d,"_",i)]])
        distrib[i] <-  paste0(d,"(",pars,")")
      }
      return(distrib)
    })
    
    return(
      list(
        dist_done = reactive(input$dist_done),
        n_iter = reactive(input$n_iter),
        par_settings = par_settings,
        dist_result = dist_result
      )
    )
  })
}

## To be copied in the UI
# mod_pathway_parameters_ui("pathway_parameters_1")

## To be copied in the server
# mod_pathway_parameters_server("pathway_parameters_1")
