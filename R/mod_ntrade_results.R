#' ntrade_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ntrade_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(1),
      column(10,
             HTML('<p class="custom-text"><br>View <i>N<sub>trade</sub></i> results 
                  in table or map format.<br><br>
                  <i class="fa-solid fa-star" style="color: #63E6BE;"></i> 
                  The <i>N<sub>trade</sub></i> redistribution from NUTS0 to NUTS2 
                  level is available in the <strong style="color: #1E68BA;">Redistribution</strong>
                  tab.<br>
                  You can also return to the <strong style="color: #1E68BA;">Data</strong> 
                  tab to review or change trade data.<br></p>'),
             shinyWidgets::radioGroupButtons(
               inputId = ns("NUTS0_btn"),
               label = NULL,
               choices = c("Table", "Map"),
               justified = TRUE,
               selected = "Table",
               width = "90%"
             ),
             uiOutput(ns("NUTS0_results"))
      ),
      column(1)
    )
  )
}

#' ntrade_results Server Functions
#'
#' @noRd
mod_ntrade_results_server <- function(id, nuts_yr, NUTS_CODES, trade_done, 
                                      time_period, units, TradeData){
  country_IDs <- q0.05 <- q0.95 <- CNTR_CODE <- CNTR_NAME <- NUTS_ID <- Median <- NULL
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    Nt <- eventReactive(trade_done(),{
      NUTS_CODES <- NUTS_CODES()
      trade <- TradeData()
      if(length(time_period())>1){
        res <- ntrade(trade_data = trade,
                      summarise_result = c("quantile(0.05)", 
                                           "median", 
                                           "quantile(0.95)",
                                           "mean", 
                                           "sd"))
        res <- res %>%
          relocate(country_IDs, q0.05, median, q0.95, mean, sd) %>%
          rename(NUTS0 = country_IDs,
                 Q0.05 = q0.05,
                 Median = median,
                 Q0.95 = q0.95,
                 Mean = mean,
                 SD = sd)
      }else{
        res <- ntrade(trade_data = trade)
        res <- res %>%
          relocate(country_IDs) %>%
          rename(NUTS0 = country_IDs)
      }
      res <- res %>% 
        left_join(select(NUTS_CODES, CNTR_CODE, CNTR_NAME)%>% distinct(),
                  by=join_by(NUTS0 == CNTR_CODE)) %>%
        relocate(CNTR_NAME, .after=NUTS0)
      return(res)
    })
    
    # EU NUTS0 map (from giscoR pkg)
    EU00 <- eventReactive(trade_done(),{
      NUTS0_map <- cached_get_EUmap(nuts_yr(), nuts=0) %>% 
        st_crop(xmin=-40,ymin=20,xmax=50,ymax=70)
      NUTS0_map
    })
    
    # Nt results
    observe({
      if(input$NUTS0_btn=="Table"){
        output$NUTS0_results <- renderUI({
          fluidRow(
            div(class="table-container",
                DT::dataTableOutput(ns("trade_table")) %>% 
                  shinycssloaders::withSpinner(type=5, color = "#327FB0", size=0.8)
            )
          )
        })
      }else if(input$NUTS0_btn=="Map"){
        output$NUTS0_results <- renderUI({
          fluidRow(
            div(class = "plot-container",
                div(class = "plot",
                    p("Place your cursor over the map to display the values", 
                      class="custom-text"),
                    br(),
                    ggiraph::girafeOutput(ns("NUTS0_map")) %>% 
                      shinycssloaders::withSpinner(type=5, color = "#327FB0", size=0.8)
                )
            )
          )
        })
      }
    })
    
    output$trade_table <- DT::renderDataTable({
      numeric_columns <- names(Nt())[which(sapply(Nt(), is.numeric))]
      DT::datatable(Nt(), options = list(dom = 'ft', pageLength = -1)) %>%
        DT::formatRound(columns = numeric_columns, digits=4) %>%
        DT::formatStyle(columns = "NUTS0", target = "cell", 
                        backgroundColor = "#F7080880") %>%
        DT::formatStyle(columns = numeric_columns, target = "cell", 
                        backgroundColor = "#F7080820")
    })
    
    output$NUTS0_map <- ggiraph::renderGirafe({
      Nt <- Nt()
      EU00 <- EU00() %>%
        left_join(select(Nt, !CNTR_NAME), by=join_by(NUTS_ID==NUTS0))
      if(length(time_period())==1){
        EU00 <- EU00 %>% 
          rename(Ntrade = !!paste0("Ntrade_", time_period()))
        tooltip <- paste0(EU00$NUTS_ID, " - ", EU00$CNTR_NAME,
                          "\nNtrade: ", round(EU00$Ntrade,2))
        title <- bquote(paste(N[trade], " ", .(time_period())))
      }else{
        EU00 <- EU00 %>% 
          rename(Ntrade = Median)
        tooltip <- paste0(EU00$NUTS_ID, " - ", EU00$CNTR_NAME,
                          "\nQ0.05: ", round(EU00$Q0.05,2),
                          "\nMedian: ", round(EU00$Ntrade,2),
                          "\nQ0.95: ", round(EU00$Q0.95,2))
        title <- expression(paste(N[trade], " ", "- Median"))
      }
      limits <- c(min(EU00$Ntrade, na.rm=T), max(EU00$Ntrade, na.rm=T))
      ggiraph_plot(data = EU00, value = "Ntrade", 
                   name = units(), 
                   title = title,
                   limits = limits,
                   tooltip = tooltip)
    })
    
    return(Nt)
  })
}

## To be copied in the UI
# mod_ntrade_results_ui("ntrade_results_1")

## To be copied in the server
# mod_ntrade_results_server("ntrade_results_1")
