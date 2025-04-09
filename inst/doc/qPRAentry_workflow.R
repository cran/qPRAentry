## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(qPRAentry)

## -----------------------------------------------------------------------------
data("datatrade_NorthAm")

## -----------------------------------------------------------------------------
extra_total <- datatrade_NorthAm$extra_import
head(extra_total)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(dplyr)
CNTR_pest <- c("CNTR_1", "CNTR_2")
extra_pest <- datatrade_NorthAm$extra_import %>% filter(partner%in%CNTR_pest)
head(extra_pest)

## -----------------------------------------------------------------------------
intra_trade  <- datatrade_NorthAm$intra_trade
head(intra_trade)

## -----------------------------------------------------------------------------
internal_production  <- datatrade_NorthAm$internal_production
head(internal_production)

## -----------------------------------------------------------------------------
trade_NorthAm <- trade_data(extra_total = extra_total,
                            extra_pest = extra_pest,
                            intra_trade = intra_trade,
                            internal_production = internal_production,
                            filter_IDs = c("US", "CA"),
                            filter_period = c("January-March", "April-June"))

## -----------------------------------------------------------------------------
head(trade_NorthAm$total_trade)

## -----------------------------------------------------------------------------
head(trade_NorthAm$intra_trade)

## -----------------------------------------------------------------------------
library(ggplot2)
plot_countries(data = trade_NorthAm$total_trade,
               iso_col = "country_IDs", 
               values_col = "total_available",
               title = "Total commodity available",
               legend_title = "units") +
  xlim(-180,-20) + ylim(0,90)

## -----------------------------------------------------------------------------
ntrade_NorthAm <- ntrade(trade_data = trade_NorthAm)
head(ntrade_NorthAm)

## -----------------------------------------------------------------------------
ntrade_NorthAm_summary <- ntrade(trade_data = trade_NorthAm,
                                 summarise_result = c("mean", "sd", 
                                                      "quantile(0.025)", 
                                                      "median",
                                                      "quantile(0.975)"))
head(ntrade_NorthAm_summary)

## -----------------------------------------------------------------------------
plot_countries(data = ntrade_NorthAm_summary,
               iso_col = "country_IDs", 
               values_col = "median",
               title = "Ntrade median",
               legend_title = "units") +
  xlim(-180,-20) + ylim(0,90)

## -----------------------------------------------------------------------------
# read data for redistribution and filter subdivisions of US and CA
redist_data <- datatrade_NorthAm$consumption_iso2 %>% 
  filter(substr(iso_3166_2, 1, 2) %in% c("US", "CA"))

data_redist <- redist_iso(data = ntrade_NorthAm_summary,
                          iso_col = "country_IDs",
                          values_col = "median",
                          redist_data = redist_data,
                          redist_iso_col = "iso_3166_2",
                          redist_values_col = "value")

head(data_redist)

## -----------------------------------------------------------------------------
# pathway model (excluding ntrade)
model <- "(1/P1) * ((P2 * 1000) + P3)"

# parameter distributions
parameters_dist <- list(P1 = list(dist = "unif", min = 0, max = 1),
                        P2 = list(dist = "beta", shape1 = 1, shape2 = 5),
                        P3 = list(dist = "norm", mean = 0, sd = 1))


res_pathway <- pathway_model(ntrade_data = ntrade_NorthAm_summary,
                             IDs_col = "country_IDs",
                             values_col = "median",
                             expression = model,
                             parameters = parameters_dist,
                             niter = 100)
head(res_pathway)

## -----------------------------------------------------------------------------
res_pathway[res_pathway$country_IDs == "Total",]

## -----------------------------------------------------------------------------
plot_countries(data = res_pathway,
               iso_col = "country_IDs", 
               values_col = "Median",
               colors = c("#DCE319FF", "#55C667FF", "#33638DFF"),
               title = "NPFP median",
               legend_title = "NPFP") +
  xlim(-180,-20) + ylim(0,90)

## -----------------------------------------------------------------------------
data("datatrade_EU")

## -----------------------------------------------------------------------------
extra_total <- datatrade_EU$extra_import %>% filter(partner=="Extra_Total")
head(extra_total)

## -----------------------------------------------------------------------------
extra_pest <- datatrade_EU$extra_import %>% filter(partner!="Extra_Total")
head(extra_pest)

## -----------------------------------------------------------------------------
intra_trade  <- datatrade_EU$intra_trade
head(intra_trade)

## -----------------------------------------------------------------------------
internal_production  <- datatrade_EU$internal_production
head(internal_production)

## -----------------------------------------------------------------------------
trade_EU <- trade_data(extra_total = extra_total,
                       extra_pest = extra_pest,
                       intra_trade = intra_trade,
                       internal_production = internal_production)

## -----------------------------------------------------------------------------
head(trade_EU$total_trade)

## -----------------------------------------------------------------------------
head(trade_EU$intra_trade)

## -----------------------------------------------------------------------------
plot_nuts(data = trade_EU$total_trade, 
          nuts_col = "country_IDs", 
          values_col = "total_available",
          nuts_level = 0,
          title = "Total commodity available",
          legend_title = "units") +
  xlim(-30,50) + ylim(25,70)

## -----------------------------------------------------------------------------
ntrade_EU <- ntrade(trade_data = trade_EU,
                    summarise_result = c("mean", "sd"))
head(ntrade_EU)

## -----------------------------------------------------------------------------
plot_nuts(data = ntrade_EU, 
          nuts_col="country_IDs", 
          values_col="mean",
          nuts_level = 0,
          title = "Ntrade mean",
          legend_title = "units") +
  xlim(-40,50) + ylim(25,70)

## ----include=FALSE------------------------------------------------------------
error_msg <- NULL
ntrade_redist_pop <- tryCatch({
  suppressMessages(
    suppressWarnings(
      redist_nuts(data = ntrade_EU,
                  nuts_col = "country_IDs",
                  values_col = "mean",
                  to_nuts = 2,
                  redist_data = "population",
                  population_year = c(2020, 2021))
    ))
}, error = function(e) {
  error_msg <<- paste("Error:", e$message)
  NULL
})
eval_cond <- ifelse(is.null(ntrade_redist_pop), FALSE, TRUE)

## ----eval=!eval_cond, echo=FALSE----------------------------------------------
# message(error_msg)

## ----eval=FALSE---------------------------------------------------------------
# ntrade_redist_pop <- redist_nuts(data = ntrade_EU,
#                                  nuts_col = "country_IDs",
#                                  values_col = "mean",
#                                  to_nuts = 2,
#                                  redist_data = "population",
#                                  population_year = c(2020, 2021))

## ----eval=eval_cond-----------------------------------------------------------
head(ntrade_redist_pop)

## ----eval=eval_cond-----------------------------------------------------------
plot_nuts(data = ntrade_redist_pop,
          nuts_col = "NUTS2",
          values_col = "mean",
          nuts_level = 2,
          title = "Ntrade mean",
          legend_title = "units") +
   xlim(-40,50) + ylim(25,70)

## -----------------------------------------------------------------------------
# read data for redistribution
nuts1_data <- datatrade_EU$consumption_nuts1

ntrade_redist_df <- redist_nuts(data = ntrade_EU,
                           nuts_col = "country_IDs",
                           values_col = "mean",
                           to_nuts = 1,
                           redist_data = nuts1_data,
                           redist_nuts_col = "NUTS_ID",
                           redist_values_col = "value")

head(ntrade_redist_df)

## -----------------------------------------------------------------------------
plot_nuts(data = ntrade_redist_df,
          nuts_level = 1,
          nuts_col = "NUTS1",
          values_col = "mean",
          title = "Ntrade mean",
          legend_title = "units") +
   xlim(-40,50) + ylim(25,70)

## ----eval=eval_cond-----------------------------------------------------------
# pathway model (excluding ntrade)
model <- "(1/P1) * P2 * P3"

# parameter distributions
parameters_dist <- list(P1 = list(dist = "beta", shape1 = 0.5, shape2 = 1),
                        P2 = list(dist = "gamma", shape = 1.5, scale = 100),
                        P3 = list(dist = "lnorm", mean = 5, sd = 2))

res_pathway <- pathway_model(ntrade_data = ntrade_redist_pop,
                             IDs_col = "NUTS2",
                             values_col = "mean",
                             expression = model,
                             parameters = parameters_dist,
                             niter = 100)
head(res_pathway)

## ----eval=eval_cond-----------------------------------------------------------
res_pathway[res_pathway$NUTS2 == "Total",]

## ----eval=eval_cond-----------------------------------------------------------
plot_nuts(data = res_pathway,
          nuts_level = 2,
          nuts_col = "NUTS2",
          values_col = "Mean",
          colors = c("#DCE319FF", "#55C667FF", "#33638DFF"),
          title = "NPFP mean",
          legend_title = "NPFP") +
   xlim(-40,50) + ylim(25,70)

