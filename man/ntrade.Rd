% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ntrade.R
\name{ntrade}
\alias{ntrade}
\title{Ntrade calculation}
\usage{
ntrade(
  trade_data,
  filter_IDs = NULL,
  filter_period = NULL,
  summarise_result = NULL
)
}
\arguments{
\item{trade_data}{An object of class \code{TradeData} that can be the output of
\code{\link[=trade_data]{trade_data()}}.}

\item{filter_IDs}{A vector containing the country IDs to filter (identification codes
of the countries of interest). By default, it is set to \code{NULL}, meaning all
\code{reporter} countries in the data frames will be considered.}

\item{filter_period}{A vector specifying the time periods to filter, based on
the \code{time_period} column. By default, it is set to \code{NULL}, meaning
all time periods in the data frames will be considered.}

\item{summarise_result}{A character vector specifying functions to summarise the
\eqn{N_{trade}} result for the selected time periods (\code{filter_period}).
It accepts the expressions \code{"mean"} for the mean, \code{"sd"} for the standard
deviation, \code{"median"} for the median value and \code{"quantile(p)"} where
\code{p} is the probability for the quantiles to the given probabilities. See examples.}
}
\value{
A data frame with the quantity of commodity imported by each country of interest
(\code{country_IDs}) from countries or regions where the pest is present. The result
is returned for each time period if \code{summarise_result} is not specified
(default is \code{NULL}). If a summary function is specified, the result will be
summarised accordingly.
}
\description{
Calculates the quantity of potentially infested imported commodity
(\eqn{N_{trade}}) from third countries where the pest is present, based on the
provided trade data (\code{TradeData} object output of the \code{\link[=trade_data]{trade_data()}}
function).
}
\details{
The calculation of \eqn{N_{trade_i}} for each country of interest \eqn{i}
is based on the equation:
\deqn{N_{trade_i} = ExtraPest_i - ExtraPest_i \sum_{j \neq i} R_{ij} + 
\sum_{j \neq i} ExtraPest_j R_{ji},}
where:
\itemize{
\item \eqn{N_{trade_i}}: quantity of commodity from third countries remaining in
country \eqn{i}, taking into account the direct importation from third countries
where the pest is present, the re-exportation to other countries of interest,
and the indirect importation of the commodity from other countries of interest.
\item \eqn{ExtraPest_i} and \eqn{ExtraPest_j}: quantity of commodity imported by
country \eqn{i} and country \eqn{j} from third countries where the pest is present
(direct import), during the period of time considered.
\item \eqn{R_{ij}} and \eqn{R_{ji}}: proportion of intra-regional trade relative
to the total available quantity in the exporting country defined as:
\deqn{R_{ij} = IntraExp_{ij}/(IP_i + ExtraTotal_i), \\
R_{ji} = IntraExp_{ji}/(IP_j + ExtraTotal_j).}
Specifically, \eqn{R_{ij}} indicates the proportion of the commodity that is exported
from country \eqn{i} to country \eqn{j} (\eqn{IntraExp_{ij}}), while \eqn{R_{ji}}
indicates the proportion exported from country \eqn{j} to country \eqn{i} (\eqn{IntraExp_{ji}}),
in both cases out of the total available commodity in the exporter country. The total
available quantity is considered as tha sum of the internal production of the country
(\eqn{IP}) and the total quantity imported from third countries (\eqn{ExtraTotal}).
Thus, the quantity of \eqn{ExtraPest_i} re-exported from country \eqn{i} to all countries
\eqn{j} is approximated by \eqn{ExtraPest_i \sum_{j \neq i} R_{ij}}, and the quantity
of \eqn{ExtraPest_j} re-exported from all countries \eqn{j} to country \eqn{i} as
\eqn{\sum_{j \neq i} ExtraPest_j R_{ji}}.
}
}
\examples{
## Example with simulated trade data for Northern America
library(dplyr)
data("datatrade_NorthAm")
# Total extra-import data: data contains imports from 5 third countries (column partner). 
extra_total <- datatrade_NorthAm$extra_import
# Extra-import data from countries where the pest is present (e.g., CNTR_1 and CNTR_2)
CNTR_pest <- c("CNTR_1", "CNTR_2")
extra_pest <- datatrade_NorthAm$extra_import \%>\% filter(partner\%in\%CNTR_pest)
# Intra-trade data
intra_trade  <- datatrade_NorthAm$intra_trade
# Internal production data
internal_production  <- datatrade_NorthAm$internal_production
# Generate trade data (TradeData object)
trade_NorthAm <- trade_data(extra_total = extra_total,
                            extra_pest = extra_pest,
                            intra_trade = intra_trade,
                            internal_production = internal_production)
# Calculation of the Ntrade for each time period
ntrade_NorthAm <- ntrade(trade_data = trade_NorthAm)
head(ntrade_NorthAm)
# Ntrade summary for the time periods
ntrade_NorthAm_summary <- ntrade(trade_data = trade_NorthAm,
                                 summarise_result = c("mean", "sd", 
                                                      "quantile(0.025)", 
                                                      "median",
                                                      "quantile(0.975)"))
head(ntrade_NorthAm_summary)
# Plot the median of Ntrade
library(ggplot2)
plot_countries(data = ntrade_NorthAm_summary,
               iso_col = "country_IDs", 
               values_col = "median") +
  xlim(-180,-20) + ylim(0,90)

## Example with simulated trade data for Europe 
# Load data
data("datatrade_EU")
# Total extra-import data: the total import is identified as partner "Extra_Total"
extra_total <- datatrade_EU$extra_import \%>\% filter(partner=="Extra_Total")
# Extra-import data from countries where the pest is present
extra_pest <- datatrade_EU$extra_import \%>\% filter(partner!="Extra_Total")
# Intra-trade data
intra_trade  <- datatrade_EU$intra_trade
# Internal production data
internal_production  <- datatrade_EU$internal_production
# Generate trade data (TradeData object)
trade_EU <- trade_data(extra_total = extra_total,
                       extra_pest = extra_pest,
                       intra_trade = intra_trade,
                       internal_production = internal_production)
# Ntrade mean and sd for the time periods
ntrade_EU <- ntrade(trade_data = trade_EU,
                    summarise_result = c("mean", "sd"))
# Plot Ntrade mean
plot_countries(data = ntrade_EU, 
               iso_col="country_IDs", 
               values_col="mean") +
  xlim(-40,50) + ylim(25,70)
# Ntrade for selected countries and a specific time period
# Sample 5 countries from trade data
country_IDs <- sample(unique(trade_EU$total_trade$country_IDs), 5)
ntrade_EU_s <- ntrade(trade_data = trade_EU,
                      filter_IDs = country_IDs,
                      filter_period = 2020)
head(ntrade_EU_s)
# Plot Ntrade result
plot_countries(data = ntrade_EU_s, 
               iso_col="country_IDs", 
               values_col="Ntrade_2020") +
  xlim(-40,50) + ylim(25,70)

}
\seealso{
\code{\link[=trade_data]{trade_data()}}
}
