---
output:
  html_document: default
  pdf_document: default
---

# Overview of $N_{trade}$ Shiny app

<hr style="border:1px solid #1E68BA">
<br>

$N_{trade}$ is defined as "the potentially infected/infested quantity of commodities 
imported by a country from third countries where the pest is present".

This application is meant to calculate $N_{trade}$ for countries included in the NUTS 
([Nomenclature of territorial units for statistics](https://ec.europa.eu/eurostat/web/nuts)) 
coding system, which includes European Union Member States, EFTA countries, candidate 
countries and potential candidates. Note that it is not necessary to consider all 
countries included in this list.

For other countries worldwide, the calculation of $N_{trade}$ can be performed using 
the `ntrade` function from the `qPRAentry` package.

For the use of this application, trade data of the commodities of interest, i.e., 
potential carriers of the pests under assessment, are required. The required trade data are: 

- $ExtraTotal Import$: Total quantity of commodity from third countries imported by the 
countries of interest.
- $ExtraPest Import$: Quantity of commodity from third countries where the pest under 
assessment is present, imported by the countries of interest.
- $Intra Trade$: Quantity of commodity traded between the countries of interest.
- $Internal Production$: Quantity of commodity produced in the countries of interest.

All the trade data required can be accessed 
through the [Eurostat database](https://ec.europa.eu/eurostat/data/database).

<br>

## Calculation of $N_{trade}$

<br>

For a given country $i$, $N_{trade_i}$ is calculated taking into account the quantity 
of commodity imported by country $i$ from third countries where the pest is present, and 
the internal trade of this commodity, i.e., export and import between country $i$ and other 
countries $j$, with $j \neq i$. Thus, $N_{trade_i}$ is approached as:

$$
N_{trade_i} = ExtraPest_i - ExtraPest_i \sum_{j \neq i} R_{ij} + \sum_{j \neq i} ExtraPest_j R_{ji},
$$

<br>

where $ExtraPest_i$ and $ExtraPest_j$ are the quantity of commodity imported from third 
countries where the pest is present by country $i$ and country $j$, respectively. $R_{ij}$ 
and $R_{ji}$ represent the proportion of commodity exported from $i$ to $j$ ($IntraExp_{ij}$), 
and from $j$ to $i$ ($IntraExp_{ji}$), respectively, out of the total available commodity in 
the exporter country. This total quantity is considered as the sum of the internal production 
of the country ($IP$) and the total quantity imported from third countries ($ExtraTotal$), 
regardless of whether the pest is present. Thus, $R_{ij}$ and $R_{ji}$ are defined as: 

  $$R_{ij} = \frac{IntraExp_{ij}}{IP_i + ExtraTotal_i},$$ 
  $$R_{ji} = \frac{IntraExp_{ji}}{IP_j + ExtraTotal_j}.$$

Based on these proportions, the quantity of $ExtraPest_i$ re-exported from country $i$ 
to all countries $j$ is approximated by $ExtraPest_i \sum_{j \neq i} R_{ij}$, and 
the quantity of $ExtraPest_j$ re-exported from all countries $j$ to country $i$ as 
$\sum_{j \neq i} ExtraPest_j R_{ji}$.

<br>

## $N_{trade}$ redistribution to NUTS2

<br>

After calculating $N_{trade}$ at the national level (NUTS0), the next step involves 
redistributing these quantities to NUTS2 regions. 

Users have two options for redistributing $N_{trade}$ data to NUTS2 regions:

1. **Human population-based redistribution using Eurostat data**

   - **Overview:** This option allows users to redistribute $N_{trade}$ data to NUTS2 
   regions proportionally based on human population data. The 
   [human population data](https://ec.europa.eu/eurostat/databrowser/product/page/demo_r_pjangrp3) 
   is sourced from Eurostat and is already integrated into the application.
   
   - **How it works:** The $N_{trade_i}$ value for a country $i$ is redistributed 
   to its respective NUTS2 regions $k$. This redistribution is done proportionally 
   based on the human population of each region $k$. Thus, the quantity allocated to a specific 
   region $k$ is calculated as $N_{trade_k} = N_{trade_i} \cdot (Population_k / Population_i)$.


2. **Custom redistribution using user-uploaded data**

   - **Overview:** This option provides users with the flexibility to perform a personalised 
   redistribution based on custom data. Users can upload their own data frame containing 
   specific values for each NUTS2 region.
   
   - **How it works:** Users upload a file containing the NUTS2 codes along with the 
   corresponding values from which $N_{trade}$ will be proportionally redistributed 
   (e.g., consumption data). The application will then use this custom data frame to redistribute 
   the $N_{trade_i}$ value for a country $i$ to its respective NUTS2 regions $k$ proportionally 
   according to the value of each region $k$. Thus, the redistributed quantity for each region $k$ 
   is calculated as $N_{trade_k} = N_{trade_i} \cdot (Value_k / Value_i)$.
   

<br>

## Downloading results

<br>

Upon completion of the analysis, the user will have the option to download a ZIP file containing:

- A comprehensive report summarising the analysis results (Ntrade_report.html)
- A CSV file with the $N_{trade}$ table results at NUTS0 level (Ntrade_NUTS0.csv)
- A CSV file with the $N_{trade}$ table redistribution results at NUTS2 level (Ntrade_NUTS2.csv)

To proceed with the analysis, navigate to the $N_{trade}$ tab at the top of the page.
