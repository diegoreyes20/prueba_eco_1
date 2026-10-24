# ------------------------------------------------------------------
# global.R: paquetes, tema y carga de datos. Rutas siempre relativas
# a la carpeta de la app (isr_dashboard/), para que baste compartir
# la carpeta "shiny" completa.
# ------------------------------------------------------------------
library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(readxl)
library(scales)
library(shinyWidgets)

tema <- bs_theme(version = 5, bootswatch = "brite") |>
  bs_add_rules("
    .navbar { background-color: $warning !important; }
    .navbar .navbar-brand,
    .navbar .nav-link,
    .navbar .nav-link:hover,
    .navbar .nav-link.active { color: $black !important; }
  ")

source("R/data_tax_revenue.R")
source("R/data_isr.R")
source("R/data_deciles.R")
source("R/data_goyo.R")

source("R/mod_tasas_plot.R")
source("R/mod_composicion.R")
source("R/mod_isr_decada.R")
source("R/mod_calculadora.R")
source("R/mod_recaudacion_historica.R")
source("R/mod_comparacion_ocde.R")
source("R/mod_portada.R")
source("R/mod_conclusiones.R")
source("R/mod_referencias.R")

pct    <- function(x, d = 1) paste0(formatC(100 * x, format = "f", digits = d), "%")
pesos  <- label_dollar(accuracy = 1, big.mark = ",")
