# ------------------------------------------------------------------
# Ingreso corriente por deciles (ENIGH) -> ingreso mensual por decil
# Fuente: data/raw/ingreso_deciles.xlsx
# ------------------------------------------------------------------
library(readxl)
library(dplyr)

deciles <- read_excel("data/raw/ingreso_deciles.xlsx", sheet = "Hoja1") |>
  rename(anio = `Año`, decil = Decil, ingreso_trimestral = `ingreso corriente total promedio trimestral por hogar`) |>
  filter(decil != "total") |>
  transmute(anio, decil = as.integer(decil), ingreso_mensual = ingreso_trimestral / 3)

ANIOS_DECILES <- sort(unique(deciles$anio))
