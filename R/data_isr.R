# ------------------------------------------------------------------
# ISR mensual 2015-2026 (tarifas del art. 96 LISR) y funciones de
# calculo para un ingreso arbitrario.
# Fuente: data/raw/isr_mensual.xlsx (El Contribuyente + Gob. de Mexico)
# ------------------------------------------------------------------
library(readxl)
library(dplyr)

CONTRIBUYENTE_LINK <- "https://www.elcontribuyente.mx/tablas-isr/"
ENIGH_CITA_TABLAS2 <- "Elaboración propia con datos de ENIGH 2024 y El Contribuyente."
SALARIO_MINIMO_LINK <- "https://www.gob.mx/conasami/documentos/evolucion-del-salario-minimo"
SALARIO_MINIMO_CITA <- "Elaboración propia con datos del Gobierno de México Evolución del Salario Mínimo y El Contribuyente."

isr <- read_excel("data/raw/isr_mensual.xlsx", sheet = "Hoja1") |>
  transmute(
    anio = periodo,
    li = limite_inferior,
    ls = limite_superior,
    cf = cuota_fija,
    tasa = porcentaje,                 # tasa marginal, en %
    punto_medio = punto_medio,
    tasa_promedio = Tasa_promedio,      # ISR en punto_medio / punto_medio
    salario_minimo_mensual = Salario_minimo_mensual,
    prop_salario_minimo = `proporcion del salario minimo para alcanzar cada tasa`
  ) |>
  arrange(anio, li)

ANIOS_ISR <- sort(unique(isr$anio))

.tramo <- function(a, y) {
  t <- isr[isr$anio == a, ]
  i <- pmax(findInterval(y, t$li), 1)
  t[i, ]
}

tasa_marginal <- function(a, y) .tramo(a, y)$tasa / 100

isr_pagar <- function(a, y) {
  t <- .tramo(a, y)
  t$cf + (y - t$li) * t$tasa / 100
}

tasa_promedio_de <- function(a, y) isr_pagar(a, y) / y

excedente_pagar <- function(a, y) isr_pagar(a, y) - .tramo(a, y)$cf
