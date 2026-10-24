# ------------------------------------------------------------------
# Composición de la recaudación fiscal y tax revenue como % del PIB
# Fuente: data/raw/composicion_tax_revenue.xlsx (OECD Revenue Statistics
# in Latin America and the Caribbean 2026)
# ------------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)

OCDE_LINK <- "https://www.oecd.org/content/dam/oecd/en/publications/reports/2026/05/revenue-statistics-in-latin-america-and-the-caribbean-2026_436209c8/2f21f131-en.pdf"
OCDE_CITA <- "Realización propia con datos de la OECD Revenue Statistics in Latin America and the Caribbean 2026."

# traducción de cada rubro (en -> es) y color fijo para todo el tablero.
# A partir de "Social security contributions" los rubros llevan un
# degradado de azul muted, de mas claro a mas oscuro.
.azules_degradado <- colorRampPalette(c("#a9c6d8", "#18314f"))(4)

RUBROS <- tribble(
  ~rubro_en, ~rubro_es, ~color,
  "Total tax revenue", "Ingresos tributarios totales", "#9aa0a6",
  "Taxes on income and profits", "Impuestos sobre la renta y las utilidades", "#f4b400",
  "Social security contributions", "Contribuciones a la seguridad social", .azules_degradado[1],
  "Taxes on payroll", "Impuestos sobre la nómina", .azules_degradado[2],
  "Taxes on goods and services", "Impuestos sobre bienes y servicios", .azules_degradado[3],
  "Taxes on property", "Impuestos sobre la propiedad", .azules_degradado[4]
)

DEFINICIONES <- tribble(
  ~rubro_en, ~definicion,
  "Total tax revenue", "Suma de todos los recursos recaudados por el gobierno.",
  "Taxes on income and profits", "Gravámenes directos aplicados sobre los ingresos netos de las personas físicas (como salarios y ganancias) y sobre las utilidades operativas de las empresas e instituciones corporativas.",
  "Social security contributions", "Aportaciones obligatorias sobre el empleo realizadas por trabajadores y empleadores con el fin de financiar programas específicos de seguro social, tales como salud, pensiones e invalidez.",
  "Taxes on payroll", "Tributos cobrados sobre la masa salarial bruta de las empresas que ingresan al presupuesto general sin otorgar una prestación, derecho o contraprestación individual directa al trabajador.",
  "Taxes on property", "Impuestos aplicados a la tenencia, uso o transferencia de activos inmobiliarios y patrimonio acumulado, incluyendo el predial, herencias, donaciones e impuestos sobre transacciones de capital.",
  "Taxes on goods and services", "Impuestos indirectos aplicados sobre la producción, venta o consumo de productos y servicios."
)

.leer_composicion <- function() {
  raw <- read_excel("data/raw/composicion_tax_revenue.xlsx", sheet = "Hoja1", col_names = FALSE)
  anios <- as.numeric(raw[1, -1])
  anios[anios == 200] <- 2000   # typo en el archivo fuente
  etiquetas <- as.character(raw[[1]])[-1]

  d <- raw[-1, ]
  names(d) <- c("etiqueta", paste0("y", anios))
  d$etiqueta <- etiquetas

  d <- d |>
    filter(!grepl("^suma", etiqueta, ignore.case = TRUE)) |>
    mutate(
      metric = if_else(grepl("as % of GDP$", etiqueta), "pib", "comp"),
      rubro_en = sub(" \\(1000\\)", "", etiqueta) |>
        sub(" as % of GDP$", "", x = _) |>
        sub(" as % of total tax revenue$", "", x = _)
    ) |>
    select(-etiqueta) |>
    pivot_longer(starts_with("y"), names_to = "anio", values_to = "valor") |>
    mutate(anio = as.numeric(sub("^y", "", anio)), valor = as.numeric(valor))

  # "Total tax revenue" no tiene fila "% of total" en la fuente (es el 100% por definición)
  total_comp <- d |>
    filter(rubro_en == "Total tax revenue", metric == "pib") |>
    mutate(metric = "comp", valor = 100)

  bind_rows(d, total_comp) |>
    inner_join(RUBROS, by = "rubro_en") |>
    arrange(anio, rubro_en)
}

tax_rev <- .leer_composicion()
ANIOS_TAXREV <- sort(unique(tax_rev$anio))
