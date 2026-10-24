# Tablero: ISR y recaudación fiscal en México

Tres pestañas: composición de la recaudación fiscal (vs. % del PIB), evolución
del ISR 2015–2026 (tasas marginal/promedio, overlay de deciles ENIGH, y
proporción del salario mínimo por tramo), y una calculadora personal de ISR.

## Cómo correrlo
1. Instalar paquetes (una vez):
   `install.packages(c("shiny","bslib","ggplot2","plotly","dplyr","tidyr","readxl","scales","shinyWidgets"))`
2. Verificar los datos: abrir R en esta carpeta y correr `source("R/verificar.R")` → debe terminar en `TODO OK`.
3. Correr el tablero: `shiny::runApp()`

Todas las rutas son relativas a esta carpeta (`isr_dashboard/`), así que basta
compartir la carpeta `shiny` completa (que la contiene) para que funcione en
otra máquina.

## Estructura
- `app.R` — UI (tema `bs_theme` Bootswatch "brite") y server.
- `global.R` — paquetes y `source()` de todo lo demás.
- `data/raw/` — copias de los 3 Excel fuente (composición de tax revenue,
  ISR mensual 2015–2026, ingreso corriente por deciles).
- `R/data_*.R` — lectura y limpieza de cada fuente de datos.
- `R/mod_*.R` — un módulo Shiny (UI + server) por pestaña; `mod_tasas_plot.R`
  es la gráfica de tasa marginal/promedio reutilizada tanto en la pestaña 2
  como en la calculadora (pestaña 3).

## Fuentes
- OECD, *Revenue Statistics in Latin America and the Caribbean 2026*.
- El Contribuyente, tablas de ISR: https://www.elcontribuyente.mx/tablas-isr/
- INEGI, ENIGH 2024 (tabulados básicos, 2025).
- Gobierno de México, CONASAMI, Evolución del Salario Mínimo.
