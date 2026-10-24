# ------------------------------------------------------------------
# Tablero: ISR y recaudacion fiscal en Mexico
# Ejecutar desde la carpeta del proyecto: shiny::runApp()
# ------------------------------------------------------------------
source("global.R")

ui <- navbarPage(
  title = "Impuesto sobre la renta en México y comparación internacional",
  theme = tema,
  tabPanel("Inicio", portada_ui()),
  tabPanel("Composición de la recaudación", mod_composicion_ui("composicion")),
  tabPanel("ISR en la última década", mod_isr_decada_ui("decada")),
  tabPanel("¿Y tú, dónde estás?", mod_calculadora_ui("calculadora")),
  tabPanel("Recaudación Histórica", mod_recaudacion_historica_ui("historica")),
  tabPanel("Comparación con otros países de la OCDE", mod_comparacion_ocde_ui("ocde")),
  tabPanel("Conclusiones", conclusiones_ui()),
  tabPanel("Referencias", referencias_ui())
)

server <- function(input, output, session) {
  mod_composicion_server("composicion")
  mod_isr_decada_server("decada")
  mod_calculadora_server("calculadora")
  mod_recaudacion_historica_server("historica")
  mod_comparacion_ocde_server("ocde")
}

shinyApp(ui, server)
