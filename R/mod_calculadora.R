# ------------------------------------------------------------------
# Pestaña 3: ¿Y tú, dónde estás? -- calculadora de ISR personal
# ------------------------------------------------------------------
mod_calculadora_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("¿Y tú, dónde estás?"),
    fluidRow(
      column(6, numericInput(ns("salario"), "Introduce tu salario mensual:",
                              value = 15000, min = 0, step = 500)),
      column(6, selectInput(ns("anio"), "Año de la tarifa:", choices = ANIOS_ISR, selected = max(ANIOS_ISR)))
    ),
    uiOutput(ns("resumen")),

    h4(uiOutput(ns("titulo_tabla4"))),
    plotlyOutput(ns("plot4"), height = "270px"),

    hr(),
    fluidRow(
      column(9, h4("Tú estás aquí, dentro de la curva de tasas")),
      column(3, checkboxInput(ns("deciles"), "Mostrar distribución por deciles", FALSE))
    ),
    uiOutput(ns("nota_deciles")),
    mod_tasas_plot_ui(ns("tasas"), mostrar_toggle_escala = TRUE)
  )
}

mod_calculadora_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    anio <- reactive(as.numeric(input$anio))
    ingreso_valido <- reactive(isTRUE(input$salario > 0))

    isr_calc <- reactive({
      req(ingreso_valido())
      y <- input$salario
      a <- anio()
      list(
        ingreso = y,
        isr = isr_pagar(a, y),
        marginal = tasa_marginal(a, y),
        promedio = tasa_promedio_de(a, y),
        excedente = excedente_pagar(a, y),
        cf = isr_pagar(a, y) - excedente_pagar(a, y)
      )
    })

    output$resumen <- renderUI({
      req(ingreso_valido())
      r <- isr_calc()
      div(class = "tarjeta", style = "background:#fff;border:1px solid #e3e3df;border-radius:10px;padding:14px 18px;margin:12px 0",
          p(HTML(paste0("Con un ingreso mensual de ", strong(pesos(r$ingreso)),
                         " en ", anio(), ", deberás pagar ", strong(pesos(r$isr)),
                         " de ISR. Tu tasa marginal (la del último peso ganado) es ",
                         strong(pct(r$marginal)), " y tu tasa promedio (lo que pagas en total entre tu ingreso) es ",
                         strong(pct(r$promedio)), ".")))
      )
    })

    output$titulo_tabla4 <- renderUI({
      req(ingreso_valido())
      paste0("Estos ", pesos(isr_calc()$isr), " se componen de la siguiente manera")
    })

    output$plot4 <- renderPlotly({
      req(ingreso_valido())
      r <- isr_calc()
      d <- tibble(
        componente = factor(c("Cuota fija", "% sobre excedente"), levels = c("% sobre excedente", "Cuota fija")),
        valor = c(r$cf, r$excedente)
      )
      p <- ggplot(d, aes(x = "ISR", y = valor, fill = componente,
                          text = paste0(componente, ": ", pesos(valor)))) +
        geom_col(width = 0.5) +
        coord_flip() +
        scale_fill_manual(values = c("Cuota fija" = "#9aa0a6", "% sobre excedente" = "#f4b400")) +
        labs(x = NULL, y = "Pesos", fill = NULL) +
        theme_minimal(base_size = 13) +
        theme(axis.text.y = element_blank(), legend.position = "bottom",
              legend.text = element_text(size = 11))

      ggplotly(p, tooltip = "text") |>
        layout(legend = list(orientation = "h", y = -0.5, font = list(size = 12)),
               margin = list(b = 70))
    })

    punto_usuario <- reactive({
      if (!ingreso_valido()) return(NULL)
      r <- isr_calc()
      list(ingreso = r$ingreso, tasa = r$promedio)
    })

    output$nota_deciles <- renderUI({
      if (isTRUE(input$deciles) && !(anio() %in% ANIOS_DECILES)) {
        tags$p(style = "color:#9a3412;font-size:13px",
               "No hay datos de la distribución del ingreso (ENIGH) para ", anio(),
               ". Años disponibles: ", paste(ANIOS_DECILES, collapse = ", "), ".")
      }
    })

    mod_tasas_plot_server("tasas", anio, reactive(input$deciles), punto_usuario, xlim_max = 1e7)
  })
}
