# ------------------------------------------------------------------
# Pestaña 2: ISR en la última década
# ------------------------------------------------------------------
ANIOS_TABLA2 <- ANIOS_ISR[ANIOS_ISR >= 2016]
ANIOS_TABLA3 <- c(2016, 2018, 2020, 2022, 2024)

mod_isr_decada_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("ISR en la última década"),

    h4("Comparación de tasas marginales y promedio"),
    fluidRow(
      column(9, radioGroupButtons(ns("anio2"), "Año:", choices = ANIOS_TABLA2,
                                   selected = max(ANIOS_TABLA2), justified = TRUE, status = "light")),
      column(3, checkboxInput(ns("deciles"), "Mostrar distribución por deciles", FALSE))
    ),
    uiOutput(ns("nota_deciles")),
    fluidRow(
      column(3,
             # --- EDITAR AQUI: interpretacion ---
             div(style = "min-height:360px;border:1px dashed #c9c9c9;border-radius:8px;padding:14px",
                 em("Interpretación"))),
      column(9, mod_tasas_plot_ui(ns("tasas"), mostrar_toggle_escala = TRUE))
    ),
    tags$p(style = "color:#5f6670;font-size:13px;margin-top:6px",
           ENIGH_CITA_TABLAS2, " ",
           tags$a(href = CONTRIBUYENTE_LINK, target = "_blank", "(El Contribuyente)"), " · ",
           "INEGI. Encuesta Nacional de Ingresos y Gastos de los Hogares. ENIGH 2024. Tabulados básicos. 2025."),

    hr(),
    h4("Proporción del salario mínimo para alcanzar el límite inferior de cada tasa marginal"),
    radioGroupButtons(ns("anio3"), "Año:", choices = ANIOS_TABLA3, selected = max(ANIOS_TABLA3), justified = TRUE, status = "light"),
    plotlyOutput(ns("plot3"), height = "440px"),
    tags$p(style = "color:#5f6670;font-size:13px;margin-top:6px",
           SALARIO_MINIMO_CITA, " ", tags$a(href = SALARIO_MINIMO_LINK, target = "_blank", "(Fuente)")),

    hr(),
    h4("Evolución de la proporción del salario mínimo necesaria para cada tramo (2015–2026)"),
    plotlyOutput(ns("plot3b"), height = "420px"),
    tags$p(style = "color:#5f6670;font-size:13px;margin-top:6px",
           SALARIO_MINIMO_CITA, " ", tags$a(href = SALARIO_MINIMO_LINK, target = "_blank", "(Fuente)")),

    # --- EDITAR AQUI: interpretacion ---
    div(style = "min-height:90px;border:1px dashed #c9c9c9;border-radius:8px;padding:14px;margin-top:6px",
        em("Interpretación"))
  )
}

mod_isr_decada_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    anio2 <- reactive(as.numeric(input$anio2))
    mod_tasas_plot_server("tasas", anio2, reactive(input$deciles), xlim_max = 1e7)

    output$nota_deciles <- renderUI({
      if (isTRUE(input$deciles) && !(anio2() %in% ANIOS_DECILES)) {
        tags$p(style = "color:#9a3412;font-size:13px",
               "No hay datos de la distribución del ingreso (ENIGH) para ", anio2(),
               ". Años disponibles: ", paste(ANIOS_DECILES, collapse = ", "), ".")
      }
    })

    output$plot3 <- renderPlotly({
      a <- as.numeric(input$anio3)
      t <- isr[isr$anio == a, ] |>
        mutate(tramo = factor(round(li), levels = round(li)),
               etiqueta = paste0(round(prop_salario_minimo, 1), " SM"))

      d <- t |>
        select(tramo, etiqueta, li, salario_minimo_mensual) |>
        pivot_longer(c(li, salario_minimo_mensual), names_to = "serie", values_to = "valor") |>
        mutate(serie = recode(serie, li = "Límite inferior del tramo", salario_minimo_mensual = "Salario mínimo mensual"))

      y_min <- sort(unique(t$li))[2]

      p <- ggplot(d, aes(x = tramo, y = valor, fill = serie,
                          text = paste0(serie, ": $", format(round(valor), big.mark = ","),
                                        if_else(serie == "Límite inferior del tramo", paste0("<br>", etiqueta, " para alcanzarlo"), "")))) +
        geom_col(position = position_dodge(width = 0.8), width = 0.75) +
        scale_fill_manual(values = c("Límite inferior del tramo" = "#18314f", "Salario mínimo mensual" = "#8bbeb2")) +
        scale_y_continuous(labels = label_dollar(big.mark = ",")) +
        coord_cartesian(ylim = c(y_min, NA)) +
        labs(x = "Límite inferior del tramo ($/mes)", y = "Pesos", fill = NULL) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom", axis.text.x = element_text(angle = 40, hjust = 1))

      ggplotly(p, tooltip = "text") |> layout(legend = list(orientation = "h", y = -0.35))
    })

    output$plot3b <- renderPlotly({
      d <- isr |>
        group_by(anio) |>
        arrange(li) |>
        mutate(tramo_idx = row_number()) |>
        ungroup() |>
        filter(tramo_idx > 1)   # se ignora la primera banda (la que inicia en 0)

      niveles_tramo <- sort(unique(d$tramo_idx))
      paleta_azul_marino <- colorRampPalette(c("#a9c6d8", "#0a1f33"))(length(niveles_tramo))
      etiquetas_tramo <- paste0("Tramo ", niveles_tramo)

      d <- d |> mutate(tramo_lbl = factor(paste0("Tramo ", tramo_idx), levels = etiquetas_tramo))

      p <- ggplot(d, aes(x = anio, y = prop_salario_minimo, color = tramo_lbl, group = tramo_lbl,
                          text = paste0(tramo_lbl, " (", anio, "): ", round(prop_salario_minimo, 1), " SM"))) +
        geom_line(linewidth = 1) +
        geom_point(size = 1.4) +
        scale_color_manual(values = setNames(paleta_azul_marino, etiquetas_tramo)) +
        scale_x_continuous(breaks = ANIOS_ISR) +
        labs(x = NULL, y = "Veces el salario mínimo mensual", color = NULL) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom", axis.text.x = element_text(angle = 40, hjust = 1))

      ggplotly(p, tooltip = "text") |> layout(legend = list(orientation = "h", y = -0.4))
    })
  })
}
