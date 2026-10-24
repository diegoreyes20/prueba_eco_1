# ------------------------------------------------------------------
# Modulo reutilizable: curva de tasa marginal vs. tasa promedio para
# un año dado, con overlay opcional de deciles de ingreso y un punto
# opcional "Tu estas aqui" (usado por la pestaña de la calculadora).
# ------------------------------------------------------------------
# mismos colores que la grafica de progresividad de la ventana de
# comparacion internacional (Tasa marginal = azul marino, Tasa promedio = rosa)
COL_MARGINAL <- "#0f3f6f"
COL_PROMEDIO <- "#ea4998"
COL_PUNTO    <- "#d32f2f"

mod_tasas_plot_ui <- function(id, height = "440px", mostrar_toggle_escala = FALSE) {
  ns <- NS(id)
  tagList(
    if (mostrar_toggle_escala) {
      radioButtons(ns("escala"), NULL, inline = TRUE,
                   choices = c("Logarítmico" = "log", "Lineal" = "lin"),
                   selected = "log")
    },
    plotlyOutput(ns("plot"), height = height)
  )
}

# anio: reactive(numeric) año seleccionado
# mostrar_deciles: reactive(logical) si se debe sobreponer la distribucion por deciles
# punto_usuario: reactive() que regresa NULL o list(ingreso=, tasa=) para el punto rojo
# xlim_max: hasta donde se extiende el eje de ingreso mensual
mod_tasas_plot_server <- function(id, anio, mostrar_deciles = reactive(FALSE),
                                   punto_usuario = reactive(NULL), xlim_max = 200000) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlotly({
      a <- anio()
      t <- isr[isr$anio == a, ]
      escala_log <- isTRUE(input$escala == "log")

      marginal_d <- t |> transmute(x = li, y = tasa, serie = "Tasa marginal")
      promedio_d <- t |> transmute(x = punto_medio, y = tasa_promedio * 100, serie = "Tasa promedio")

      if (xlim_max > max(marginal_d$x)) {
        marginal_d <- bind_rows(marginal_d,
          tibble(x = xlim_max, y = tail(marginal_d$y, 1), serie = "Tasa marginal"))
        promedio_d <- bind_rows(promedio_d,
          tibble(x = xlim_max, y = tasa_promedio_de(a, xlim_max) * 100, serie = "Tasa promedio"))
      }

      if (escala_log) {
        marginal_d$x <- pmax(marginal_d$x, 1)
        promedio_d$x <- pmax(promedio_d$x, 1)
      }

      p <- ggplot() +
        geom_step(data = marginal_d, aes(x, y, color = serie, group = serie,
                                          text = paste0("Tasa marginal: ", round(y, 1), "%<br>Desde: $", format(round(x), big.mark = ","))),
                  direction = "hv", linewidth = 1) +
        geom_line(data = promedio_d, aes(x, y, color = serie, group = serie,
                                          text = paste0("Tasa promedio: ", round(y, 1), "%<br>Ingreso: $", format(round(x), big.mark = ","))),
                  linewidth = 1)

      if (isTRUE(mostrar_deciles()) && a %in% ANIOS_DECILES) {
        dd <- deciles[deciles$anio == a, ]
        p <- p +
          geom_vline(data = dd, aes(xintercept = ingreso_mensual), color = "#5f6670", alpha = 0.25, linewidth = 0.6) +
          geom_text(data = dd, aes(x = ingreso_mensual, y = 34, label = paste0("D", decil)),
                    angle = 90, size = 2.8, color = "#5f6670", alpha = 0.6, vjust = -0.3)
      }

      pu <- punto_usuario()
      if (!is.null(pu)) {
        p <- p + geom_point(aes(x = pu$ingreso, y = pu$tasa * 100,
                                 text = paste0("Tú estás aquí<br>Ingreso: $", format(round(pu$ingreso), big.mark = ","),
                                               "<br>Tasa promedio: ", round(pu$tasa * 100, 1), "%")),
                             color = COL_PUNTO, size = 3.5) +
          annotate("text", x = pu$ingreso, y = pu$tasa * 100 + 3, label = "Tú estás aquí",
                   color = COL_PUNTO, size = 3.3, fontface = "bold")
      }

      fmt_eje <- label_dollar(accuracy = 1, scale_cut = cut_short_scale())
      escala_x <- if (escala_log) {
        scale_x_log10(labels = fmt_eje)
      } else {
        scale_x_continuous(labels = fmt_eje)
      }

      p <- p +
        scale_color_manual(values = c("Tasa marginal" = COL_MARGINAL, "Tasa promedio" = COL_PROMEDIO)) +
        escala_x +
        scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 37)) +
        { if (!escala_log) coord_cartesian(xlim = c(0, xlim_max)) } +
        labs(x = "Ingreso mensual", y = "Tasa", color = NULL) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom", panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank())

      ggplotly(p, tooltip = "text") |>
        layout(legend = list(orientation = "h", x = 0, y = -0.3, title = list(text = "")),
               hovermode = "x unified", margin = list(t = 10))
    })
  })
}
