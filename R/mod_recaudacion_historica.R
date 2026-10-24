# ------------------------------------------------------------------
# Pestaña: Recaudación Histórica (SAT) -- de goyo/pt6.R, sin cambios
# de lógica, solo envuelto como módulo Shiny.
# ------------------------------------------------------------------
mod_recaudacion_historica_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "Opciones",
      width = 300,

      radioButtons(
        ns("precios"),
        "Tipo de cifras",
        choices = c(
          "Pesos Nominales"        = "nominal",
          "Pesos Reales"           = "real"
        ),
        selected = "nominal"
      ),

      sliderInput(
        ns("rango_anios"),
        "Periodo",
        min = rango_anios[1], max = rango_anios[2],
        value = rango_anios, step = 1, sep = ""
      ),

      helpText(
        sprintf(
          "Datos mensuales de enero %d a %s %d. Cifras en millones de pesos. ",
          rango_anios[1], tolower(MESES[ultimo$mes_num]), ultimo$anio
        ),
        "La ponderación usa el INPC disponible en Banxico para el primer trimestre del 2018, usando como base de la ponderación el valor de agosto 2026"
      )
    ),

    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,

      card(
        card_header("Participación del ISR en la recaudación total"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,
            selectInput(
              ns("mes_sel"), "Mes",
              choices = opciones_mes(ultimo$anio),
              selected = ultimo$mes_num,
              selectize = FALSE, width = "100%"
            ),
            selectInput(
              ns("anio_sel"), "Año",
              choices = sort(unique(datos$anio), decreasing = TRUE),
              selected = ultimo$anio,
              selectize = FALSE, width = "100%"
            )
          ),
          div(class = "fs-1 fw-bold",
              style = paste0("color:", COLOR_ISR),
              textOutput(ns("proporcion"))),
          div(class = "text-muted small", textOutput(ns("detalle_proporcion")))
        )
      ),

      card(
        card_header("Recaudación acumulada"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,
            div(
              div(class = "text-muted small", "Recaudación total"),
              div(class = "fs-3 fw-bold",
                  style = paste0("color:", COLOR_TOTAL),
                  textOutput(ns("acum_total"))),
              div(class = "text-muted small", textOutput(ns("acum_total_millones")))
            ),
            div(
              div(class = "text-muted small", "ISR"),
              div(class = "fs-3 fw-bold",
                  style = paste0("color:", COLOR_ISR),
                  textOutput(ns("acum_isr"))),
              div(class = "text-muted small", textOutput(ns("acum_isr_millones")))
            )
          ),
          div(class = "text-muted small", textOutput(ns("detalle_acumulado")))
        )
      )
    ),

    card(
      card_header("Recaudación Tributaria"),
      plotlyOutput(ns("graf"), height = "460px"),
      card_body(
        fill = FALSE,
        p("La fecha límite para realizar la declaración anual de Impuestos ante el SAT es el 31 de Marzo para personas morales y el 30 de Abril para personas físicas. Esto explica la estacionalidad de los datos")
      ),
      card_footer("Fuente: SAT, ingresos tributarios del Gobierno Federal.")
    )
  )
}

mod_recaudacion_historica_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    base <- reactive({
      d <- datos %>%
        filter(anio >= input$rango_anios[1], anio <= input$rango_anios[2])

      if (input$precios == "real") {
        d %>% mutate(v_total = total_real, v_isr = isr_real)
      } else {
        d %>% mutate(v_total = total, v_isr = isr)
      }
    })

    titulo_y <- reactive({
      if (input$precios == "real") "Millones de pesos de agosto 2026"
      else "Millones de pesos corrientes"
    })

    observeEvent(input$anio_sel, {
      disp   <- meses_disponibles(as.integer(input$anio_sel))
      actual <- isolate(input$mes_sel)
      sel    <- if (!is.null(actual) && as.integer(actual) %in% disp) {
        as.integer(actual)
      } else {
        max(disp)
      }
      updateSelectInput(
        session, "mes_sel",
        choices  = opciones_mes(as.integer(input$anio_sel)),
        selected = sel
      )
    })

    fila_sel <- reactive({
      req(input$anio_sel, input$mes_sel)
      f <- datos %>%
        filter(anio == as.integer(input$anio_sel),
               mes_num == as.integer(input$mes_sel))
      req(nrow(f) == 1)
      f
    })

    output$proporcion <- renderText({
      f <- fila_sel()
      sprintf("%.1f%%", 100 * f$isr / f$total)
    })

    output$detalle_proporcion <- renderText({
      f    <- fila_sel()
      cols <- columnas_valor(input$precios)
      unidad <- if (input$precios == "real") "pesos de agosto 2026" else "pesos corrientes"
      sprintf("%s %d: ISR $%s de $%s millones de %s",
              MESES[f$mes_num], f$anio,
              fmt_millones(f[[cols[2]]]), fmt_millones(f[[cols[1]]]), unidad)
    })

    output$acum_total <- renderText(fmt_billones(sum(base()$v_total)))
    output$acum_isr   <- renderText(fmt_billones(sum(base()$v_isr)))

    output$acum_total_millones <- renderText(
      paste0("$", fmt_millones(sum(base()$v_total)), " millones")
    )
    output$acum_isr_millones <- renderText(
      paste0("$", fmt_millones(sum(base()$v_isr)), " millones")
    )

    output$detalle_acumulado <- renderText({
      d <- base()
      req(nrow(d) > 0)
      ini <- d[1, ]
      fin <- d[nrow(d), ]
      unidad <- if (input$precios == "real") "pesos de agosto 2026" else "pesos corrientes"
      sprintf("De %s %d a %s %d, en %s",
              tolower(MESES[ini$mes_num]), ini$anio,
              tolower(MESES[fin$mes_num]), fin$anio, unidad)
    })

    output$graf <- renderPlotly({
      hacer_grafica(base(), titulo_y())
    })
  })
}
