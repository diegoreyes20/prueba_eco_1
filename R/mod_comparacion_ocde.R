# ------------------------------------------------------------------
# Pestaña: Comparación con otros países de la OCDE -- de goyo/pt6.R,
# sin cambios de lógica, solo envuelto como módulo Shiny.
# ------------------------------------------------------------------
mod_comparacion_ocde_ui <- function(id) {
  ns <- NS(id)

  botones_anio <- div(
    class = "btn-group flex-wrap", role = "group",
    lapply(anios_ocde, function(a) {
      tags$button(
        type = "button",
        class = paste("btn btn-outline-light btn-anio", if (a == anio_inicial_mapa) "active" else ""),
        `data-anio` = a,
        a
      )
    })
  )
  script_anio <- tags$script(HTML(sprintf("
    $(document).on('click', '.btn-anio', function() {
      $('.btn-anio').removeClass('active');$(this).addClass('active');
      Shiny.setInputValue('%s', parseInt($(this).attr('data-anio'), 10),
                          {priority: 'event'});
    });
  ", ns("anio_mapa"))))

  tagList(
    script_anio,

    card(
      card_body(
        layout_columns(
          col_widths = c(7, 5),
          fill = FALSE,
          div(
            div(class = "fw-bold mb-2", "Año"),
            botones_anio
          ),
          selectInput(
            ns("medida"), "Medida",
            choices = OPCIONES_MEDIDA, selected = "total",
            selectize = FALSE, width = "100%"
          )
        )
      )
    ),

    # --- Mapa y paneles de comparación de países ---
    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,

      card(
        card_header(textOutput(ns("titulo_mapa"))),
        plotlyOutput(ns("mapa"), height = "400px"),
        card_footer(
          "Un País en blanco significa que no se tiene el dato. ")
      ),

      card(
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
            span("Estadísticas comparativas"),
            div(
              style = "width: 200px;",
              selectInput(ns("pais_fijo"), label = NULL, choices = opciones_paises, selected = "MEX", selectize = FALSE, width = "100%")
            )
          )
        ),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,
            div(
              class = "border-end pe-2",
              h6(class = "text-info mb-3 fw-bold", "País Seleccionado en el Mapa"),
              uiOutput(ns("detalle_pais_click"))
            ),
            div(
              class = "ps-2",
              h6(class = "text-info mb-3 fw-bold", "País de Referencia"),
              uiOutput(ns("detalle_pais_fijo"))
            )
          )
        )
      )
    ),

    # --- Panel de Tasas del ISR y Panel Vacío lado a lado ---
    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,
      card(
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
            span(textOutput(ns("titulo_tasas"))),
            div(
              style = "width: 200px;",
              selectInput(
                ns("pais_tasas"), label = NULL,
                choices = OPCIONES_TASAS,
                selected = "MEX",
                selectize = FALSE, width = "100%"
              )
            )
          )
        ),
        card_body(
          uiOutput(ns("aviso_tasas")),
          uiOutput(ns("tabla_tasas"))
        )
      ),
      card(
        card_header(textOutput(ns("titulo_prog"))),
        card_body(
          radioButtons(
            ns("escala_x"), NULL, inline = TRUE,
            choices = c("Logarítmico" = "log",
                        "Lineal"      = "lin"),
            selected = "log"
          ),
          plotlyOutput(ns("graf_prog"), height = "340px"),
          uiOutput(ns("texto_prog"))
        ),
        card_footer(
          ""
        )
      )
    ),

    card(
      card_header("Notas sobre los datos"),
      card_body(
        class = "small",
        tags$ul(
          tags$li("Fuente: OCDE, Revenue Statistics in Latin America and the Caribbean. ",
                  "Gobierno general, en dólares corrientes y como porcentaje del PIB."),
          tags$li("El ISR de la OCDE se reporta por separado para personas físicas y para ",
                  "empresas. El ISR total es la suma de ambos y solo se muestra donde la OCDE ",
                  "reporta los dos."),
          tags$li("México: la OCDE no reporta su ISR. Se estimó con el ISR anual del SAT ",
                  "Así mismo, se utilizaron datos del Informe Tributario y de Gestión para completar las aportaciones de personas Físicas y Morales"),
          tags$li("La aportación de las personas físicas se ven reducidas porque ",
                  "las retenciones salariales se clasifican según quién las entera al SAT. Por eso, el ISR que se descuenta del salario de un trabajador puede aparecer bajo el rubro de retenciones realizadas por personas morales, aunque el ingreso gravado sea el del trabajador.")
        )
      )
    )
  )
}

mod_comparacion_ocde_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    anio_mapa <- reactive({
      a <- input$anio_mapa
      if (is.null(a) || !(as.integer(a) %in% anios_ocde)) max(anios_ocde) else as.integer(a)
    })

    trazas <- reactive({
      req(input$medida)
      d <- datos_mapa(input$medida, anio_mapa())
      d$hover <- texto_hover(d, input$medida, anio_mapa())
      armar_trazas(d)
    })

    zmax_medida <- reactive({
      req(input$medida)
      ceiling(max(ocde[[MEDIDAS[[input$medida]]$pib]], na.rm = TRUE))
    })

    output$titulo_mapa <- renderText({
      req(input$medida)
      paste0(MEDIDAS[[input$medida]]$etiqueta, ", ", anio_mapa(), " (% del PIB)")
    })

    output$mapa <- renderPlotly({
      tr   <- trazas()
      zmax <- zmax_medida()
      p <- plot_geo(source = "mapa")
      escala_puesta <- FALSE

      for (t in tr) {
        d <- t$datos
        if (t$tipo == "poligono" && t$con_dato) {
          p <- add_trace(
            p, type = "choropleth",
            locations = d$iso3, locationmode = "ISO-3",
            z = d$valor, zmin = 0, zmax = zmax,
            colorscale = ESCALA_AZUL,
            text = d$hover, hoverinfo = "text",
            marker = list(line = list(color = "#6c757d", width = 0.7)),
            showscale = !escala_puesta,
            colorbar = list(title = list(text = "% del PIB"), ticksuffix = "%",
                            len = 0.7, thickness = 14)
          )
          escala_puesta <- TRUE
        } else if (t$tipo == "poligono") {
          p <- add_trace(
            p, type = "choropleth",
            locations = d$iso3, locationmode = "ISO-3",
            z = rep(0, nrow(d)), zmin = 0, zmax = 1,
            colorscale = list(c(0, "#ffffff"), c(1, "#ffffff")),
            text = d$hover, hoverinfo = "text",
            marker = list(line = list(color = "#6c757d", width = 0.7)),
            showscale = FALSE
          )
        } else if (t$con_dato) {
          p <- add_trace(
            p, type = "scattergeo", mode = "markers",
            lat = d$lat, lon = d$lon,
            text = d$hover, hoverinfo = "text",
            marker = list(size = 11, color = d$valor, colorscale = ESCALA_AZUL,
                          cmin = 0, cmax = zmax, showscale = FALSE,
                          line = list(color = "#212529", width = 1))
          )
        } else {
          p <- add_trace(
            p, type = "scattergeo", mode = "markers",
            lat = d$lat, lon = d$lon,
            text = d$hover, hoverinfo = "text",
            marker = list(size = 11, color = "#ffffff",
                          line = list(color = "#6c757d", width = 1))
          )
        }
      }

      p %>%
        layout(
          geo = list(
            projection = list(type = "mercator"),
            lonaxis = list(range = c(-150, -20)),
            lataxis = list(range = c(-57, 33)),
            resolution = 50,
            showland = TRUE, landcolor = "#ffffff",
            showocean = TRUE, oceancolor = "#e9f0f7",
            showcountries = TRUE, countrycolor = "#adb5bd", countrywidth = 0.6,
            showcoastlines = TRUE, coastlinecolor = "#adb5bd", coastlinewidth = 0.6,
            showframe = FALSE, bgcolor = "rgba(0,0,0,0)"
          ),
          margin = list(l = 0, r = 0, t = 0, b = 0),
          showlegend = FALSE,
          hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#ffffff",
                            font = list(color = "#212529", size = 13))
        ) %>%
        config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d")) %>%
        event_register("plotly_click")
    })

    # País seleccionado con un clic en el mapa
    seleccion <- reactiveVal("MEX")
    observeEvent(event_data("plotly_click", source = "mapa"), {
      ev  <- event_data("plotly_click", source = "mapa")
      iso <- iso_desde_clic(ev, trazas())
      if (!is.null(iso)) seleccion(iso)
    })

    # Función auxiliar para generar el contenido HTML de un país y año determinados
    generar_detalle_pais <- function(iso, anio) {
      f <- ocde[ocde$iso3 == iso & ocde$anio == anio, ]
      if (nrow(f) == 0) return(p("Sin datos disponibles para este año."))

      celda_pib <- function(x) if (is.na(x)) "—" else sprintf("%.2f%%", x)
      celda_usd <- function(x) if (is.na(x)) "—" else fmt_usd(x)
      filas <- list(
        list("Recaudación tributaria total", f$recaudacion_total_pib, f$recaudacion_total_musd),
        list("ISR de personas físicas",      f$isr_personas_pib,      f$isr_personas_musd),
        list("ISR de personas morales",      f$isr_empresas_pib,      f$isr_empresas_musd),
        list("ISR total",                    f$isr_total_pib,         f$isr_total_musd)
      )

      notas <- character(0)
      if (iso == "MEX") {
        notas <- c(notas, paste0(
          "La OCDE no reporta el ISR de México. Se estimó con el ISR del SAT ",
          "(personas y empresas, nivel federal) entre el PIB implícito de la OCDE."))
      }
      if (f$isr_componentes == "Solo personas") {
        notas <- c(notas, "Solo se dispone del ISR de personas físicas.")
      }
      if (f$isr_componentes == "Solo empresas") {
        notas <- c(notas, "Solo se dispone del ISR de empresas.")
      }
      if (f$isr_componentes == "Sin dato de ISR") {
        notas <- c(notas, "No hay dato de ISR para este país en la base.")
      }
      if (isTRUE(as.logical(f$isr_derivado))) {
        notas <- c(notas, "El ISR como porcentaje del PIB se calculó a partir de los dólares.")
      }
      if (iso == "CUB") {
        notas <- c(notas, "Serie con ruptura en 2021; leer con cautela.")
      }

      tagList(
        h5(f$pais, tags$small(class = "text-muted", paste0(" · ", anio))),
        tags$table(
          class = "table table-sm mb-2",
          tags$thead(tags$tr(
            tags$th(""),
            tags$th(class = "text-end", "% del PIB"),
            tags$th(class = "text-end", "Dólares")
          )),
          tags$tbody(lapply(filas, function(r) {
            tags$tr(
              tags$td(r[[1]]),
              tags$td(class = "text-end", celda_pib(r[[2]])),
              tags$td(class = "text-end", celda_usd(r[[3]]))
            )
          }))
        ),
        if (!is.na(f$isr_participacion_pct)) {
          p(class = "mb-1 small", "El ISR total equivale a ",
            strong(sprintf("%.1f%%", f$isr_participacion_pct)),
            " de la recaudación tributaria total.")
        },
        if (length(notas) > 0) {
          tags$ul(class = "small text-muted mb-0", lapply(notas, tags$li))
        }
      )
    }

    # Renderizar el país seleccionado en el mapa
    output$detalle_pais_click <- renderUI({
      generar_detalle_pais(seleccion(), anio_mapa())
    })

    # Renderizar el país fijo seleccionado desde el menú desplegable
    output$detalle_pais_fijo <- renderUI({
      req(input$pais_fijo)
      generar_detalle_pais(input$pais_fijo, anio_mapa())
    })

    # --- Tasas del ISR de personas físicas ---
    observeEvent(seleccion(), {
      if (seleccion() %in% tasas$iso3) {
        updateSelectInput(session, "pais_tasas", selected = seleccion())
      }
    })

    output$aviso_tasas <- renderUI({
      iso <- seleccion()
      if (!(iso %in% tasas$iso3)) {
        nombre <- ocde$pais[ocde$iso3 == iso][1]
        div(class = "alert alert-secondary py-2 small",
            paste0("El documento no incluye tasas para ", nombre,
                   " (seleccionado en el mapa). Puedes elegir otro país en la lista."))
      }
    })

    output$titulo_tasas <- renderText({
      req(input$pais_tasas)
      paste0("Tasas del ISR de personas físicas: ",
             tasas$pais[tasas$iso3 == input$pais_tasas][1])
    })

    output$tabla_tasas <- renderUI({
      req(input$pais_tasas)
      tabla_tasas_ui(input$pais_tasas)
    })

    # --- Gráfica de progresividad ---
    curva <- reactive({
      req(input$pais_tasas, input$escala_x)
      curva_progresividad(input$pais_tasas, log_x = input$escala_x == "log")
    })

    output$titulo_prog <- renderText({
      req(input$pais_tasas)
      paste0("Progresividad del ISR de personas físicas: ",
             tasas$pais[tasas$iso3 == input$pais_tasas][1])
    })

    output$graf_prog <- renderPlotly({
      cu <- curva()
      validate(need(!is.null(cu), paste0(
        "El documento solo da la tasa máxima de este país (tasa única), ",
        "así que no hay tramos que graficar. Elige otro país en la lista.")))
      u <- cu$unidad[1]
      plot_ly(cu, x = ~x) %>%
        add_lines(y = ~marginal, name = "Tasa marginal (tramo)",
                  line = list(color = COLOR_TOTAL, width = 2.5, shape = "hv")) %>%
        add_lines(y = ~promedio, name = "Tasa promedio (impuesto / base)",
                  line = list(color = COLOR_ISR, width = 2.5)) %>%
        layout(
          hovermode = "x unified",
          xaxis = list(title = paste0("Base gravable (", u, ")"),
                       type = if (input$escala_x == "log") "log" else "linear",
                       tickformat = "~s", hoverformat = ",.2f", showgrid = FALSE),
          yaxis = list(title = "Tasa (%)", ticksuffix = "%", hoverformat = ".2f",
                       rangemode = "tozero"),
          legend = list(orientation = "h", x = 0, y = -0.25),
          margin = list(t = 10)
        ) %>%
        config(displaylogo = FALSE)
    })

    output$texto_prog <- renderUI({
      cu <- curva()
      req(!is.null(cu))
      p(class = "small mb-0",
        "Con una base igual al umbral del último tramo (",
        fmt_num(cu$umbral[1], if (cu$umbral[1] %% 1 == 0) 0 else 2), " ", cu$unidad[1],
        "), la tasa promedio es ", strong(fmt_tasa(round(cu$prom_top[1], 2))),
        " frente a una tasa marginal de ", strong(fmt_tasa(cu$marg_top[1])), ".")
    })
  })
}
