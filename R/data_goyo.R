# ------------------------------------------------------------------
# Datos y helpers de la sección "goyo" (recaudación histórica SAT +
# comparación internacional OCDE). Copiado sin cambios de lógica
# desde goyo/pt6.R, solo se ajustaron las rutas de los archivos.
# ------------------------------------------------------------------

#--------------------------------Cargar datos--------------------------------------#

ARCHIVO <- file.path("data/raw/goyo", "IngresosTributarios_red.xls")

MESES <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio",
           "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

cargar_datos <- function(ruta) {
  crudo <- suppressMessages(
    read_excel(ruta, col_names = FALSE, col_types = "text")
  )
  m <- as.matrix(crudo)
  m[] <- trimws(m)

  es_encabezado <- matrix(grepl("^a.o$", tolower(m)), nrow = nrow(m))
  pos <- which(es_encabezado, arr.ind = TRUE)
  if (nrow(pos) == 0) {
    stop("No encontré el encabezado 'Año' en ", ruta,
         ". Columnas leídas: ", ncol(crudo), ".")
  }
  fila0 <- pos[1, 1]
  col0  <- pos[1, 2]
  if (ncol(m) < col0 + 6) {
    stop("Se esperaban 7 columnas a partir de 'Año' (columna ", col0,
         ") pero el archivo leído tiene ", ncol(m), " columnas.")
  }

  tabla <- as.data.frame(
    m[(fila0 + 1):nrow(m), col0:(col0 + 6), drop = FALSE],
    stringsAsFactors = FALSE
  )
  names(tabla) <- c("anio", "mes", "total", "isr", "inpc", "total_real", "isr_real")

  tabla %>%
    mutate(
      anio    = suppressWarnings(as.integer(anio)),
      mes_num = match(tolower(mes), tolower(MESES)),
      across(c(total, isr, inpc, total_real, isr_real),
             ~ suppressWarnings(as.numeric(.x)))
    ) %>%
    filter(!is.na(anio), !is.na(mes_num)) %>%
    mutate(
      fecha    = as.Date(sprintf("%d-%02d-01", anio, mes_num)),
      etiqueta = paste(tolower(substr(mes, 1, 3)), anio)
    ) %>%
    arrange(fecha)
}

datos <- cargar_datos(ARCHIVO)

rango_anios <- range(datos$anio)
ultimo      <- datos[which.max(datos$fecha), ]

meses_disponibles <- function(anio) {
  sort(datos$mes_num[datos$anio == anio])
}
opciones_mes <- function(anio) {
  m <- meses_disponibles(anio)
  setNames(m, MESES[m])
}

columnas_valor <- function(tipo) {
  if (tipo == "real") c("total_real", "isr_real") else c("total", "isr")
}

fmt_millones <- function(x) {
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}
fmt_billones <- function(x_millones) {
  paste0("$", formatC(x_millones / 1e6, format = "f", digits = 2, big.mark = ","),
         " billones")
}

#------------------------------Cargar Gráficas-------------------------------------#

COLOR_TOTAL <- "#0f3f6f"  # gris oscuro
COLOR_ISR   <- "#ea4998"  # rosa

hacer_grafica <- function(d, titulo_y) {
  plot_ly(d, x = ~fecha) %>%
    add_lines(
      y = ~v_total, name = "Recaudación total",
      line = list(color = COLOR_TOTAL, width = 2.5)
    ) %>%
    add_lines(
      y = ~v_isr, name = "ISR",
      line = list(color = COLOR_ISR, width = 2.5)
    ) %>%
    layout(
      hovermode = "x unified",
      xaxis = list(title = "", showgrid = FALSE, hoverformat = "%Y-%m"),
      yaxis = list(title = titulo_y, tickformat = ",.0f",
                   hoverformat = ",.0f", rangemode = "tozero"),
      legend = list(orientation = "h", x = 0, y = -0.12),
      margin = list(t = 10)
    ) %>%
    config(displaylogo = FALSE)
}

#------------------------------Pt2 Mapita---------------------------------------#

ARCHIVO_OCDE <- file.path("data/raw/goyo", "oecd_lac_recaudacion_isr_total.csv")
if (!file.exists(ARCHIVO_OCDE)) {
  stop("No encuentro ", ARCHIVO_OCDE, ".")
}

ocde <- read.csv(ARCHIVO_OCDE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
ocde <- ocde[!ocde$iso3 %in% c("ALC", "OCDE"), ]
anios_ocde <- sort(unique(ocde$anio))

d_paises <- ocde %>% select(iso3, pais) %>% distinct() %>% arrange(pais)
opciones_paises <- setNames(d_paises$iso3, d_paises$pais)

MEDIDAS <- list(
  total = list(
    etiqueta = "Recaudación tributaria total",
    pib = "recaudacion_total_pib", musd = "recaudacion_total_musd"),
  isr_total = list(
    etiqueta = "ISR total (personas y empresas)",
    pib = "isr_total_pib", musd = "isr_total_musd"),
  isr_personas = list(
    etiqueta = "ISR de personas físicas",
    pib = "isr_personas_pib", musd = "isr_personas_musd"),
  isr_empresas = list(
    etiqueta = "ISR de empresas",
    pib = "isr_empresas_pib", musd = "isr_empresas_musd")
)
OPCIONES_MEDIDA <- setNames(
  names(MEDIDAS),
  vapply(MEDIDAS, function(m) m$etiqueta, character(1))
)

ESCALA_AZUL <- list(
  c(0,   "#74c6c7"), c(1/6, "#93dbe3"), c(2/6, "#4cb4c6"),
  c(3/6, "#2695bb"), c(4/6, "#006e8d"), c(5/6, "#144456"), c(1, "#063746")
)

ISLAS <- data.frame(
  iso3 = c("ATG", "BHS", "BRB", "GRD", "LCA", "TTO"),
  lat  = c(17.06, 24.25, 13.19, 12.12, 13.91, 10.69),
  lon  = c(-61.80, -76.00, -59.54, -61.68, -60.98, -61.22),
  stringsAsFactors = FALSE
)

fmt_usd <- function(x_musd) {
  ifelse(
    is.na(x_musd), "n.d.",
    ifelse(
      x_musd >= 1000,
      paste0("US$ ", formatC(x_musd / 1000, format = "f", digits = 1, big.mark = ","),
             " mil millones"),
      paste0("US$ ", formatC(x_musd, format = "f", digits = 0, big.mark = ","),
             " millones")
    )
  )
}

datos_mapa <- function(medida_id, anio) {
  m <- MEDIDAS[[medida_id]]
  d <- ocde[ocde$anio == anio, ]
  d$valor <- d[[m$pib]]
  d$musd  <- d[[m$musd]]
  d
}

texto_hover <- function(d, medida_id, anio) {
  m <- MEDIDAS[[medida_id]]
  fuente <- if (medida_id == "total") rep("OCDE", nrow(d)) else d$fuente_isr
  ifelse(
    is.na(d$valor),
    paste0("<b>", d$pais, "</b><br>", m$etiqueta, " (", anio, ")<br>Sin dato disponible"),
    paste0("<b>", d$pais, "</b><br>", m$etiqueta, " (", anio, ")<br>",
           sprintf("%.2f%% del PIB", d$valor), "<br>", fmt_usd(d$musd),
           "<br><i>Fuente: ", fuente, "</i>")
  )
}

armar_trazas <- function(d) {
  isl <- merge(d, ISLAS, by = "iso3")
  trazas <- list(
    list(tipo = "poligono", con_dato = TRUE,  datos = d[!is.na(d$valor), ]),
    list(tipo = "poligono", con_dato = FALSE, datos = d[is.na(d$valor), ]),
    list(tipo = "isla",     con_dato = TRUE,  datos = isl[!is.na(isl$valor), ]),
    list(tipo = "isla",     con_dato = FALSE, datos = isl[is.na(isl$valor), ])
  )
  Filter(function(t) nrow(t$datos) > 0, trazas)
}

iso_desde_clic <- function(ev, trazas) {
  if (is.null(ev) || nrow(ev) == 0) return(NULL)
  cand <- NULL
  if ("location" %in% names(ev)) cand <- as.character(ev$location[1])
  if (is.null(cand) || !(cand %in% ocde$iso3)) {
    if (all(c("curveNumber", "pointNumber") %in% names(ev))) {
      k <- ev$curveNumber[1] + 1
      i <- ev$pointNumber[1] + 1
      if (k >= 1 && k <= length(trazas) && i >= 1 && i <= nrow(trazas[[k]]$datos)) {
        cand <- trazas[[k]]$datos$iso3[i]
      }
    }
  }
  if (!is.null(cand) && cand %in% ocde$iso3) cand else NULL
}

anio_inicial_mapa <- max(anios_ocde)

# ---------------------------------------------------------------------------
# Tasas del impuesto sobre la renta de personas físicas (tablas por país)
# ---------------------------------------------------------------------------
ARCHIVO_TASAS <- file.path("data/raw/goyo", "tasas_isr_personas_fisicas.csv")
if (!file.exists(ARCHIVO_TASAS)) {
  stop("No encuentro ", ARCHIVO_TASAS, ".")
}

tasas <- read.csv(ARCHIVO_TASAS, fileEncoding = "latin1", stringsAsFactors = FALSE)

paises_tasas <- unique(tasas[, c("iso3", "pais", "tipo_regimen")])
paises_tasas <- paises_tasas[order(paises_tasas$pais), ]
con_tabla <- paises_tasas$tipo_regimen == "progresiva"
OPCIONES_TASAS <- list(
  "Con tabla de tramos" = setNames(paises_tasas$iso3[con_tabla],  paises_tasas$pais[con_tabla]),
  "Solo tasa máxima"    = setNames(paises_tasas$iso3[!con_tabla], paises_tasas$pais[!con_tabla])
)

fmt_num <- function(x, decimales) {
  ifelse(is.na(x), "—", formatC(x, format = "f", digits = decimales, big.mark = ","))
}
fmt_tasa <- function(x) {
  ifelse(is.na(x), "—",
         paste0(sub("\\.?0+$", "", formatC(x, format = "f", digits = 2)), "%"))
}

tabla_tasas_ui <- function(iso) {
  d <- tasas[tasas$iso3 == iso, ]
  if (nrow(d) == 0) return(NULL)

  if (d$tipo_regimen[1] == "tasa_maxima") {
    return(tagList(
      div(class = "fs-1 fw-bold", fmt_tasa(d$tasa_pct[1])),
      div(class = "text-muted", "Tasa máxima"),
      p(class = "small text-muted mt-2",
        "El documento solo indica la tasa máxima de ", d$pais[1],
        "; no incluye tabla de tramos.")
    ))
  }

  d <- d[order(d$tramo), ]
  con_cuota <- any(!is.na(d$cuota_fija))
  con_ded   <- any(!is.na(d$deduccion))
  u <- d$unidad[1]

  vals <- c(d$limite_inferior, d$limite_superior, d$cuota_fija, d$deduccion)
  dec  <- if (any(abs(vals - round(vals)) > 1e-9, na.rm = TRUE)) 2 else 0

  columnas <- list(
    list(titulo = paste0("Desde (", u, ")"), valores = fmt_num(d$limite_inferior, dec)),
    list(titulo = paste0("Hasta (", u, ")"),
         valores = ifelse(is.na(d$limite_superior), "en adelante",
                          fmt_num(d$limite_superior, dec)))
  )
  if (con_cuota) {
    columnas <- c(columnas, list(
      list(titulo = paste0("Cuota fija (", u, ")"), valores = fmt_num(d$cuota_fija, dec))))
  }
  columnas <- c(columnas, list(
    list(titulo = if (con_cuota) "Tasa sobre el excedente" else "Tasa",
         valores = fmt_tasa(d$tasa_pct))))
  if (con_ded) {
    columnas <- c(columnas, list(
      list(titulo = paste0("Deducción (", u, ")"), valores = fmt_num(d$deduccion, dec))))
  }

  metodo <- if (con_cuota) {
    "Impuesto = cuota fija + (base gravable − límite inferior) × tasa."
  } else if (con_ded) {
    "Impuesto = base gravable × tasa − deducción."
  } else {
    NULL
  }
  notas <- unique(d$nota[!is.na(d$nota) & d$nota != ""])

  tagList(
    tags$table(
      class = "table table-sm table-striped",
      tags$thead(tags$tr(lapply(columnas, function(cl) {
        tags$th(class = "text-end", cl$titulo)
      }))),
      tags$tbody(lapply(seq_len(nrow(d)), function(i) {
        tags$tr(lapply(columnas, function(cl) tags$td(class = "text-end", cl$valores[i])))
      }))
    ),
    if (!is.null(metodo)) p(class = "small text-muted", metodo),
    lapply(notas, function(n) p(class = "small", tags$strong("Nota: "), n))
  )
}

# ---------------------------------------------------------------------------
# Progresividad: tasa marginal vs. tasa promedio según la base gravable
# ---------------------------------------------------------------------------
impuesto_isr <- function(d, base) {
  n     <- nrow(d)
  piso  <- c(0, d$limite_superior[-n])
  techo <- ifelse(is.na(d$limite_superior), Inf, d$limite_superior)
  vapply(base, function(b) {
    sum(d$tasa_pct / 100 * pmax(0, pmin(b, techo) - piso))
  }, numeric(1))
}

curva_progresividad <- function(iso, log_x = TRUE) {
  d <- tasas[tasas$iso3 == iso & tasas$tipo_regimen == "progresiva", ]
  if (nrow(d) == 0) return(NULL)
  d <- d[order(d$tramo), ]
  n <- nrow(d)
  umbral_top <- d$limite_inferior[n]
  xmax <- 2 * umbral_top
  x <- if (log_x) {
    exp(seq(log(d$limite_superior[1] / 10), log(xmax), length.out = 600))
  } else {
    seq(0, xmax, length.out = 601)[-1]
  }
  piso <- c(0, d$limite_superior[-n])
  idx  <- findInterval(x, piso, left.open = TRUE)
  idx[idx < 1] <- 1
  data.frame(
    x        = x,
    marginal = d$tasa_pct[idx],
    promedio = 100 * impuesto_isr(d, x) / x,
    unidad   = d$unidad[1],
    umbral   = umbral_top,
    prom_top = 100 * impuesto_isr(d, umbral_top) / umbral_top,
    marg_top = d$tasa_pct[n]
  )
}
