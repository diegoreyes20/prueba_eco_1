# ------------------------------------------------------------------
# Pestaña: Portada
# ------------------------------------------------------------------
portada_ui <- function() {
  div(
    style = paste0(
      "background-image: url('fondo.jpg'); background-size: cover; ",
      "background-position: center; min-height: 85vh; width: 100%; ",
      "display: flex; align-items: center; justify-content: center;"
    ),
    div(
      style = paste0(
        "background: rgba(255,255,255,0.82); border-radius: 14px; ",
        "padding: 40px 50px; max-width: 760px; text-align: center;"
      ),
      h1(style = "font-weight: 700;",
         "Impuesto sobre la renta en México y comparación internacional"),
      h4(style = "margin-top: 22px; font-style: italic; color: #333;",
         "¿Qué tan progresivo es el impuesto sobre la renta en México y cómo se compara internacionalmente?"),
      p(style = "margin-top: 30px; font-size: 18px;",
        "Gregorio Uribe Cruz y Diego Reyes Rodríguez"),
      p(style = "font-size: 14px; color: #5f6670;",
        "Economía Pública")
    )
  )
}
