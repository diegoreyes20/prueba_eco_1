# ------------------------------------------------------------------
# Pestaña: Conclusiones
# ------------------------------------------------------------------
conclusiones_ui <- function() {
  tagList(
    h3("Conclusiones"),

    h4("¿Qué tan progresivo es el impuesto sobre la renta en México?"),
    # --- EDITAR AQUI: conclusion 1 ---
    div(style = "min-height:120px;border:1px dashed #c9c9c9;border-radius:8px;padding:14px;margin-bottom:28px",
        em("conclusion 1")),

    h4("¿Cómo se compara internacionalmente?"),
    # --- EDITAR AQUI: conclusion 2 ---
    div(style = "min-height:120px;border:1px dashed #c9c9c9;border-radius:8px;padding:14px",
        em("conclusion 2"))
  )
}
