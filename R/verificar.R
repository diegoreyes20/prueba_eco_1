# ------------------------------------------------------------------
# Verificacion rapida de los datos. Ejecutar desde la carpeta del
# proyecto: source("R/verificar.R")
# ------------------------------------------------------------------
source("global.R")

stopifnot(nrow(tax_rev) == length(ANIOS_TAXREV) * nrow(RUBROS) * 2)
stopifnot(all(table(isr$anio) == 11))
stopifnot(nrow(deciles) == length(ANIOS_DECILES) * 10)

cat("Anios composicion:", ANIOS_TAXREV, "\n")
cat("Anios ISR:", range(ANIOS_ISR), "\n")
cat("Anios deciles (ENIGH):", ANIOS_DECILES, "\n")
cat("Ejemplo: ISR 2026 para $15,000/mes =", round(isr_pagar(2026, 15000), 2),
    "| tasa marginal =", pct(tasa_marginal(2026, 15000)),
    "| tasa promedio =", pct(tasa_promedio_de(2026, 15000)), "\n")
cat("TODO OK\n")
