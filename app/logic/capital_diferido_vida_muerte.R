#' Calcular la prima pura única de un capital diferido de vida
#'
#' @param x Edad de la persona (un valor numérico)
#' @param n Plazo en años (un valor numérico)
#' @param C Capital asegurado en dólares (un valor numérico)
#' @param i Tasa de interés efectiva anual (un valor numérico)
#' @param ley_supervivencia
#'
#' @return prima pura única
#' @export
prima_pura_unica <- function(x, n, C, i, ley_supervivencia) {

  # Función de descuento
  v <- 1 / (1 + i)
browser()
  # Calcular la prima pura única para cada ley de supervivencia
  P1 <- C * sum(p1(x) * p1(x + 1)^(1:(n - 1)) * v^(1:n))

  return(list(P1 = P1, P2 = P2, P3 = P3))
}

edad <- 30 # Edad de la persona
plazo <- 10 # Plazo en años
capital_asegurado <- 1000 # Capital asegurado en dólares
tasa_interes <- 0.04 # Tasa de interés efectiva anual
prima_pura_unica(edad, plazo, capital_asegurado, tasa_interes, ley_supervivencia = p1 <- function(x) (1 - x / 100))
