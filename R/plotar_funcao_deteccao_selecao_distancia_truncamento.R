# Documentacao da funcao plotar_funcao_deteccao_selecao_distancia_truncamento() --------
#' Title
#'
#' @param dados breve descrição...
#'
#' @return retorna...
#' @export
#'
#' @examples \dontrun{
#' plotar_funcao_deteccao_modelos_selecionados()
#' }
plotar_funcao_deteccao_selecao_distancia_truncamento <- function(dados) {
  dados$modelos |>
    purrr::map(
      \(.x) plot(
        .x,
        xlab = "Distancia (m)",
        ylab = "Probabilidade de detecacao",
        pl.col = "chartreuse4"
      )
    )
}

