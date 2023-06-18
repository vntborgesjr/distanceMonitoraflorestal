# Documentacao da funcao plotar_n_obs_validadas_interativo() --------------------
#' Gera um grafico de barras interativo com o numero de observacoes validadas para cada nivel taxonomico
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{...} contendo graficos de barras interativos.
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{plotar_n_obs_validadas_interativo()}
plotar_n_obs_validadas_interativo <- function(
    dados
) {
  #
  grafico_n_sp_validada <- dados |>
    dplyr::mutate(validacao = forcats::fct_reorder(
      validacao,
      n,
      .desc = TRUE
    )) |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = validacao,
      y = n,
    ) +
    ggplot2::geom_col(
      fill = "chartreuse4"
    ) +
    ggplot2::labs(
      title = "Numero de obs. validadas para \ncada nivel taxonomico",
      x = "Nivel taxonomico",
      y = "Contagem"
    ) +
    ggplot2::theme_minimal(14)

  # gerar grafico interativo
  grafico_n_sp_validada <- plotly::ggplotly(
    grafico_n_sp_validada
  )

  return(grafico_n_sp_validada)
}

utils::globalVariables(
  c(
    ""
  )
)
