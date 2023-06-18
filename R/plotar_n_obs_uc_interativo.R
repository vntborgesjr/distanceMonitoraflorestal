# Documentacao da funcao plotar_n_obs_uc_interativo() --------------------
#' Gera um grafico de barras interativo com o numero de observacoes por UC
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
#' @examples \dontrun{plotar_n_obs_uc_interativo()}
# desenha um grafico de barras intereativo com o numero de observacoes por UC
plotar_n_obs_uc_interativo <- function(
    dados
) {
  # desenha o grafico com mais de 1000 observacoes
  fig <-
    plotly::ggplotly(dados |>
                       dplyr::mutate(
                         n_obs = dplyr::case_when(
                           n %in% 1:100 ~ "Ate 100 observacoes",
                           n %in% 101:500 ~ "Entre 101 e 500 observacoes",
                           n %in% 501:1000 ~ "Entre 501 e 1000 observacoes",
                           n > 1000 ~ "Mais de 1000 observacoes"
                         ),
                         nome_uc_abv = forcats::fct_reorder(
                           nome_uc_abv,
                           dplyr::desc(n)
                         ),
                         n_obs = forcats::fct_reorder(
                           n_obs,
                           dplyr::desc(n)
                         )
                       ) |>
                       ggplot2::ggplot() +
                       ggplot2::aes(x = nome_uc_abv,
                                    y = n,
                                    label = nome_uc) +
                       ggplot2::geom_col(fill = "chartreuse4") +
                       ggplot2::labs(x = "Unidades de Conservacao",
                                     y = "Numero de observacoes") +
                       ggplot2::facet_wrap(
                         facets = ggplot2::vars(
                           n_obs
                         ),
                         nrow = 4,
                         scales = "free"
                       ) +
                       ggplot2::theme_minimal(14)
    )

  # retronar os graficos
  return(fig)
}

utils::globalVariables(
  c(
    ""
  )
)
