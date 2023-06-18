# Documentacao da funcao plotar_n_obs_sp_uc_interativo() --------------------
#' Gera um grafico de barras interativo com o numero de observacoes por especie e UC
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
#' @examples \dontrun{plotar_n_obs_sp_uc_interativo()}
plotar_n_obs_sp_uc_interativo <- function(
    dados
) {
  # desenha o grafico com mais de 1000 observacoes
  mais_mil_obs <- dados |>
    dplyr::filter(n %in% 1001:2497) |>
    dplyr::mutate(
      nome_sp_abv = forcats::fct_reorder(
        nome_sp_abv,
        dplyr::desc(n)
      )
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = nome_sp_abv,
                 y = n,
                 fill = nome_uc_abv) +
    ggplot2::geom_col(
      #fill = "chartreuse4",
      position = "dodge"
    ) +
    #ggplot2::geom_label() +
    ggplot2::labs(
      x = "Nome das especies",
      y = "Numero de observacoes"
    ) +
    ggplot2::theme_minimal(14)

  # transforma em grafico interativo
  mais_mil_obs <- plotly::ggplotly(mais_mil_obs)

  # desenha o grafico com 501 a 1000 observacoes
  quinhentos_mil_obs <- dados  |>
    dplyr::filter(n %in% 501:1000) |>
    dplyr::mutate(
      nome_sp_abv = forcats::fct_reorder(
        nome_sp_abv,
        dplyr::desc(n)
      )
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = nome_sp_abv,
                 y = n,
                 fill = nome_uc_abv) +
    ggplot2::geom_col(
      #fill = "chartreuse4",
      position = "dodge"
    ) +
    #ggplot2::geom_label() +
    ggplot2::labs(
      x = "Nome das especies",
      y = "Numero de observacoes"
    ) +
    ggplot2::theme_minimal(14)

  # transforma em grafico interativo
  quinhentos_mil_obs <- plotly::ggplotly(quinhentos_mil_obs)

  # desenha o grafico com 101 a 500 observacoes
  cem_quintas_obs <- dados  |>
    dplyr::filter(n %in% 101:500) |>
    dplyr::mutate(
      nome_sp_abv = forcats::fct_reorder(
        nome_sp_abv,
        dplyr::desc(n)
      )
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = nome_sp_abv,
                 y = n,
                 fill = nome_uc_abv) +
    ggplot2::geom_col(
      #fill = "chartreuse4",
      position = "dodge"
    ) +
    #ggplot2::geom_label() +
    ggplot2::labs(
      x = "Nome das especies",
      y = "Numero de observacoes"
    ) +
    ggplot2::theme_minimal(14)

  # tansforma em grafico interativo
  cem_quintas_obs <- plotly::ggplotly(cem_quintas_obs)

  # desenha o grafico com menos de 100 observacoes
  uma_cem_obs <- dados  |>
    dplyr::filter(n < 100) |>
    dplyr::mutate(
      nome_sp_abv = forcats::fct_reorder(
        nome_sp_abv,
        dplyr::desc(n)
      )
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = nome_sp_abv,
                 y = n,
                 fill = nome_uc_abv) +
    ggplot2::geom_col(
      #fill = "chartreuse4",
      position = "dodge"
    ) +
    #ggplot2::geom_label() +
    ggplot2::labs(
      x = "Nome das especies",
      y = "Numero de observacoes"
    ) +
    ggplot2::theme_minimal(14)

  # tansforma em grafico interativo
  uma_cem_obs <- plotly::ggplotly(uma_cem_obs)

  # organizar os graficos
  fig <-  plotly::subplot(
    mais_mil_obs,
    quinhentos_mil_obs,
    cem_quintas_obs,
    uma_cem_obs,
    nrows = 4,
    titleX = FALSE,
    titleY = TRUE,
    shareX = FALSE,
    shareY = TRUE
  )

  # retronar os graficos
  return(fig)
}

utils::globalVariables(
  c(
    "n_obs"
  )
)
