# Documentacao da funcao plotar_distribuicao_distancia_estatico() --------------------
#' Gera tres graficos estaticos descrevendo a variacao das distancias perpendiculares observadas
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{...} contendo um grafico de caixa, um grafico de pontos e um histograma da distribuicao de distancias perpendiculares.
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{plotar_distribuicao_distancia_estatico()}
plotar_distribuicao_distancia_estatico <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados_transformados_dist_r.rds"
      )
    )
) {
  # desenha o grafico de caixa
  box <- dados |>
    ggplot2::ggplot() +
    ggplot2::aes(x = distance) +
    ggplot2::geom_boxplot(col = "black",
                          fill = "chartreuse4") +
    ggplot2::scale_y_continuous(
      breaks = NULL,
      limits = c(-.8, .8)) +
    ggplot2::labs(
      x = "",
      y = " \n \n",
    ) +
    ggplot2::theme_minimal()

  # desenha o grafico de pontos
  pontos <- dados |>
    dplyr::arrange(dplyr::desc(distance)) |>
    ggplot2::ggplot() +
    ggplot2::aes(y = dplyr::desc(seq_along(distance)),
                 x = distance) +
    ggplot2::geom_point(
      color = "chartreuse4"
    )  +
    ggplot2::labs(x = "Distancia",
                  y = " \n \n") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank())

  # dessenha o hitograma
  hist <- dados |>
    dplyr::arrange(dplyr::desc(distance)) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = distance) +
    ggplot2::geom_histogram(binwidth = 2.5,
                            fill = "chartreuse4",
                            col = "white",
                            center = 1.25) +
    ggplot2::labs(x = "",
                  y = "Frequencia") +
    ggplot2::theme_minimal()

  # organizar os graficos
  fig <- ggpubr::ggarrange(
    hist,
    box,
    pontos,
    nrow = 3
  )

  # retronar os graficos
  return(fig)
}


# Documentacao da funcao plotar_n_obs_sp_estatico() --------------------
#' Gera graficos de barras estaticos com o numero de observacoes por especie
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{...} contendo graficos de barras estaticos.
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{plotar_n_obs_sp_estatico()}
plotar_n_obs_sp_estatico <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_sp.rds"
      )
    )
) {
  fig <- dados |>
    dplyr::mutate(
      n_obs = dplyr::case_when(
        n > 1000 ~ "Mais de 1000 observacoes",
        n %in% 501:1000 ~ "Entre 501 e 1000 observacoes",
        n %in% 101:500 ~ "Entre 101 e 500 observacoes",
        n %in% 1:100 ~ "Ate 100 observacoes"
      ),
      nome_sp_abv = forcats::fct_reorder(
        nome_sp_abv,
        n
      ),
      n_obs = forcats::fct_reorder(
        n_obs,
        dplyr::desc(n)
      )
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(y = nome_sp_abv,
                 x = n,
                 label = n) +
    ggplot2::geom_col(
      fill = "chartreuse4",
      #width = .9
    ) +
    ggplot2::geom_label(
      size = 8
    ) +
    ggplot2::labs(y = "Especies",
                  x = "Numero de observacoes") +
    ggplot2::facet_wrap(
      facets = ggplot2::vars(
        n_obs
      ),
      nrow = 4,
      scales = "free"
    ) +
    ggplot2::theme_minimal(14) +
    ggplot2::theme(
      title = ggplot2::element_text(size = 40),
      axis.text = ggplot2::element_text(size = 30),
      # Remove labels from the vertical axis
      #axis.text.y = element_blank(),
      strip.text = ggplot2::element_text(size = 40),
    )


  # retronar os graficos
  return(fig)
}

# Documentacao da funcao plotar_n_obs_uc_estatico() --------------------
#' Gera um grafico de barras estatico com o numero de observacoes por UC
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
#' @examples \dontrun{plotar_n_obs_uc_estatico()}
plotar_n_obs_uc_estatico <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_uc.rds"
      )
    )
) {
  # desenha os graficos com mais de 1000, de 501 a 1000, de 101 a 500 e ate 100 observacoes
  fig <- dados |>
    dplyr::mutate(
      n_obs = dplyr::case_when(
        n > 1000 ~ "Mais de 1000 observacoes",
        n %in% 501:1000 ~ "Entre 501 e 1000 observacoes",
        n %in% 101:500 ~ "Entre 101 e 500 observacoes",
        n %in% 1:100 ~ "Ate 100 observacoes"
      ),
      nome_uc = forcats::fct_reorder(
        nome_uc,
        n
      ),
      n_obs = forcats::fct_reorder(
        n_obs,
        dplyr::desc(n)
      )
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(y = nome_uc,
                 x = n,
                 label = n) +
    ggplot2::geom_col(
      fill = "chartreuse4",
      #width = .9
    ) +
    ggplot2::geom_label(
      size = 8
    ) +
    ggplot2::geom_text(
      ggplot2::aes(0, y = nome_uc, label = nome_uc),
      hjust = 0,
      nudge_x = 0.3,
      colour = "black",
      family = "Econ Sans Cnd",
      size = 7
    ) +
    ggplot2::labs(y = "Unidades de Conservacao",
                  x = "Numero de observacoes") +
    ggplot2::facet_wrap(
      facets = ggplot2::vars(
        n_obs
      ),
      nrow = 4,
      scales = "free"
    ) +
    ggplot2::theme_minimal(14) +
    ggplot2::theme(
      title = ggplot2::element_text(size = 40),
      axis.text = ggplot2::element_text(size = 30),
      # Remove labels from the vertical axis
      axis.text.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 40),
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    )

  # retronar os graficos
  return(fig)
}

# Documentacao da funcao plotar_n_obs_validadas_estatico() --------------------
#' Gera um grafico de barras estatico com o numero de observacoes validadas para cada nivel taxonomico
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
#' @examples \dontrun{plotar_n_obs_validadas_estatico()}
plotar_n_obs_validadas_estatico <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/tabela_n_obs_validadas.rds"
      )
    )
) {
  #
  grafico_n_sp_validada_estatico <- dados  |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = validacao,
      y = n,
      label = n
    ) +
    ggplot2::geom_col(
      fill = "chartreuse4"
    ) +
    ggplot2::geom_label(
      ggplot2::aes(
        label = n
      )
    ) +
    ggplot2::labs(
      title = "Numero de obs. validadas para \ncada nivel taxonomico",
      x = "Nivel taxonomico",
      y = "Contagem"
    ) +
    ggplot2::theme_minimal(14)

  # retornar grafico estatico
  return(grafico_n_sp_validada_estatico)
}

utils::globalVariables(
  c(
    ""
  )
)
