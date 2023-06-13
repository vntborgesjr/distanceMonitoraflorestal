# Documentaçãoo da função plotar_distribuicao_distancia_interativo() --------------------
#' Gráficos interativos das distâncias perpendiculares
#'
#' @description
#' A funcao \code{plotar_distribuicao_distancia_interativo()} gera três
#' gráficos interativos, descrevendo a variação das distâncias
#' perpendiculares observadas.
#'
#' @param dados receba uma `tibble` gerada a partir da função
#' [transformar_dados_formato_Distance()] ou que contenha uma coluna chamada
#' `distance`
#' @param largura_caixa recebe um valor que estabelece a largura das colunas
#' do histograma
#'
#' @return Retorna um objeto do tipo \code{...} contendo um gráfico de pontos
#'  (Cleveland dotplot), de caixa (boxplot) e um histograma, da distribuição
#'   de distâncias perpendiculares.
#'
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{plotar_distribuicao_distancia_interativo()}
plotar_distribuicao_distancia_interativo <- function(
    dados,
    largura_caixa = 1
) {
  # desenha o grafico de caixa
  box <- dados |>
    ggplot2::ggplot() +
    ggplot2::aes(x = distance) +
    ggplot2::geom_boxplot(
      col = "black",
      fill = "chartreuse4"
    ) +
    ggplot2::scale_y_continuous(
      breaks = NULL,
      limits = c(-.8, .8)) +
    ggplot2::labs(
      x = "",
      y = " \n \n",
    ) +
    ggplot2::theme_minimal()

  # transforma em grafico interativo
  box <- plotly::ggplotly(box)

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

  # transforma em grafico interativo
  pontos <- plotly::ggplotly(pontos)

  # dessenha o hitograma
  hist <- dados |>
    dplyr::arrange(dplyr::desc(distance)) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = distance) +
    ggplot2::geom_histogram(binwidth = largura_caixa,
                            fill = "chartreuse4",
                            col = "white",
                            center = 1.25) +
    ggplot2::labs(x = "",
                  y = "Frequencia") +
    ggplot2::theme_minimal()

  # tansforma em grafico interativo
  hist <- plotly::ggplotly(hist)

  # organizar os graficos
  fig <-  plotly::subplot(
    hist,
    box,
    pontos,
    nrows = 3,
    titleX = TRUE,
    titleY = TRUE
  )

  # retronar os graficos
  return(fig)
}

utils::globalVariables(
  c(
    "dados",
    "largura_da_caixa",
    "box",
    "distance",
    "pontos",
    "hist",
    "fig"
  )
)
