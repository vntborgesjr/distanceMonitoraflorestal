# Documentaçãoo da função plotar_distribuicao_distancia_estatico() --------------------
#' Gráficos interativos das distâncias perpendiculares
#'
#' @description
#' A funcao \code{plotar_distribuicao_distancia_estatico()} gera três
#' gráficos, descrevendo a variação das distâncias
#' perpendiculares observadas.
#'
#' @param dados receba uma `tibble` gerada a partir da função
#' [transformar_dados_formato_Distance()] ou que contenha uma coluna chamada
#' `distance`
#' @param largura_caixa recebe um valor que estabelece a largura das colunas
#' do histograma
#' @param cor define a cor do contorno das barras
#' @param preenchimento define a cor de preenchimento das barras
#' @param legenda define o conteúdo da legenda da figura. "auto" atribui as
#' o vetor c("a", "b", "c")
#' @param familia_fonte define o tipo de fonte
#' @param tamanho_fonte_titulo_eixos define o tamanho da fonte dos títulos
#' dos eixos
#' @param tamanho_fonte_valores_eixos define o tamanho da fonte dos valores dos
#' eixos
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
#' @examples \dontrun{plotar_distribuicao_distancia_estatico()}
plotar_distribuicao_distancia_estatico <- function(
    dados,
    largura_caixa = 1,
    cor = "black",
    preenchimento = "chartreuse4",
    legenda = "auto",
    familia_fonte = "serif",
    tamanho_fonte_titulo_eixos = 12,
    tamanho_fonte_valores_eixos = 11
) {
  # desenha o grafico de caixa
  box <- dados |>
    ggplot2::ggplot() +
    ggplot2::aes(x = distance) +
    ggplot2::geom_boxplot(
      col = cor,
      fill = preenchimento
    ) +
    ggplot2::scale_y_continuous(
      breaks = NULL,
      limits = c(-.8, .8)) +
    ggplot2::labs(
      x = "",
      y = " \n \n",
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(
        family = familia_fonte,
        size = tamanho_fonte_titulo_eixos
      ),
      axis.text = ggplot2::element_text(
        family = familia_fonte,
        size = tamanho_fonte_valores_eixos
      )
    )

  # desenha o grafico de pontos
  pontos <- dados |>
    dplyr::arrange(dplyr::desc(distance)) |>
    ggplot2::ggplot() +
    ggplot2::aes(y = dplyr::desc(seq_along(distance)),
                 x = distance) +
    ggplot2::geom_point(
      color = preenchimento
    )  +
    ggplot2::labs(x = "Distância",
                  y = " \n \n") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(
        family = familia_fonte,
        size = tamanho_fonte_titulo_eixos
      ),
      axis.text = ggplot2::element_text(
        family = familia_fonte,
        size = tamanho_fonte_valores_eixos
      )
    )

  # dessenha o hitograma
  hist <- dados |>
    dplyr::arrange(dplyr::desc(distance)) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = distance) +
    ggplot2::geom_histogram(binwidth = largura_caixa,
                            fill = preenchimento,
                            col = "white",
                            center = 1.25) +
    ggplot2::labs(x = "",
                  y = "Frequência") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(
        family = familia_fonte,
        size = tamanho_fonte_titulo_eixos
      ),
      axis.text = ggplot2::element_text(
        family = familia_fonte,
        size = tamanho_fonte_valores_eixos
      )
    )

  # organizar os graficos
  fig <-  ggpubr::ggarrange(
    hist,
    box,
    pontos,
    nrow = 3,
    labels = legenda
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
