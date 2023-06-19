# Documentação da função plotar_distancia_covariavel() --------------------
#' Plota as distâncias perpendiculares contra uma covariável
#'
#' @description
#' Breve descrição da função...
#'
#' @param dados recebe uma `tibble` gerada pela função [filtrar_dados()] ou
#' [transformar_dados_formato_Distance()].
#' @param x recebe a distância ou a covariável.
#' @param y recebe a distância ou a covariável.
#' @param xlab recebe o título do eixo x.
#' @param ylab recebe o título do eixo y.
#'
#' @return Retorna um gráfico de pontos das distâncias perpendiculares
#' observadas contra uma covariável.
#' @export
#'
#' @examples \dontrun{
#' plotar_distancia_covariavel()
#' }
plotar_distancia_covariavel <- function(
    dados,
    x,
    y,
    xlab,
    ylab
) {

  # gera gráfico de distancia x covariavel
  fig <- dados |>
    ggplot2::ggplot() +
    ggplot2::aes(
      y = {{ y }},
      x = {{ x }}
    ) +
    ggplot2::geom_point(
      color = "chartreuse4"
    ) +
    ggplot2::labs(
      x = xlab,
      y = ylab
    )
    ggplot2::theme_minimal()

  # retorna o gráfico interativo
  plotly::ggplotly(fig)

}

utils::globalVariables(
  c(
    "fig",
    "dados",
    "x",
    "y"
  )
)
