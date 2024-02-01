# Documentacao da funcao plotar_resultados_Distance() --------
#' Plota os resultados de abundância ou densidade estimada
#'
#' @param dados recebe um objeto do tipo `data.frame` gerado pela função
#' [gerar_resultados_Distance()]
#' @param tipo_de_resultado ...
#' @param intervalo_confianca ...
#'
#' @description
#' Descrever o que cada opção retorna
#'
#' @return Retorna ...
#' @export
#'
#' @examples \dontrun{
#'
#' }
plotar_resultados_Distance <- function(
    dados,
    tipo_de_resultado = c("abundância", "densidade"),
    intervalo_confianca = FALSE
) {

  # definir o tipo de resultado fornecido pelo usuário
  tipo_de_resultado <- match.arg(tipo_de_resultado)

  if (tipo_de_resultado == "abundância") {
    if (intervalo_confianca == FALSE) {
      grafico <- dados |>
        dplyr::mutate(Ano = as.integer(Ano)) |>
        dplyr::group_by(
          Modelo,
          Ano
        ) |>
        dplyr::summarise(`Abundância estimada` = sum(`Abundância estimada`)) |>
        ggplot2::ggplot() +
        ggplot2::aes(
          x = Ano,
          y = `Abundância estimada`
        ) +
        ggplot2::geom_line() +
        ggplot2::scale_x_continuous(breaks = 2014:2021) +
        # ggplot2::facet_wrap(facets = ggplot2::vars(Modelo)) +
        ggplot2::theme_bw()

    } else {

      print("Não há estimativa de intervalo de confiança para a abundância estimada")
      # grafico <- dados |>
      #   dplyr::mutate(Ano = as.integer(Ano)) |>
      #   dplyr::group_by(
      #     Modelo,
      #     Ano
      #   ) |>
      #   dplyr::summarise(`Abundância estimada` = sum(`Abundância estimada`)) |>
      #   ggplot2::ggplot() +
      #   ggplot2::aes(
      #     x = Ano,
      #     y = `Abundância estimada`
      #   ) +
      #   ggplot2::geom_line() +
      #   ggplot2::scale_x_continuous(breaks = 2014:2021) +
      #   # ggplot2::facet_wrap(facets = ggplot2::vars(Modelo)) +
      #   ggplot2::theme_bw() +
      #   ggplot2::geom_line(
      #     ggplot2::aes(
      #       x = Ano,
      #       y = `Abundância estimada` - `Intervalo de confiança inferior`
      #     ),
      #     linetype = 3
      #   )  +
      #   ggplot2::geom_line(
      #     ggplot2::aes(
      #       x = Ano,
      #       y = `Abundância estimada` + `Intervalo de confiança superior`
      #     ),
      #     linetype = 3
      #   )

    }
  } else {
    if (intervalo_confianca == FALSE) {

      grafico <- dados |>
        dplyr::filter(Rotulo != "Total") |>
        dplyr::mutate(Ano = as.integer(Rotulo)) |>
        dplyr::group_by(
          Modelo,
          Ano
        ) |>
        dplyr::summarise(`Estimativa de densidade (ind/ha)` = sum(`Estimativa de densidade (ind/ha)`)) |>
        ggplot2::ggplot() +
        ggplot2::aes(
          x = Ano,
          y = `Estimativa de densidade (ind/ha)`
        ) +
        ggplot2::geom_line() +
        ggplot2::scale_x_continuous(breaks = 2014:2021) +
        # ggplot2::facet_wrap(facets = ggplot2::vars(Modelo)) +
        ggplot2::theme_bw()

    } else {

      grafico <- dados |>
        dplyr::filter(Rotulo != "Total") |>
        dplyr::mutate(Ano = as.integer(Rotulo)) |>
        ggplot2::ggplot() +
        ggplot2::aes(
          x = Ano,
          y = `Estimativa de densidade (ind/ha)`
        ) +
        ggplot2::geom_line() +
        ggplot2::scale_x_continuous(breaks = 2014:2021) +
        # ggplot2::facet_wrap(facets = ggplot2::vars(Modelo)) +
        ggplot2::theme_bw() +
        ggplot2::geom_line(
          ggplot2::aes(
            x = Ano,
            y = `Estimativa de densidade (ind/ha)` - `Intervalo de confiança inferior`
          ),
          linetype = 3
        )  +
        ggplot2::geom_line(
          ggplot2::aes(
            x = Ano,
            y = `Estimativa de densidade (ind/ha)` + `Intervalo de confiança superior`
          ),
          linetype = 3
        )

    }
  }

  return(grafico)

}


utils::globalVariables(
  c(
    "tipo_de_resultado",
    "dados",
    "grafico"
  )
)
