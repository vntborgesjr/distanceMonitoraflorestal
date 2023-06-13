# Documentacao da funcao testar_bondade_ajuste() --------------------------
#' Teste de bondade de ajuste e plot de quantis
#'
#' @description
#' A função `testar_bondade_ajuste()` realiza o teste de bondade de ajsute dos
#' modelos de função de detecção. Para dados contínuos de distância, são
#' utilizados os testes de Kolmogorov-Smrinov a Cramer-von Mises. Quando os
#' dados de distância estão categorizados o teste $X{^2}$ pode ser usado.
#'
#'  @usage testar_bondade_ajuste(
#'            dados,
#'            plot = TRUE,
#'            chisq = FALSE,
#'            nboot = 100,
#'            ks = FALSE,
#'            nc = NULL,
#'            intervalos_distancia = NULL
#'         )
#'
#' @param dados recebe uma lista nomeada gerada pela função
#'  [gerar_lista_modelos_selecionados()] contendo os modelos de função de
#'  detecção
#' @param plot lógico, se `TRUE` retorna o plot de quantis teóricos x  empíricos
#' para cada modelo
#' @param chisq lógico, se `TRUE` retorna a estatística $X{^2}$ mesmo para
#' modelos que usam dados contínuos de distância. Para dados de distância
#' categorizados a estatística $X{^2}$ é automaticamente utilizada
#' @param nboot número de réplicas usados para calcular os p-valores no teste
#' de ajuste de bondade de Kolmogorov_Smirnov
#' @param ks lógico, se `TRUE` realiza o teste de Kolmogorov-Smirnov (como
#' envolve o uso de bootstrap pode levar mais tempo). Por configuração,
#' `ks = FALSE` realizando o teste de Cramer-von Mises
#' @param nc número de classes de distâncias igualmente espaçadas para o teste
#' de quiquadrado quando `chisq = TRUE`
#' @param intervalos_distancia vetor de pontos de corte utilizado para
#' categorização das distâncias
#'
#' @return retorna uma lista contendo objetos da classe `dsmodel`
#' @export
#'
#' @examples
# gerar dados filtrados para a uma espécie e uma UC e transformar para o
#' # formato para a análise no pacote Distance
#' dados_dasy_croc_tap_arap <- filtrar_dados(
#'   dados = monitora_aves_masto_florestal,
#'   nome_ucs = "resex_tapajos_arapiuns",
#'   nome_sps = "dasyprocta_croconota",
#'   validacao_obs = "especie"
#' ) |>
#'   transformar_dados_formato_Distance()
#'
#' # ajustar modelos com funções chave diferentes
#' # modelo Half-normal
#' modelo_hn <- dados_dasy_croc_tap_arap |>
#'   ajustar_modelos_Distance(
#'     funcao_chave = "hn",
#'     truncamento = 15
#'   )
#'
#' # modelo Hazard-rate
#' modelo_hr <- dados_dasy_croc_tap_arap |>
#'   ajustar_modelos_Distance(
#'     funcao_chave = "hr",
#'     truncamento = 15
#'   )
#'
#' # modelo Uniform
#' modelo_unif <- dados_dasy_croc_tap_arap |>
#'   ajustar_modelos_Distance(
#'     funcao_chave = "unif",
#'     truncamento = 15
#'   )
#'
#' # gerar a tabela de seleção com o resumo comparativo dos modelos
#' modelos_selecionados <- selecionar_funcao_deteccao_termo_ajuste(
#'   modelo_hn$`Sem termo`,
#'   modelo_hn$Cosseno,
#'   modelo_hn$`Hermite polinomial`,
#'   modelo_hr$`Sem termo`,
#'   modelo_hr$Cosseno,
#'   modelo_hr$`Polinomial simples`,
#'   modelo_unif$Cosseno,
#'   modelo_unif$`Polinomial simples`
#' )
#'
#' # gerar uma lista com os modelos selecionados ordenados do melhor para o pior modelo
#' lista_modelos_selecionados <- gerar_lista_modelos_selecionados(
#' modelo_hr$`Sem termo`,
#' modelo_hn$Cosseno,
#' modelo_unif$Cosseno,
#' modelo_unif$`Polinomial simples`,
#' modelo_hn$`Sem termo`,
#' nome_modelos_selecionados = selecao_funcao_deteccao_termo_ajuste
#' )
#'
#' # teste de bondade de ajuste dos modelos e Q-Q plots
#' bondade_ajuste <- testar_bondade_ajuste(
#'   lista_modelos_selecionados,
#'   plot = TRUE,
#'   chisq = TRUE,
#'   intervalos_distancia =  NULL
#' )
#'
#' bondade_ajuste
testar_bondade_ajuste <- function(
    dados,
    plot = FALSE,
    chisq = FALSE,
    nboot = 100,
    ks = FALSE,
    nc = NULL,
    intervalos_distancia = NULL
) {

  # controlar para a presença de dados binados
  if (is.null(intervalos_distancia)) {

    # teste de bondade de ajuste com dados de distância contínuos
    # gera uma lista com os resultados dos testes de bondade de ajuste para dados
    # não categorizados
    bondade_ajuste <- dados |>
      purrr::map(
        \(x) Distance::gof_ds(
          x,
          plot = plot,
          chisq = chisq,
          nboot = nboot,
          ks = ks,
          nc = nc,
          breaks = intervalos_distancia
        )
      ) |>
      # gerar o data.frame com os resultados dos testes de bondade de ajuste
      purrr::map(
        \(x) data.frame(x$dsgof$CvM)
      ) |>
      purrr::list_rbind() |>
      dplyr::mutate(Modelo = names(dados)) |>
      dplyr::relocate(Modelo, .before = W)

  } else{

    # teste de bondade de ajuste com dados de distância categorizados
    # gera uma lista com os resultados dos testes de bondade de ajuste para dados
    # categorizados
    bondade_ajuste <- dados |>
      purrr::map(
        \(x) Distance::gof_ds(
          x,
          plot = plot,
          chisq = chisq,
          nboot = nboot,
          ks = ks,
          nc = nc,
          breaks = intervalos_distancia
        )
      )
    # atribuir nomes aos itens da lista com os resultados dos testes de bondade de ajuste
    names(bondade_ajuste) <- names(dados)

  }
  # retornar o data.frame com o resultado dos testes de bondade de ajuste
  return(bondade_ajuste)
}

utils::globalVariables(
  c(
    "intervalos_distancia",
    "bondade_ajuste",
    "dados",
    "x",
    "plot",
    "chisq",
    "nboot",
    "ks",
    "nc",
    "breaks",
    "intervalos_distancia",
    "W"
  )
)
