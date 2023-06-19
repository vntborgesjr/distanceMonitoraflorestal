# Documentação do função gerar_intervalos_confianca_bootstrap() -----------
#' Estimativa de incerteza para modelos *distance sampling* por *bootstrap*
#'
#' @description
#' Conduz um *bootstrap* para modelos de *distance sampling*.
#'
#' @param dados recebe um conjunto de dados filtrados pela função
#' [filtrar_dados()] e transformados pela função
#'  [transformar_dados_formato_Distance()].
#' @param funcao_chave recebe uma dentre três funções utilizadas para modelar a
#' detectabilidade da espécie: "hn" - Half-normal; "hr" - Hazard-rate; e
#' "unif" - Uniform.
#' @param termos_ajuste recebe um dentre n termos de ajuste: `"cos"` - cosseno;
#' `"herm"` - Hermet polynomial; e `"poly"` - Polinomial simples. Quando `NULL`
#' diferentes modelos são ajustados para função chave escolhida utilizando
#' diferentes termos de ajuste. Veja Details.
#' @param truncamento recebe uma lista contendo as distâncias de truncagem
#' de forma numérica (ex. 20), ou como porcentagem (como um caracter, ex. 25%).
#' Também pode ser alimentada no formato de lista, com os elementos `left` e
#' `right` (ex. list(left = 1, right =20)) se a truncagem a esquerda for
#' necessária. Por configuração, a distância máxima é utilizada como valor de
#' truncagem a direita. Quando os dados estão categorizados, a truncagem a
#' direita é o valor final da última coluna. O valor de truncagem a esquerda
#' é zero, por configuração.
#' @param formula a fórmula para o parâmetro de escala. Para análise CDS manter
#' a configuração original ~ 1.
#' @param funcao_sumario defini se o bootstrap retornará intervalos de confiança
#' para estimativa de abunância ou de densidade.
#' @param nboot número de bootstraps a serem conduzidos.
#' @param nucleos número de núcleos do CPU usados para realizar a estimativa do
#' intervalo de confiança.
#'
#' @details
#' Quando `termo_ajuste = NULL` modelos com diferente termos de ajuste serão
#' ajustados aos dados, a depender da função chave utilizada. Para
#' `funcao_chave = "hn"`, são utilizados os termos de ajuste `NULL` (sem termos),
#'  `"cos"` e `"herm"`; para `funcao_chave = "hr"`, são utilizados os termos de
#'  ajuste `NULL` (sem termos), `"cos"` e `"poly"`; e para `funcao_chave  = "unif"`,
#'  são utilizados so termos de ajuste `"cos"` e `"poly"`.
#'
#' @return Retornar um vetor contendo o intervalo de confiança e um gáfico de
#' histograma contendo a distribuição de frequências da estimativa de
#' abundância ou de densidade e seus respectivos intervalos de confiança.
#' @export
#'
#' @examples \dontrun{
#' gerar_intervalos_confianca_bootstrap()
#' }
gerar_intervalos_confianca_bootstrap <- function(
    dados,
    funcao_chave = c("hn", "hr", "unif"),
    termos_ajuste = NULL,
    truncamento = NULL,
    formula = ~ 1,
    funcao_sumario = c("abundancia", "densidade"),
    nboot = 100,
    nucleos = 1
) {

  # definir a função chave
  funcao_chave <- match.arg(funcao_chave)

  # ajustando o modelo desejado
  modelo <- Distance::ds(
    data = dados,
    key = funcao_chave,
    truncation = truncamento,
    adjustment = termos_ajuste,
    formula = formula
  )

  # definir função sumário
  funcao_sumario <- match.arg(funcao_sumario)

  # controlar o tipo de função sumária
  if (funcao_sumario == "abundancia") {

    # conduzir bootstrap para retornar abundância
    est.boot <- Distance::bootdht(
      model = modelo,
      flatfile = dados,
      summary_fun = Distance::bootdht_Nhat_summarize,
      nboot = nboot,
      cores = nucleos
    )

  } else {

    # conduzir bootstrap para retornar densidade
    est.boot <- Distance::bootdht(
      model = modelo,
      flatfile = dados,
      summary_fun = Distance::bootdht_Dhat_summarize,
      nboot = nboot,
      cores = nucleos
    )

  }

  # nível de significância
  alpha <- 0.05
  bootci <- stats::quantile(
    est.boot$Dhat,
    probs = c(alpha/2, 1-alpha/2),
    na.rm = TRUE
  )

  # histograma com a distribuição das estimativas de bootstrap
  hist_boots1 <- est.boot |>
    ggplot2::ggplot() +
    ggplot2::aes(x = Dhat) +
    ggplot2::geom_histogram(
      fill = "chartreuse4",
      color = "black"
    ) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = bootci[1]),
      color = "red",
      linetype = "dashed"
    ) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = bootci[2]),
      color = "red",
      linetype = "dashed"
    ) +
    ggplot2::labs(
      title = "Distribuicao das estimativas de bootstrap",
      x = "Densidade estimada",
      y = "Frequencia"
    ) +
    ggplot2::theme_minimal()

  # gerar grafico interativo
  hist_interativo <- plotly::ggplotly(hist_boots1)
  hist_interativo

  # retorna o intervalo de confiança
  return(list(
    intervalo_confianca = bootci,
    histograma = hist_interativo
  ))

}

utils::globalVariables(
  c(
    "funcao_chave",
    "funcao_sumario",
    "alpha",
    "bootci",
    "hist_interativo",
    "hist_boots1",
    "Dhat",
    "est.boot"
  )
)
