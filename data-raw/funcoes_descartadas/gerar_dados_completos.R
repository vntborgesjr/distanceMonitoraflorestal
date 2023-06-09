# Documentacao da funcao gerar_dados_completos() ----------------------
#' Seleciona, trasforma e renomea as colunas e observacoes nos dados brutos
#'
#' @description
#' A funcao \code{gerar_dados_completos()} recebe uma \code{tibble} gerada a partir da funcao \code{carregar_dados_brutos_xlsx()}, selecionando, transformando e renomeando suas colunas e observacoes.
#'
#' @usage gerar_dados_completos(dados)
#'
#' @param dados recebe uma \code{tibble} contendo os dados brutos.
#'
#' @details
#' A funcao \code{gerar_dados_completos()} seleciona, transforma e renomeia as colunas e observacoes dos dados brutos do Projeto Monitora Componente Florestal. A funcao gera novas colunas contendo o numero de vezes que cada estacao amostral foi visitada, a categoria da UC,o nome abreviado das UC's, o nome abreviado das especies, o esforco amostral total empregado em cada estacao amostral, o numero de observadores e o tempo total de cada censo. Quando ausentes, as distancias de trilhas percorridas sao imputadas.
#'
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo uma selecao de colunas transformadas e renomeadas a partir dos dados brutos do Projeto Monitora Componente Florestal.

#' @export

#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto

#' @examples \dontrun{gerar_dados_completos(dados = dados_brutos)}
gerar_dados_completos <- function(dados) {

  # gera tabela de consulta com os nomes antigos das colunas
  nomes_antigos <- names(dados)

  # gerar novos nomes em portugues para as colunas
  nomes_novos_pt <- c(
    codigo_uc = nomes_antigos[1],
    nome_uc = nomes_antigos[2],
    numero_ea = nomes_antigos[3],
    nome_ea = nomes_antigos[4],
    data_amostragem = nomes_antigos[6],
    nomes_antigos[7],
    estacao = nomes_antigos[8],
    esforco_dia = nomes_antigos[5],
    nomes_antigos[16],
    nomes_antigos[17],
    nomes_antigos[18],
    nomes_antigos[19],
    nome_sp = nomes_antigos[22],
    validacao = nomes_antigos[23],
    distancia = nomes_antigos[27],
    tamanho_grupo = nomes_antigos[25],
    observadores = nomes_antigos[14],
    horario_inicio = nomes_antigos[9],
    horario_termino = nomes_antigos[10]
  )



  dados_completos <- dados |>
    # selecionar as colunas de interesse
    dplyr::select(tidyselect::all_of(nomes_novos_pt)) |>
    dplyr::mutate(
      # gera uma nova coluna contendo a categoria das UCs
      categoria_uc = stringi::stri_extract_first_words(
        nome_uc
      ),
      # gera uma nova coluna com o nome das UCs abreviados
      nome_uc_abv = forcats::lvls_revalue(
        nome_uc,
        new_levels = c(
          "ETM", "EM", "EN", "ESGT", "FJ", "PCV", "PA", "PSBoc", "PSBod", "PSC",
          "PSM", "PSC", "PSD", "PSP", "PSO", "PPN", "PCO", "PI", "PJau", "PJur",
          "PMR", "PS", "PV", "PCA", "PMT", "RG", "RJ", "RTap", "RU", "RG",
          "RTrom", "RAT", "RBA", "RCI", "RCM", "RRC", "RROP", "RIA", "RRA", "RTA"
        )
      ),
      # gera nova coluna com o nome das sp abreviados
      nome_sp_abv = stringr::str_split(# divide o genero e epiteto em diferentes listas
        nome_sp, " "
      ) |>
        purrr::map(# retem apenas as 4 primeiras letras dos itens de cada lista
          \(string) stringr::str_sub(
            string, 1, 4
          )
        ) |>
        purrr::map(# aidiciona "." ao final dos itens de cada lista
          \(string) stringr::str_c(
            string, "."
          )
        ) |>
        purrr::map(# adiciona " " ao final dos itens de cada lista
          \(string) stringr::str_flatten(
            string, " "
          )
        ) |>
        purrr::list_c(), # concatena os itens das duas listas
      # padroniza a codificacao da coluna de validacao
      validacao = forcats::fct_recode(
        validacao,
        "Especie" = "E",
        "Especie" = "e",
        "Familia" = "F",
        "Genero" = "G",
        "Genero" = "g",
        "Ordem" = "O"
      ),
      # atribui o tipo fator a todas as colunas tipo caracter
      dplyr::across(
        tidyselect::where(is.character),
        as.factor
      ),
      # gera uma nova coluna com o tempo total do censo
      tempo_censo = horario_inicio - horario_termino,
      # padroniza os separadores entre os nomes dos observadores
      # substitu "e" por ","
      novo = stringr::str_replace_all(
        observadores,
        " e ",
        ", "
      ),
      # substitui "E" por ","
      novo = stringr::str_replace_all(
        novo,
        " E ",
        ", "
      ),
      # substitui "/" por ","
      novo = stringr::str_replace_all(
        novo,
        "/",
        ", "
      ),
      # substitui ";" por ","
      novo = stringr::str_replace_all(
        novo,
        ";",
        ", "
      ),
      novo = stringr::str_replace_all(# substitui "a" por ","
        novo,
        " a ",
        ", "
      )
    ) |>
    # gerar colunas individualizadas para cada observador
    tidyr::separate_wider_delim(
      novo,
      ",",
      names = c("obs1", "obs2", "obs3", "obs4", "obs5", "obs6"),
      too_few = "align_start"
    ) |>
    # gerar coluna com o numero total de observadores
    dplyr::mutate(
      # substituir o nome dos observadores por 1 e NAs por 0
      obs1 = ifelse(!is.na(obs1), 1, 0),
      obs2 = ifelse(!is.na(obs2), 1, 0),
      obs3 = ifelse(!is.na(obs3), 1, 0),
      obs4 = ifelse(!is.na(obs4), 1, 0),
      obs5 = ifelse(!is.na(obs5), 1, 0),
      obs6 = ifelse(!is.na(obs6), 1, 0),
      # somar colunas para gerar o numero total de observadores
      numero_observadores = obs1 + obs2 + obs3 + obs4 + obs5 + obs6
    ) |>
    # completar observacoes repetidas que estao ausentes
    # agrupar por nome da estacao amostral e data dee amostragem
    dplyr::group_by(
      nome_ea,
      data_amostragem
    ) |>
    # aninhar dados pelas colunas agrupadas
    tidyr::nest() |>
    dplyr::mutate(
      # gerar nova coluna de esforco sem observacoes ausentes
      esforco_dia2 = purrr::map(
        data,
        \(.x) rep(.x$esforco_dia[!is.na(.x$esforco_dia)][1])
      ),
      # gerar nova coluna de tempo de censo sem observacoes ausentes
      tempo_censo2 = purrr::map(
        data,
        \(.x) rep(.x$tempo_censo[!is.na(.x$tempo_censo)][1])
      ),
    ) |>
    # desaninhar os dados
    tidyr::unnest(
      c(data, esforco_dia2, tempo_censo2)
    ) |>
    # desagrupar os dados
    dplyr::ungroup() |>
    # selecionar as colunas de interesse
    dplyr::select(
      tidyselect::ends_with(
        c("uc", "ea")
      ),
      data_amostragem,
      estacao,
      ano,
      esforco_dia = esforco_dia2,
      nome_sp:numero_observadores,
      tempo_censo = tempo_censo2,
      -tempo_censo,
      -esforco_dia,
      -tidyselect::starts_with("obs"),
      -tidyselect::starts_with("horario")
    ) |>
    # reposicionar colunas
    dplyr::relocate(
      categoria_uc,
      .before = nome_uc
    ) |>
    dplyr::relocate(
      nome_uc_abv,
      .after = nome_uc
    ) |>
    dplyr::relocate(
      nome_sp_abv,
      .after = nome_sp
    ) |>
    # preencher observacoes ausentes
    tidyr::fill(esforco_dia)

  # calculo do esforco amostral total
  dados_completos <- dados_completos |>
    # seleciona combinacoes unicas de nome_ea e data_amostragem
    dplyr::distinct(
      nome_ea,
      data_amostragem
    ) |>
    # conta o numero de vezes que uma ea foi amostrada
    dplyr::count(
      nome_ea,
      name = "n_visitas_repetidas"
    ) |>
    # reune os dados gerados com os dados_completos a partir da coluna nome_ea
    dplyr::left_join(
      y = dados_completos,
      by = dplyr::join_by(nome_ea),
    ) |>
    # gera a distancia total percorrida em cada ea
    dplyr::mutate(esforco_total = esforco_dia*n_visitas_repetidas) |>
    # reposiciona as colunas
    dplyr::relocate(
      esforco_total,
      .after = esforco_dia
    ) |>
    # agrupa a tibble pelas colunas nome_uc e nome_ea
    dplyr::group_by(
      nome_uc,
      nome_ea
    ) |>
    # filtra a tibble mantendo apenas as linhas contendo o esforco maximo de um dia
    dplyr::filter(esforco_dia == max(esforco_dia))  |>
    # desagrupa a tibble
    dplyr::ungroup() |>
    # eliminar espacos, letras minusculas e caracteres especiais das observacoes
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.factor),
        \(.x) janitor::make_clean_names(
          .x, allow_dupes = TRUE
        )
      ),
      # atribui o tipo fator a todas as colunas tipo caracter
      dplyr::across(
        tidyselect::where(is.character),
        as.factor
      )
    )

  # retornar o tibble com os dados completos
  return(dados_completos)
}

