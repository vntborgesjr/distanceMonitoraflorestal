# Documentacao da funcao ajuste_modelo_distace_hn() -----------------------
#' Ajusta a funcao de deteccao Half-Normal com diferentes termos de ajuste
#'
#' @param dados
#' @param lista_termos_ajuste
#' @param truncamento
#'
#' @return
#' @export
#'
#' @examples
ajuste_modelos_distance_hn <- function(
    dados,
    lista_termos_ajuste = list(
      `Sem termo` = NULL,
      Cosseno = "cos",
      `Hermite polinomial` = "herm"
    ),
    truncamento = NULL,
    formula = ~ 1
) {
  # ajustando a função de detecção
  # Key function - Half-normal
  modelos_ajustados <- purrr::map(
    lista_termos_ajuste,
    \(.x) Distance::ds(
      data = dados,
      truncation = truncamento,
      formula = formula,
      key = "hn",
      adjustment = .x
    )
  )

  # retorna o output dos modelos
  return(modelos_ajustados)

}

# Documentacao da funcao ajuste_modelos_distance_hr() ----------------------
#' Title
#'
#' @param dados
#' @param lista_termos_ajuste
#' @param truncamento
#'
#' @return
#' @export
#'
#' @examples
ajuste_modelos_distance_hr <- function(
    dados,
    lista_termos_ajuste = list(
      `Sem termo` = NULL,
      Cosseno = "cos",
      `Polinomial simples` = "poly"
    ),
    truncamento = NULL,
    formula = ~ 1
) {
  # ajustando a função de detecção
  # Key function - Hazard-rate
  modelos_ajustados <- purrr::map(
    lista_termos_ajuste,
    \(.x) Distance::ds(
      data = dados,
      truncation = truncamento,
      formula = formula,
      key = "hr",
      adjustment = .x
    )
  )

  # retorna o output dos modelos
  return(modelos_ajustados)

}

# Documentacao da funcao ajuste_modelos_distance_unif() -------------------
#' Title
#'
#' @param dados
#' @param lista_termos_ajuste
#' @param truncamento
#'
#' @return
#' @export
#'
#' @examples
ajuste_modelos_distance_unif <- function(
    dados,
    lista_termos_ajuste = list(
      #`Sem termo` = NULL,
      Cosseno = "cos",
      `Polinomial simples` = "poly"
    ),
    truncamento = NULL,
    formula = ~ 1
) {
  # ajustando a função de detecção
  # Key function - Uniforme
  modelos_ajustados <- purrr::map(
    lista_termos_ajuste,
    \(.x) Distance::ds(
      data = dados,
      truncation = truncamento,
      formula = formula,
      key = "unif",
      adjustment = .x
    )
  )

  # retorna o output dos modelos
  return(modelos_ajustados)

}

# Documentacao da funcao contar_n_obs_sp_ano() --------------------
#' Gera uma tabela com o numero total de observacoes para cada especie e em cada ano
#'
#' @description
#' A funcao \code{contar_n_obs_sp_ano()} gera uma tabela contendo o numero total de observacoes para cada especie e em cada ano
#'
#' @usage contar_n_obs_sp_ano(dados = monitora_aves_masto_florestal)
#'
#' @param dados recebe a \code{tibble} gerada pelas funcoes \code{carregar_dados_brutos()}, \code{carregar_dados_filtrados()}, ou \code{gerar_dados_selecionados()}. Por configuracao, carrega a base de dados burtos de aves e medios e grandes mamiferos do Pojeto Monitora Componente Florestal
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo tres colunas, ano que contem o ano em que a especie foi amostrada, nome_sp contendo o nome da especie e n contendo o numero de observacoes realizadas para especie em cada ano
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # numero de anos em que uma UC foi amostrada - dados brutos
#' contar_n_obs_sp_ano()
#'
# numero de anos em que uma UC foi amostrada - dados filtrados
#' dados_filtrados <- gerar_dados_filtrados(
#'   nome_ucs = "resex_tapajos_arapiuns"
#' )
#'
#' contar_n_obs_sp_ano(dados_filtrados)
#'
#' # numero de anos em que uma UC foi amostrada - dados selecionados
#' dados_selecionados <- gerar_dados_selecionados()
#'
#' contar_n_obs_sp_ano(dados_selecionados)
contar_n_obs_sp_ano <- function(
    dados = monitora_aves_masto_florestal
) {
  # gerar a tabela com o numero de observacoes por UC em cada ano
  n_obs_sp_ano <- dados  |>
    dplyr::count(
      ano,
      nome_sp,
      nome_sp_abv
    )

  # retorna a tabela com o numero de observacoes por UC
  return(n_obs_sp_ano)
}

utils::globalVariables(
  c(
    "n_obs_sp_ano",
    "dados",
    "ano",
    "nome_sp",
    "nome_sp_abv"
  )
)

# Documentacao da funcao contar_n_obs_sp_uc_ano() --------------------
#' Numero total de observacoes para cada especie, em cada Unidade de Conservacao e em cada ano
#'
#' @description
#' A funcao \code{contar_n_obs_sp_uc_ano()} gera uma tabela contendo o numero total de observacoes para cada especie, em cada Unidade de Conservacao e em cada ano
#'
#' @usage contar_n_obs_sp_uc_ano(dados = monitora_aves_masto_florestal)
#'
#' @param dados recebe a \code{tibble} gerada pelas funcoes \code{carregar_dados_brutos()}, \code{carregar_dados_filtrados()}, ou \code{gerar_dados_selecionados()}. Por configuracao, carrega a base de dados burtos de aves e medios e grandes mamiferos do Pojeto Monitora Componente Florestal
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo seis colunas, \code{ano} que contem o ano em que a especie foi amostrada, \code{nome_uc} que contem o nome da Unidade de Conservacao, \code{nome_uc_abv} que contem o nome da Unidadade de Conservacao abreviado, \code{nome_sp} contendo o nome da especie, \code{nome_sp_abv} que contem o nome da especie abreviado e \code{n} contendo o numero de observacoes realizadas para especie em cada ano
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # numero de observacoes para cada especie, em cada UC e por ano - dados brutos
#' contar_n_obs_sp_uc_ano()
#'
#  # numero de observacoes para cada especie, em cada UC e por ano - dados filtrados
#' dados_filtrados <- gerar_dados_filtrados(
#'   nome_ucs = "resex_tapajos_arapiuns"
#' )
#'
#' contar_n_obs_sp_uc_ano(dados_filtrados)
#'
#' # numero de observacoes para cada especie, em cada UC e por ano - dados selecionados
#' dados_selecionados <- gerar_dados_selecionados()
#'
#' contar_n_obs_sp_uc_ano(dados_selecionados)
contar_n_obs_sp_uc_ano <- function(
    dados = monitora_aves_masto_florestal
) {
  # gera o numero de observacoes por especie, por Uc e por ano
  n_obs_sp_uc_ano <- dados |>
    dplyr::count(
      nome_uc,
      nome_uc_abv,
      nome_sp,
      nome_sp_abv,
      ano
    )

  # retorna o numero de observacoes por especie, por Uc e por ano
  return(n_obs_sp_uc_ano)
}

utils::globalVariables(
  c(
    "n_obs_sp_uc_ano",
    "dados",
    "ano",
    "nome_sp",
    "nome_sp_abv",
    "nome_uc",
    "nome_uc_abv"
  )
)

# Documentacao da funcao contar_n_obs_sp_uc_estacao_ano() --------------------
#' Numero total de observacoes para cada especie, em cada Unidade de Conservacao, estacao e em cada ano
#'
#' @description
#' A funcao \code{contar_n_obs_sp_uc_estacao_ano()} gera uma tabela contendo o numero total de observacoes para cada especie, em cada Unidade de Conservacao, em cada estacao e em cada ano
#'
#' @usage contar_n_obs_sp_uc_estacao_ano(dados = monitora_aves_masto_florestal)
#'
#' @param dados recebe a \code{tibble} gerada pelas funcoes [carregar_dados_brutos()], [gerar_dados_filtrados()], ou [gerar_dados_selecionados()]. Por configuracao, carrega a base de dados burtos de aves e medios e grandes mamiferos do Pojeto Monitora Componente Florestal
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo seis colunas, \code{ano} que contem o ano em que a especie foi amostrada, \code{nome_uc} que contem o nome da Unidade de Conservacao, \code{nome_uc_abv} que contem o nome da Unidadade de Conservacao abreviado, \code{nome_sp} contendo o nome da especie, \code{nome_sp_abv} que contem o nome da especie abreviado e \code{n} contendo o numero de observacoes realizadas para especie em cada ano
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # numero de observacoes para cada especie, em cada UC, estacao e ano - dados brutos
#' contar_n_obs_sp_uc_estacao_ano()
#'
#' # numero de observacoes para cada especie, em cada UC, estacao e ano - dados filtrados
#' dados_filtrados <- gerar_dados_filtrados(
#'   nome_ucs = "resex_tapajos_arapiuns"
#' )
#'
#' contar_n_obs_sp_uc_estacao_ano(dados_filtrados)
#'
#' # numero de observacoes para cada especie, em cada UC, estacao e ano - dados selecionados
#' dados_selecionados <- gerar_dados_selecionados()
#'
#' contar_n_obs_sp_uc_estacao_ano(dados_selecionados)
contar_n_obs_sp_uc_estacao_ano <- function(
    dados = monitora_aves_masto_florestal
) {

  # gera o numero de observacoes por especie, por Uc, por estacao e por ano
  n_obs_sp_uc_estacao_ano <- dados |>
    dplyr::count(
      nome_uc,
      nome_uc_abv,
      nome_sp,
      nome_sp_abv,
      estacao,
      ano
    )

  # retorna o numero de observacoes por especie, por Uc, por estacao e por ano
  return(n_obs_sp_uc_estacao_ano)

}

utils::globalVariables(
  c(
    "n_obs_sp_uc_estacao_ano",
    "dados",
    "ano",
    "estacao",
    "nome_sp",
    "nome_sp_abv",
    "nome_uc",
    "nome_uc_abv"
  )
)

# Documentacao da funcao contar_n_obs_sp_uc() --------------------
#' Numero total de observacoes para cada especie e em cada Unidade de Conservacao
#'
#' @description
#' A funcao \code{contar_n_obs_sp_uc()} gera uma tabela contendo o numero total de observacoes para cada especie e em cada Unidade de Conservacao
#'
#' @usage contar_n_obs_sp_uc(dados = monitora_aves_masto_florestal)
#'
#' @param dados recebe a \code{tibble} gerada pelas funcoes [carregar_dados_brutos()], [gerar_dados_filtrados()], ou [gerar_dados_selecionados()]. Por configuracao, carrega a base de dados burtos de aves e medios e grandes mamiferos do Pojeto Monitora Componente Florestal
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo seis colunas, \code{ano} que contem o ano em que a especie foi amostrada, \code{nome_uc} que contem o nome da Unidade de Conservacao, \code{nome_uc_abv} que contem o nome da Unidadade de Conservacao abreviado, \code{nome_sp} contendo o nome da especie, \code{nome_sp_abv} que contem o nome da especie abreviado e \code{n} contendo o numero de observacoes realizadas para especie em cada ano
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # numero de observacoes para cada especie e em cada UC - dados brutos
#' contar_n_obs_sp_uc()
#'
#' # numero de observacoes para cada especie e em cada UC - dados filtrados
#' dados_filtrados <- gerar_dados_filtrados(
#'   nome_ucs = "resex_tapajos_arapiuns"
#' )
#'
#' contar_n_obs_sp_uc(dados_filtrados)
#'
#' # numero de observacoes para cada especie e em cada UC - dados selecionados
#' dados_selecionados <- gerar_dados_selecionados()
#'
#' contar_n_obs_sp_uc(dados_selecionados)
contar_n_obs_sp_uc <- function(
    dados = monitora_aves_masto_florestal
) {

  # gera o numero de observacoes por especie
  n_obs_sp_uc <- dados |>
    dplyr::count(
      nome_uc,
      nome_uc_abv,
      nome_sp,
      nome_sp_abv
    )

  # retorna o numero de observacoes por especie
  return(n_obs_sp_uc)

}

utils::globalVariables(
  c(
    "n_obs_sp_uc",
    "dados",
    "nome_sp",
    "nome_sp_abv",
    "nome_uc",
    "nome_uc_abv"
  )
)

# Documentacao da funcao contar_n_obs_sp() --------------------
#' Numero total de observacoes para cada especie
#'
#' @description
#' A funcao \code{contar_n_obs_sp()} gera uma tabela contendo o numero total de observacoes para cada especie
#'
#' @usage contar_n_obs_sp(dados = monitora_aves_masto_florestal)
#'
#' @param dados recebe a \code{tibble} gerada pelas funcoes [carregar_dados_brutos()], [gerar_dados_filtrados()], ou [gerar_dados_selecionados()]. Por configuracao, carrega a base de dados burtos de aves e medios e grandes mamiferos do Pojeto Monitora Componente Florestal
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo três colunas, \code{nome_sp} contendo o nome da especie, \code{nome_sp_abv} que contem o nome da especie abreviado e \code{n} contendo o numero de observacoes realizadas para cada especie
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # numero de observacoes para cada especie - dados brutos
#' contar_n_obs_sp()
#'
#' # numero de observacoes para cada especie - dados filtrados
#' dados_filtrados <- gerar_dados_filtrados(
#'   nome_ucs = "resex_tapajos_arapiuns"
#' )
#'
#' contar_n_obs_sp(dados_filtrados)
#'
#' # numero de observacoes para cada especie - dados selecionados
#' dados_selecionados <- gerar_dados_selecionados()
#'
#' contar_n_obs_sp(dados_selecionados)
contar_n_obs_sp <- function(
    dados = monitora_aves_masto_florestal
) {

  # gera o numero de observacoes por especie
  n_obs_sp <- dados |>
    dplyr::count(
      nome_sp,
      nome_sp_abv
    )

  return(n_obs_sp)

}

utils::globalVariables(
  c(
    "n_obs_sp",
    "dados",
    "nome_sp",
    "nome_sp_abv"
  )
)

# Documentacao da funcao contar_n_obs_uc_ano() --------------------
#' Numero total de observacoes em cada Unidade de Conservacao por ano
#'
#' @description
#' A funcao \code{contar_n_obs_uc_ano()} gera uma tabela contendo o numero total de observacoes para cada Unidade de Conservacao em cada ano
#'
#' @usage contar_n_obs_uc_ano(dados = monitora_aves_masto_florestal)
#'
#' @param dados recebe a \code{tibble} gerada pelas funcoes [carregar_dados_brutos()], [gerar_dados_filtrados()], ou [gerar_dados_selecionados()]. Por configuracao, carrega a base de dados burtos de aves e medios e grandes mamiferos do Pojeto Monitora Componente Florestal
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo quatro colunas, \code{nome_uc} contendo o nome da Unidade de Conservacao, \code{nome_uc_abv} que contem o nome abreviado da Unidade de Conservacao, \code{ano} que contem o ano da amostragem e \code{n} contendo o numero de observacoes realizadas para cada Unidade de Conservacao
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # numero de observacoes para cada UC - dados brutos
#' contar_n_obs_uc_ano()
#'
#' # numero de observacoes para cada UC - dados filtrados
#' dados_filtrados <- gerar_dados_filtrados(
#'   nome_ucs = "resex_tapajos_arapiuns"
#' )
#'
#' contar_n_obs_uc_ano(dados_filtrados)
#'
#' # numero de observacoes para cada UC - dados selecionados
#' dados_selecionados <- gerar_dados_selecionados()
#'
#' contar_n_obs_uc_ano(dados_selecionados)
contar_n_obs_uc_ano <- function(
    dados = monitora_aves_masto_florestal
) {

  # gerar a tabela com o numero de observacoes por UC em cada ano
  n_obs_uc_ano <- dados  |>
    dplyr::count(
      nome_uc,
      nome_uc_abv,
      ano
    )

  # retorna a tabela com o numero de observacoes por UC por ano
  return(n_obs_uc_ano)

}

utils::globalVariables(
  c(
    "n_obs_uc_ano",
    "dados",
    "nome_uc",
    "nome_uc_abv",
    "ano"
  )
)

# Documentacao da funcao contar_n_obs_uc() --------------------
#' Numero total de observacoes em cada Unidade de Consesrvacao
#'
#' @description
#' A funcao \code{contar_n_obs_uc()} gera uma tabela contendo o numero total de observacoes para cada Unidade de Conservacao
#'
#' @usage contar_n_obs_uc(dados = monitora_aves_masto_florestal)
#'
#' @param dados recebe a \code{tibble} gerada pelas funcoes [carregar_dados_brutos()], [gerar_dados_filtrados()], ou [gerar_dados_selecionados()]. Por configuracao, carrega a base de dados burtos de aves e medios e grandes mamiferos do Pojeto Monitora Componente Florestal
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo três colunas, \code{nome_uc} contendo o nome da Unidade de Conservacao, \code{nome_uc_abv} que contem o nome abreviado da Unidade de Conservacao e \code{n} contendo o numero de observacoes realizadas para cada Unidade de Conservacao
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # numero de observacoes para cada UC - dados brutos
#' contar_n_obs_uc()
#'
#' # numero de observacoes para cada UC - dados filtrados
#' dados_filtrados <- gerar_dados_filtrados(
#'   nome_ucs = "resex_tapajos_arapiuns"
#' )
#'
#' contar_n_obs_uc(dados_filtrados)
#'
#' # numero de observacoes para cada UC - dados selecionados
#' dados_selecionados <- gerar_dados_selecionados()
#'
#' contar_n_obs_uc(dados_selecionados)
contar_n_obs_uc <- function(
    dados = monitora_aves_masto_florestal
) {

  # gerar a tabela com o numero de observacoes por UC
  n_obs_uc <- dados  |>
    dplyr::count(
      nome_uc,
      nome_uc_abv
    )

  # retorna a tabela com o numero de observacoes por UC
  return(n_obs_uc)

}

utils::globalVariables(
  c(
    "n_obs_uc",
    "dados",
    "nome_uc",
    "nome_uc_abv"
  )
)

# Documentacao da funcao contar_n_obs_validadas() --------------------
#' Numero total de observacoes validadas por nivel taxonomico
#'
#' @description
#' A funcao \code{contar_n_obs_validadas()} gera uma tabela ou um vetor contendo o numero total de observacoes validadas para cada nivel taxonomico
#'
#' @usage contar_n_obs_validadas(
#'          dados = monitora_aves_masto_florestal,
#'          retorna_tabela = TRUE
#'        )
#'
#' @param dados recebe a \code{tibble} gerada pelas funcoes [carregar_dados_brutos()], [gerar_dados_filtrados()], ou [gerar_dados_selecionados()]. Por configuracao, carrega a base de dados burtos de aves e medios e grandes mamiferos do Pojeto Monitora Componente Florestal
#' @param retorna_tabela logico, indica se o objeto retornado e uma uma \code{tibble} ou um \code{vetor}
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo duas colunas, \code{validacao} contendo os os niveis taxonomicos de validacao e \code{n} contendo o numero de observacoes validadas para cada nivel taxonomico
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # observacoes para cada nivel taxonomico - dados brutos
#' contar_n_obs_validadas()
#'
#' # observacoes para cada nivel taxonomico - dados brutos
#' contar_n_obs_validadas(retorna_tabela = FALSE)
#'
#' # observacoes para cada nivel taxonomico - dados filtrados
#' dados_filtrados <- gerar_dados_filtrados(
#'   nome_ucs = "resex_tapajos_arapiuns"
#' )
#'
#' contar_n_obs_validadas(dados_filtrados)
#'
#' # observacoes para cada nivel taxonomico - dados selecionados
#' dados_selecionados <- gerar_dados_selecionados()
#'
#' contar_n_obs_validadas(dados_selecionados)
contar_n_obs_validadas <- function(
    dados = monitora_aves_masto_florestal,
    retorna_tabela = TRUE
) {

  # gerar tabela com o numero de observacoes validadas para cada nivel
  # taxonomico
  tabela_n_obs_validadas <- dados |>
    dplyr::count(validacao)

  # extrair os numeros de observacoes validadas
  n_obs_validadas <- tabela_n_obs_validadas |>
    dplyr::pull(var = n)

  # atribuir nome dos niveis taxonomicos aos valores
  names(n_obs_validadas) <- tabela_n_obs_validadas$validacao

  # retornar observacoes validadas...
  if (retorna_tabela == TRUE) {

    # em formato de tabela
    return(tabela_n_obs_validadas)

  } else {

    # retorna em formato de vetor
    return(n_obs_validadas)

  }

}

utils::globalVariables(
  c(
    "tabela_n_obs_validadas",
    "n_obs_validadas",
    "dados",
    "validacao",
    "n"
  )
)

# Documentacao da funcao contar_n_sp() --------------------
#' Numero total de especies
#'
#' @description
#' A funcao \code{contar_n_sp()} gera o numero total de especies contidos na base de dados fornecida
#'
#' @usage contar_n_sp(dados = monitora_aves_masto_florestal)
#'
#' @param dados recebe a \code{tibble} gerada pelas funcoes [carregar_dados_brutos()], [gerar_dados_filtrados()], ou [gerar_dados_selecionados()]. Por configuracao, carrega a base de dados burtos de aves e medios e grandes mamiferos do Pojeto Monitora Componente Florestal
#'
#' @return Retorna um objeto do tipo \code{vetor} contendo o numero total de especies contido na base de dados fornecida
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # numero total de especies - dados brutos
#' contar_n_sp()
#'
#' # numero total de especies para uma UC - dados filtrados
#' dados_filtrados <- gerar_dados_filtrados(
#'   nome_ucs = "resex_tapajos_arapiuns"
#' )
#'
#' contar_n_sp(dados_filtrados)
#'
#' # numero total de especies - dados selecionados
#' dados_selecionados <- gerar_dados_selecionados()
#'
#' contar_n_sp(dados_selecionados)
contar_n_sp <- function(dados = monitora_aves_masto_florestal) {

  # numero total de especies
  n_sp <- dados |>
    dplyr::distinct(nome_sp) |>
    nrow()

  # retorna o numero total de especies
  return(n_sp)

}

utils::globalVariables(
  c(
    "n_sp",
    "dados",
    "nome_sp"
  )
)

# Documentacao da funcao contar_n_uc_ano() --------------------
#' Numero de Unidades de Conservacao amostradas por ano
#'
#' @description
#' A funcao \code{contar_n_uc_ano()} gera o numero total de Unidades de Conservacao amostradas por ano contidos na base de dados fornecida
#'
#' @usage contar_n_uc_ano(dados = monitora_aves_masto_florestal)
#'
#' @param dados recebe a \code{tibble} gerada pelas funcoes [carregar_dados_brutos()], [gerar_dados_filtrados()], ou [gerar_dados_selecionados()]. Por configuracao, carrega a base de dados burtos de aves e medios e grandes mamiferos do Pojeto Monitora Componente Florestal
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo o numero total de Unidades de Conservacao amostradas por ano contido na base de dados fornecida
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # numero total de UCs em cada ano - dados brutos
#' contar_n_uc_ano()
#'
#' # numero total de UCs em cada ano - dados filtrados
#' dados_filtrados <- gerar_dados_filtrados(
#'   nome_ucs = "resex_tapajos_arapiuns"
#' )
#'
#' contar_n_uc_ano(dados_filtrados)
#'
#' # numero total de UCs em cada ano - dados selecionados
#' dados_selecionados <- gerar_dados_selecionados()
#'
#' contar_n_uc_ano(dados_selecionados)
contar_n_uc_ano <- function(dados = monitora_aves_masto_florestal) {

  # gera o numero de Ucs amostradas por ano
  n_uc_ano <- dados |>
    dplyr::count(
      ano,
      nome_uc
    ) |>
    dplyr::group_by(
      ano
    ) |>
    dplyr::count(
      nome_uc
    ) |>
    dplyr::summarise(
      n_ucs = sum(n)
    )

  # retorna uma tabela com o numero de UCs amostradas por ano
  return(n_uc_ano)

}

utils::globalVariables(
  c(
    "n_uc_ano",
    "dados",
    "nome_uc",
    "ano",
    "n_ucs",
    "n"
  )
)

# Documentacao da funcao contar_n_uc() --------------------
#' Numero de Unidades de Conservacao amostradas
#'
#' @description
#' A funcao \code{contar_n_uc()} gera o numero total de Unidades de Conservacao amostradas contidas na base de dados fornecida
#'
#' @usage contar_n_uc(dados = monitora_aves_masto_florestal)
#'
#' @param dados recebe a \code{tibble} gerada pelas funcoes [carregar_dados_brutos()], [gerar_dados_filtrados()], ou [gerar_dados_selecionados()]. Por configuracao, carrega a base de dados burtos de aves e medios e grandes mamiferos do Pojeto Monitora Componente Florestal
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo o numero total de Unidades de Conservacao contido na base de dados fornecida
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # numero total de UCs - dados brutos
#' contar_n_uc()
#'
#' # numero total de UCs - dados filtrados
#' dados_filtrados <- gerar_dados_filtrados(
#'   nome_sps = "dasyprocta_corconota"
#' )
#'
#' contar_n_uc(dados_filtrados)
#'
#' # numero total de UCs - dados selecionados
#' dados_selecionados <- gerar_dados_selecionados()
#'
#' contar_n_uc(dados_selecionados)
contar_n_uc <- function(dados = monitora_aves_masto_florestal) {

  # numero total de Uc's
  n_ucs <- dados |>
    dplyr::distinct(nome_uc) |>
    nrow()

  # retornos n numero total de Uc's
  return(n_ucs)

}

utils::globalVariables(
  c(
    "n_ucs",
    "dados",
    "nome_uc"
  )
)

# Documentacao da funcao gerar_caracteristicas_area_estudo_taxa_encontro() --------
#' Title
#'
#' @param dados
#' @param resultado_selecao_modelos
#'
#' @return
#' @export
#'
#' @examples
gerar_caracteristicas_area_estudo_taxa_encontro <- function(
    dados,
    resultado_selecao_modelos
) {
  # arae total, densidade estimada, erro padrão da densidade estimada,
  # coeficiente de variação da densidade destimada, intervalo de
  # confiança inferior e superior do coeficiente de variação,
  # gruas de liberdade
  caracteristicas_area_estudo_taxa_encontro <- dados |>
    purrr::map(
      \(.x) .x$dht$individuals$summary[1:9]
    ) |>
    purrr::list_rbind() |>
    dplyr::mutate(Modelo = resultado_selecao_modelos$Model) |> # pode ser um argumento da função
    dplyr::relocate(Modelo, .before = Region) |>
    dplyr::rename(
      Regiao = Region,
      `Area coberta` = CoveredArea,
      Esforco = Effort,
      `Taxa de encontro` = ER,
      `ep da Taxa de encontro` = se.ER,
      `cv. da Taxa de encontro` = cv.ER
    )

  # área de estudo, tamanho da área de estudo, trilhas ou estações
  # amostrais, esforço total em cada trilha, abundância estimada em cada
  # estação amostral, número de detecções em cada estação amostral,
  # área total amostrada
  return(caracteristicas_area_estudo_taxa_encontro)
}

# Documentacao da funcao gerar_caracteristicas_densidade() --------
#' Title
#'
#' @param dados
#' @param resultado_selecao_modelos
#'
#' @return
#' @export
#'
#' @examples
gerar_caracteristicas_densidade <- function(
    dados,
    resultado_selecao_modelos
) {
  # área de estudo, tamanho da área de estudo, trilhas ou estações
  # amostrais, esforço total em cada trilha, abundância estimada em cada
  # estação amostral, número de detecções em cada estação amostral,
  # área total amostrada
  caracteristicas_densidade <- dados |>
    purrr::map(
      \(.x) .x$dht$individuals$D
    ) |>
    purrr::list_rbind() |>
    dplyr::select(!c(Label, CoveredArea)) |>
    dplyr::rename(
      Regiao = Region.Label,
      `Estacao amostral` = Sample.Label,
      Esforco = Effort.x,
      `Abundancia estimada` = Nhat,
      `N de deteccoes` = n
    )

  caracteristicas_densidade <- caracteristicas_densidade |>
    dplyr::mutate(Modelo = rep(resultado_selecao_modelos$Model, each = length(unique(caracteristicas_densidade$`Estacao amostral`)))) |> # pode ser um argumento da função
    dplyr::relocate(Modelo, .before = Regiao)

  # área de estudo, tamanho da área de estudo, trilhas ou estações
  # amostrais, esforço total em cada trilha, abundância estimada em cada
  # estação amostral, número de detecções em cada estação amostral,
  # área total amostrada
  return(caracteristicas_densidade)
}

# Documantacao da funcao gerar_caracteristicas_esforco_abundancia_deteccao()  -------------------------------------------------
#' Title
#'
#' @param dados
#'
#' @return
#' @export
#'
#' @examples
gerar_caracteristicas_esforco_abundancia_deteccao <- function(
    dados,
    resultado_selecao_modelos
) {

  # área de estudo, tamanho da área de estudo, trilhas ou estações
  # amostrais, esforço total em cada trilha, abundância estimada em cada
  # estação amostral, número de detecções em cada estação amostral,
  # área total amostrada
  caracteristicas_esforco_abundancia_deteccao <- dados |>
    purrr::map(
      \(.x) .x$dht$individuals$Nhat.by.sample[1:8]
    ) |>
    purrr::list_rbind() |>
    dplyr::select(!c(Label, CoveredArea)) |>
    dplyr::rename(
      Regiao = Region.Label,
      `Estacao amostral` = Sample.Label,
      Esforco = Effort.x,
      `Abundancia estimada` = Nhat,
      `N de deteccoes` = n
    )

  caracteristicas_esforco_abundancia_deteccao <- caracteristicas_esforco_abundancia_deteccao |>
    dplyr::mutate(Modelo = rep(resultado_selecao_modelos$Model, each = length(unique(caracteristicas_densidade$`Estacao amostral`)))) |> # pode ser um argumento da função
    dplyr::relocate(Modelo, .before = Regiao)

  # área de estudo, tamanho da área de estudo, trilhas ou estações
  # amostrais, esforço total em cada trilha, abundância estimada em cada
  # estação amostral, número de detecções em cada estação amostral,
  # área total amostrada
  return(caracteristicas_esforco_abundancia_deteccao)

}

# Documentacao da funcao gerar_tabdin_dados_brutos() --------------------
#' Gera uma tabela dinamica dos dados brutos
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#' @param n_linhas Descricao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_dados_brutos()}
gerar_tabdin_dados_brutos <- function(
    dados = readxl::read_excel(
      path = paste0(
        here::here(),
        "/data-raw/monitora_masto_aves_2023_04_04.xlsx"
      ),
      sheet = "dados brutos"
    ),
    n_linhas = 1:1000
) {
  # gerar tabela dinamica dos dados brutos
  dados_brutos <- dados |>
    dplyr::slice(n_linhas) |>
    DT::datatable(filter = list(position = "top"))

  # retornar a tabela dinamica dos dados brutos
  return(dados_brutos)
}

# Documentacao da funcao gerar_tabdin_dados_completos() --------------------
#' Gera uma tabela dinamica dos dados completos
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#' @param n_linhas Descricao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_dados_completos()}
gerar_tabdin_dados_completos <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_completos.rds"
      )
    ),
    n_linhas = 1:4500
) {
  # gerar tabela dinamica dos dados completos
  dados_completos <- dados |>
    dplyr::slice(n_linhas) |>
    DT::datatable(filter = list(position = "top"))

  # retornar a tabela dinamica dos dados completos
  return(dados_completos)
}

# Documentacao da funcao gerar_tabdin_dados_selecionados_distanceR_cov() --------------------
#' Gera uma tabela dinamica dos dados selecionados no formato distance do R com covariaveis
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#' @param n_linhas Descricao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_dados_selecionados_distanceR_cov()}
gerar_tabdin_dados_selecionados_distanceR_cov <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados_transformados_dist_r_cov.rds"
      )
    ),
    n_linhas = 1:4500
) {
  # gerar tabela dinamica dos dados
  dados_transformados_dist_r_cov <- dados |>
    dplyr::slice(
      n_linhas
    ) |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica dos dados_transformados_dist_r_selecionado
  return(dados_transformados_dist_r_cov)
}

# Documentacao da funcao gerar_tabdin_dados_selecionados_distanceR() --------------------
#' Gera uma tabela dinamica dos dados selecionados no formato distance do R sem covariaveis
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#' @param n_linhas Descricao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_dados_selecionados_distanceR()}
gerar_tabdin_dados_selecionados_distanceR <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados_transformados_dist_r.rds"
      )
    ),
    n_linhas = 1:4500
) {
  # gerar tabela dinamica dos dados selecionados no formato distance do R sem covariaveis
  dados_transformados_dist_r_completo <- dados |>
    dplyr::slice(
      n_linhas
    ) |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica dos dados_transformados_dist_r_completo
  return(dados_transformados_dist_r_completo)
}

# Documentacao da funcao gerar_tabdin_dados_filtrados() --------------------
#' Gera uma tabela dinamica dos dados selecionados e filtrados por UC e especie
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#' @param nome_uc Descricao
#' @param nome_sp Descricao
#' @param n_linhas Descricao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_dados_filtrados()}
gerar_tabdin_dados_filtrados <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    ),
    nome_uc = "Resex Tapajos-Arapiuns",
    nome_sp = "Dasyprocta croconota",
    n_linhas = 1:4500
) {
  # gerar tabela dinamica dos dados filtrados por UC e especie
  dados_filtrado <- dados |>
    dplyr::filter(
      nome_uc == nome_uc,
      nome_sp == nome_sp
    ) |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica dos dados filtrados por UC e especie
  return(dados_filtrado)
}

# Documentacao da funcao gerar_tabdin_dados_selecionados() --------------------
#' Gera uma tabela dinamica dos dados selecionados
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#' @param n_linhas Descricao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_dados_selecionados()}
gerar_tabdin_dados_selecionados <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    ),
    n_linhas = 1:4500
) {
  # gerar tabela dinamica dos dados selecionados
  tabdin_dados_selecionados <- dados |>
    dplyr::slice(
      n_linhas
    ) |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica dos dados selecionados
  return(tabdin_dados_selecionados)
}

# Documentacao da funcao gerar_tabdin_n_ano_uc() --------------------
#' Gera uma tabela dinamica do numero de anos em que cada UC foi amostrada
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_n_ano_uc()}
gerar_tabdin_n_ano_uc <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_ano_uc.rds"
      )
    )
) {
  # gerar tabela dinamica dos dados do numero de anos em que cada UC foi amostrada
  tabdin_n_ano_uc <- dados |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica do numero de anos em que cada UC foi amostrada
  return(tabdin_n_ano_uc)
}

# Documentacao da funcao gerar_tabdin_n_obs_sp_ano() --------------------
#' Gera uma tabela dinamica do numero de observacoes por especie em cada ano
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_n_obs_sp_ano()}
gerar_tabdin_n_obs_sp_ano <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_sp_ano.rds"
      )
    )
) {
  # gerar tabela dinamica dos dados do numero de gerar_tabdin_n_obs_sp_ano
  tabdin_n_obs_sp_ano <- dados |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica do numero de gerar_tabdin_n_obs_sp_ano
  return(tabdin_n_obs_sp_ano)
}

# Documentacao da funcao gerar_tabdin_n_obs_sp_uc_ano() --------------------
#' Gera uma tabela dinamica do numero de observacoes por especie, em cada UC e em cada ano
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_n_obs_sp_uc_ano()}
gerar_tabdin_n_obs_sp_uc_ano <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_sp_uc_ano.rds"
      )
    )
) {
  # gerar tabela dinamica dos dados do numero de observacoes por UC, por especies, por estacao e por ano
  tabdin_n_obs_sp_uc_ano <- dados |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica do numero de observacoes por UC, por especies, por estacao e por ano
  return(tabdin_n_obs_sp_uc_ano)
}

# Documentacao da funcao gerar_tabdin_n_obs_sp_uc_estacao_ano() --------------------
#' Gera uma tabela dinamica do numero de observacoes por especie, em cada UC, estacao e ano
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_n_obs_sp_uc_estacao_ano()}
gerar_tabdin_n_obs_sp_uc_estacao_ano <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_sp_uc_estacao_ano.rds"
      )
    )
) {
  # gerar tabela dinamica dos dados do numero de observacoes por UC, por especies, por estacao e por ano
  tabdin_n_obs_sp_uc_estacao_ano <- dados |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica do numero de observacoes por UC, por especies, por estacao e por ano
  return(tabdin_n_obs_sp_uc_estacao_ano)
}

# Documentacao da funcao gerar_tabdin_n_obs_sp_uc() --------------------
#' Gera uma tabela dinamica do numero de observacoes por especie, em cada UC
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_n_obs_sp_uc()}
gerar_tabdin_n_obs_sp_uc <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_sp_uc.rds"
      )
    )
) {
  # gerar tabela dinamica dos dados do numero de observacoes por sp por UC
  tabdin_n_obs_sp_uc <- dados |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica do numero de observacoes por sp por UC
  return(tabdin_n_obs_sp_uc)
}

# Documentacao da funcao gerar_tabdin_n_obs_sp() --------------------
#' Gera uma tabela dinamica do numero de observacoes por especie
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_n_obs_sp()}
gerar_tabdin_n_obs_sp <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_sp.rds"
      )
    )
) {
  # gerar tabela dinamica dos dados do numero de observacoes por sp
  tabdin_n_obs_sp <- dados |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica do numero de observacoes por sp
  return(tabdin_n_obs_sp)
}

# Documentacao da funcao gerar_tabdin_n_obs_uc_ano() --------------------
#' Gera uma tabela dinamica do numero de observacoes por UC em cada ano
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_n_obs_uc_ano()}
gerar_tabdin_n_obs_uc_ano <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_uc_ano.rds"
      )
    )
) {
  # gerar tabela dinamica dos dados do numero de anos em que cada UC foi amostrada
  tabdin_n_obs_uc_ano <- dados |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica do numero de anos em que cada UC foi amostrada
  return(tabdin_n_obs_uc_ano)
}

# Documentacao da funcao gerar_tabdin_n_obs_uc() --------------------
#' Gera uma tabela dinamica do numero de observacoes por UC
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_n_obs_uc()}
gerar_tabdin_n_obs_uc <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_uc.rds"
      )
    )
) {
  # gerar tabela dinamica dos dados do numero de observacoes por uc
  tabdin_n_obs_uc <- dados |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica do numero de observacoes por uc
  return(tabdin_n_obs_uc)
}

# Documentacao da funcao gerar_tabdin_n_uc_ano() --------------------
#' Gera uma tabela dinamica do numero de UC's amostrada em cada ano
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_n_uc_ano()}
gerar_tabdin_n_uc_ano <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_uc_ano.rds"
      )
    )
) {
  # gerar tabela dinamica dos dados do numero de observacoes por sp
  tabdin_n_uc_ano <- dados |>
    DT::datatable(
      filter = list(
        position = "top"
      )
    )

  # retornar a tabela dinamica do numero de observacoes por sp
  return(tabdin_n_uc_ano)
}

# Documentacao da funcao plotar_distribuicao_distancia_interativo() --------------------
#' Gera tres graficos interativos descrevendo a variacao das distancias perpendiculares observadas
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
#' @examples \dontrun{plotar_distribuicao_distancia_interativo()}
plotar_distribuicao_distancia_interativo <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados_transformados_dist_r.rds"
      )
    ),
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

# Documentacao da funcao plotar_funcao_deteccao_modelos_selecionad --------
#' Title
#'
#' @param dados
#'
#' @return
#' @export
#'
#' @examples
plotar_funcao_deteccao_modelos_selecionados <- function(dados) {
  dados |>
    purrr::map(
      \(.x) plot(
        .x,
        xlab = "Distancia (m)",
        ylab = "Probabilidade de detecacao",
        pl.col = "chartreuse4"
      )
    )
}

# Documentacao da funcao plotar_funcao_deteccao_selecao_distancia_truncamento() --------
#' Title
#'
#' @param dados
#'
#' @return
#' @export
#'
#' @examples
plotar_funcao_deteccao_selecao_distancia_truncamento <- function(dados) {
  dados$modelos |>
    purrr::map(
      \(.x) plot(
        .x,
        xlab = "Distancia (m)",
        ylab = "Probabilidade de detecacao",
        pl.col = "chartreuse4"
      )
    )
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

# Documentacao da funcao plotar_n_obs_sp_interativo() --------------------
#' Gera graficos de barras interativos com o numero de observacoes por especie
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
#' @examples \dontrun{plotar_n_obs_sp_interativo()}
plotar_n_obs_sp_interativo <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_sp.rds"
      )
    )
) {
  fig <- plotly::ggplotly(
    # gerar figura
    fig <- dados |>
      dplyr::mutate(
        n_obs = dplyr::case_when(
          n > 1000 ~ "Mais de 1000 observacoes",
          n %in% 501:1000 ~ "Entre 501 e 1000 observacoes",
          n %in% 101:500 ~ "Entre 101 e 500 observacoes",
          n %in% 1:100 ~ "Ate 100 observacoes"
        ),
        nome_sp = forcats::fct_reorder(
          nome_sp,
          n
        ),
        n_obs = forcats::fct_reorder(
          n_obs,
          dplyr::desc(n)
        )
      ) |>
      ggplot2::ggplot() +
      ggplot2::aes(y = nome_sp,
                   x = n,
                   label = nome_sp_abv) +
      ggplot2::geom_col(
        fill = "chartreuse4",
        #width = .9
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
        # Remove labels from the vertical axis
        axis.text.y = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(size = 10),
      )
  )

  # retronar os graficos
  return(fig)
}

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
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_sp_uc.rds"
      )
    )
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
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/n_obs_uc.rds"
      )
    )
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

# Documentacao da funcao plotar_n_obs_validadas_interativo() --------------------
#' Gera um grafico de barras interativo com o numero de observacoes validadas para cada nivel taxonomico
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
#' @examples \dontrun{plotar_n_obs_validadas_interativo()}
plotar_n_obs_validadas_interativo <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/tabela_n_obs_validadas.rds"
      )
    )
) {
  #
  grafico_n_sp_validada <- dados  |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = validacao,
      y = n,
    ) +
    ggplot2::geom_col(
      fill = "chartreuse4"
    ) +
    ggplot2::labs(
      title = "Numero de obs. validadas para \ncada nivel taxonomico",
      x = "Nivel taxonomico",
      y = "Contagem"
    ) +
    ggplot2::theme_minimal(14)

  # gerar grafico interativo
  grafico_n_sp_validada <- plotly::ggplotly(
    grafico_n_sp_validada
  )

  return(grafico_n_sp_validada)
}

# Documentacao da funcao selecionar_distancia_truncamento() ------------------
#' Title
#'
#' @param dados
#' @param dist_truncamento
#'
#' @return
#' @export
#'
#' @examples
selecionar_distancia_truncamento <- function(
    dados,
    dist_truncamento = list(
      `25%` = "25%",
      `20%` = "20%",
      `15%` = "15%",
      `10%` = "10%",
      `5%` = "5%"
    )
) {

  # ajustar modelos com funcao de deteccao do tipo Half-normal e sem termos
  # de ajuste a diferentes distancias de truncamento
  modelos_hn_diferentes_dist_truncamento <- purrr::map(
    dist_truncamento,
    \(.x) Distance::ds(
      dados,
      key = NULL,
      adjustment = NULL,
      truncation = .x
    )
  )

  # performa a selecao de modelos e gera um data.frame com os resultados
  tabela_selecao_dist_truncamento <- modelos_hn_diferentes_dist_truncamento |>
    purrr::map(
      \(x) Distance::summarize_ds_models(x, delta_only = FALSE)
    ) |>
    purrr::list_rbind() |>
    dplyr::mutate(AIC = round(AIC, 3))

  # gerar uma tabela com os valores da AIC e as distancias de truncamento
  # correspondentes
  aic <- modelos_hn_diferentes_dist_truncamento |>
    purrr::map(\(x) stats::AIC(x)) |>
    purrr::list_rbind() |>
    dplyr::mutate(dist_truncamento = names(dist_truncamento),
           AIC = round(AIC, 3))

  # corrigir a coluna com as distancias de truncamento adequadas
  tabela_selecao_dist_truncamento <- tabela_selecao_dist_truncamento |>
    dplyr::left_join(aic, dplyr::join_by(AIC)) |>
    dplyr::mutate(Model = dist_truncamento) |>
    dplyr::select(!df:dist_truncamento)

  # retorna um data.frame com a selecao da melhor distancia de truncamento
  return(list(
    modelos = modelos_hn_diferentes_dist_truncamento,
    selecao = tabela_selecao_dist_truncamento
  ))

}

# Documentacao da funcao selecionar_funcao_deteccao_termo_ajuste() ------------------
#' Title
#'
#' @param dados
#'
#' @return
#' @export
#'
#' @examples
selecionar_funcao_deteccao_termo_ajuste <- function(
    dados
) {

  # gerar rank das funcoes de deteccao e termos de ajuste
  selecao_funcao_deteccao_termo_ajuste <- Distance::summarize_ds_models(
    dados$`half-normal`$`Sem termo`,
    dados$`half-normal`$Cosseno,
    dados$`half-normal`$`Hermite polinomial`,
    dados$`hazard-rate`$`Sem termo`,
    dados$`hazard-rate`$Cosseno,
    dados$`hazard-rate`$`Polinomial simples`,
    dados$uniforme$Cosseno,
    dados$uniforme$`Polinomial simples`,
    delta_only = FALSE
  )

  #
  tabela_selecao_funcao_deteccao_termo_ajuste <- selecao_funcao_deteccao_termo_ajuste |>
    dplyr::select(!1) |>
    dplyr::rename(Model = `Key function`) |>
    dplyr::distinct()

  # retorna um data.frame com a selecao das funcoes de deteccao e termos
  # de ajuste
  return(tabela_selecao_funcao_deteccao_termo_ajuste)

}

# Documentacao da funcao testar_bondade_ajuste() --------------------------
#' Title
#'
#' @param dados
#'
#' @return
#' @export
#'
#' @examples
testar_bondade_ajuste <- function(
    dados,
    plot = FALSE,
    chisq = FALSE,
    nc = NULL,
    breaks = NULL
) {

  # gera uma lista com os resultados dos testes de bondade de ajuste
  bondade_ajuste <- dados |>
    purrr::map(
      \(x) Distance::gof_ds(
        x,
        plot = plot,
        chisq = chisq,
        nc = nc,
        breaks = breaks
      )
    ) |>
    # gerar o data.frame com os resultados dos testes de bondade de ajuste
    purrr::map(
      \(x) data.frame(x$dsgof$CvM)
    ) |>
    purrr::list_rbind() |>
    dplyr::mutate(Modelo = names(dados)) |>
    dplyr::relocate(Modelo, .before = W)

  # retornar o data.frame com o resultado dos testes de bondade de ajuste
  return(bondade_ajuste)
}

# Documentacao da funcao transformar_para_distanceR_covariaveis() --------------------
#' Gera uma tabela no formato para analise no pacote Distance do R, com duas covariaveis, a partir dos dados selecionados
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{tibble}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{transformar_para_distanceR_covariaveis()}
# transforma para o formato para analise no pacote distance do R
# adicionando duas covariaveis
transformar_para_distanceR_covariaveis <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    )
) {
  dados_transformados_dist_r_cov <- dados |>
    dplyr::select(
      Region.Label = uc_name,
      Sample.Label = `ea_name`,
      Effort = total_effort,
      sampling_day,
      uc_name_abv,
      sp_name,
      sp_name_abv,
      distance,
      year,
      size = group_size,
      cense_time
    ) |>
    dplyr::mutate(
      Area = 0,
      # Sample.Label = lubridate::date(Sample.Label),
      object = 1:nrow(dados)
    ) |>
    dplyr::relocate(
      Area,
      .before = Sample.Label
    )

  # garvar no diretorio inst/extdata/ arquivo dados_transformados_dist_r_cov.rds
  # readr::write_rds(
  #   dados_transformados_dist_r_cov,
  #   file = paste0(
  #     here::here(),
  #     "/data/dados_selecionados_transformados_dist_r_cov.rds"
  #   )
  # )

  # retorna o data.frame
  return(dados_transformados_dist_r_cov)
}

# Documentacao da funcao transformar_para_distanceR_covariaveis() --------------------
#' Gera uma tabela no formato para analise no pacote Distance do R, com duas covariaveis, a partir dos dados selecionados
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descircao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{tibble}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{transformar_para_distanceR_covariaveis_sem_repeticao()}
# transforma para o formato para analise no pacote distance do R
# adicionando duas covariaveis e mantendo o esforco sem contar as repeticoes
transformar_para_distanceR_covariaveis_sem_repeticao <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    )
) {
  dados_transformados_dist_r_cov_sem_repeticao <- dados |>
    dplyr::select(
      Region.Label = uc_name,
      Sample.Label = `ea_name`,
      Effort = day_effort,
      sampling_day,
      uc_name_abv,
      sp_name,
      sp_name_abv,
      distance,
      season = season,
      year,
      size = group_size,
      cense_time
    ) |>
    dplyr::mutate(
      Area = 0,
      # Sample.Label = lubridate::date(Sample.Label),
      object = 1:nrow(dados)
    ) |>
    dplyr::relocate(
      Area,
      .before = Sample.Label
    )

  # # garvar no diretorio inst/extdata/ arquivo dados_transformados_dist_r_cov.rds
  # readr::write_rds(
  #   dados_transformados_dist_r_cov,
  #   file = paste0(
  #     here::here(),
  #     "/data/dados_selecionados_transformados_dist_r_cov.rds"
  #   )
  # )
  # retorna o data.frame
  return(dados_transformados_dist_r_cov_sem_repeticao)
}

# Documentacao da funcao transformar_para_distanceR() --------------------
#' Gera uma tabela no formato para analise no pacote Distance do R, sem covariaveis, a partir dos dados selecionados
#'
#' @description
#' A funcao \code{} ...
#'
#' @param dados Descricao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{tibble}...
#' @author
#' Vitor N. T. Borges-Junior
#' Julia L. Luz
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{transformar_para_distanceR()}
transforma_para_distanceR_com_repeticao_filtra_uc_sp <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    ),
    nome_uc = "Resex Tapajos-Arapiuns",
    nome_sp = "Dasyprocta croconota"
) {
  # transforma dados para formato distance R
  dados_transformados_dist_r_com_repeticao_filtra_uc_sp <- dados |>
    dplyr::select(
      Region.Label = uc_name,
      Sample.Label = `ea_name`,
      Effort = total_effort,
      distance,
      sp_name,
      sampling_day,
      year,
      season
    ) |>
    dplyr::mutate(
      Area = 0
    ) |>
    dplyr::relocate(
      Area,
      .before = Sample.Label
    ) |>
    dplyr::filter(
      Region.Label == nome_uc,
      sp_name == nome_sp
    )

  # gerar coluna object
  dados_transformados_dist_r_com_repeticao_filtra_uc_sp <- dados_transformados_dist_r_com_repeticao_filtra_uc_sp |>
    dplyr::mutate(object = seq_along(dados_transformados_dist_r_com_repeticao_filtra_uc_sp$Region.Label))

  # # grava uma versao dados_transformados_dist_r.rds no diretorio data
  # readr::write_rds(
  #   dados_transformados_dist_r,
  #   file = paste0(
  #     here::here(),
  #     "/data/dados_selecionados_transformados_dist_r.rds"
  #   )
  # )

  # retorna o data.frame
  return(dados_transformados_dist_r_com_repeticao_filtra_uc_sp)
}

# Ducumentacao para transforma_para_dsitanceR_quase_sem_repeticao_ --------
#' Title
#'
#' @param dados
#' @param nome_uc
#' @param nome_sp
#'
#' @return
#' @export
#'
#' @examples
transforma_para_dsitanceR_quase_sem_repeticao_filtra_uc_sp <- function(
    dados = readr::read_rds(
      file = paste0(
        here::here(),
        "/data/dados_selecionados.rds"
      )
    ),
    nome_uc = "Resex Tapajos-Arapiuns",
    nome_sp = "Dasyprocta croconota"
) {
  # transforma dados para formato distance R
  dados_transformados_dist_r_quase_sem_repeticao_filtra_uc_sp <- dados |>
    dplyr::select(
      Region.Label = uc_name,
      Sample.Label = `ea_name`,
      Effort = day_effort,
      distance,
      sp_name,
      sampling_day,
      year,
      season,
    ) |>
    dplyr::mutate(
      Area = 0
    ) |>
    dplyr::relocate(
      Area,
      .before = Sample.Label
    ) |>
    dplyr::filter(
      Region.Label == nome_uc,
      sp_name == nome_sp
    )

  # gerar filtro para eliminar amostras repetidas mantendo o dia com o maior n de obs
  # gerar o n de obs por data de amostragem
  n_obs_data <- dados_transformados_dist_r_quase_sem_repeticao_filtra_uc_sp |>
    dplyr::group_by(Sample.Label, sampling_day, year, season) |>
    dplyr::count(sampling_day) |>
    dplyr::ungroup()

  # gerar as datas com maior n de obs em cada estacao e ano
  data_com_maior_n_obs <- dados_transformados_dist_r_quase_sem_repeticao_filtra_uc_sp |>
    dplyr::group_by(Sample.Label, year, season) |>
    dplyr::count(sampling_day) |>
    dplyr::reframe(n_max = max(n)) |>
    dplyr::ungroup()

  # juntar as duas data.frames para obter as datas com maior n de obs
  # em cada ano e excluir datas de amostragem repitidas na mesma estacao
  # e ano
  dados_para_filtrar_por_data_quase_sem_repeticao <- n_obs_data |>
    dplyr::semi_join(
      data_com_maior_n_obs,
      dplyr::join_by(Sample.Label, year, season, n == n_max),
    ) |>
    dplyr::distinct(sampling_day, year, season)

  # gerar o filtro de datas
  filtro_datas_quase_sem_repeticao <- dados_para_filtrar_por_data_quase_sem_repeticao$sampling_day

  # eliminar amostras repetidas
  dados_transformados_dist_r_quase_sem_repeticao_filtra_uc_sp <- dados_transformados_dist_r_quase_sem_repeticao_filtra_uc_sp |>
    dplyr::filter(sampling_day %in% filtro_datas_quase_sem_repeticao)

  # gerar coluna object
  dados_transformados_dist_r_quase_sem_repeticao_filtra_uc_sp <- dados_transformados_dist_r_quase_sem_repeticao_filtra_uc_sp |>
    dplyr::mutate(object = seq_along(dados_transformados_dist_r_quase_sem_repeticao_filtra_uc_sp$Region.Label))

  # # grava uma versao dados_transformados_dist_r.rds no diretorio data
  # readr::write_rds(
  #   dados_transformados_dist_r,
  #   file = paste0(
  #     here::here(),
  #     "/data/dados_selecionados_transformados_dist_r.rds"
  #   )
  # )

  # retorna o data.frame
  return(dados_transformados_dist_r_quase_sem_repeticao_filtra_uc_sp)
}

# resolvendo variaveis globais --------------------------------------------
# adicionar todas as variaveis
utils::globalVariables(
  c(
    "codigo_uc",
    "nome_uc",
    "numero_ea",
    "nome_ea",
    "data_amostragem",
    "ano",
    "estacao",
    "esforco_dia",
    "classe",
    "ordem",
    "familia",
    "genero",
    "nome_sp",
    "validacao",
    "distancia",
    "tamanho_grupo",
    "observadores",
    "horario_inicio",
    "horario_termino",
    "data",
    "tempo_censo",
    "novo",
    "obs1",
    "obs2",
    "obs3",
    "obs4",
    "obs5",
    "obs6",
    "numero_observadores",
    "esforco_dia2",
    "tempo_censo2",
    "n_repeticoes",
    "esforco_total",
    "categoria_uc",
    "Region.Label",
    "Sample.Label",
    "Effort.x",
    "Nhat",
    "Regiao",
    "caracteristicas_densidade",
    "distance",
    "nome_uc_abv",
    "nome_sp_abv",
    "uc_name",
    "uc_name_abv",
    "sp_name",
    "sp_name_abv",
    "year",
    "season",
    "day_effort",
    "total_effort",
    "sampling_day",
    "Area",
    "ea_name",
    "n",
    "n_obs",
    "n_max",
    "group_size",
    "cense_time",
    "W",
    "AIC",
    "df",
    "Key function",
    "Modelo",
    "Region",
    "CoveredArea",
    "Effort",
    "ER",
    "se.ER",
    "cv.ER",
    "Label"
  )
)


