test_that("retorna um data.frame", {

  # gerar dados filtrados para a uma espécie e uma UC e transformar para o
  # formato para a análise no pacote Distance
  dados_dasy_croc_tap_arap <- filtrar_dados(
    dados = monitora_aves_masto_florestal,
    nome_ucs = "resex_tapajos_arapiuns",
    nome_sps = "dasyprocta_croconota",
    validacao_obs = "especie"
  ) |>
    transformar_dados_formato_Distance()

  # ajustar modelos com funções chave diferentes
  # modelo Half-normal
  modelo_hn <- dados_dasy_croc_tap_arap |>
    ajustar_modelos_Distance(
      funcao_chave = "hn",
      truncamento = 15
    )

  # modelo Hazard-rate
  modelo_hr <- dados_dasy_croc_tap_arap |>
    ajustar_modelos_Distance(
      funcao_chave = "hr",
      truncamento = 15
    )

  # modelo Uniform
  modelo_unif <- dados_dasy_croc_tap_arap |>
    ajustar_modelos_Distance(
      funcao_chave = "unif",
      truncamento = 15
    )

  # gerar a tabela de seleção com o resumo comparativo dos modelos
  modelos_selecionados <- selecionar_funcao_deteccao_termo_ajuste(
    modelo_hn$`Sem termo`,
    modelo_hn$Cosseno,
    modelo_hn$`Hermite polinomial`,
    modelo_hr$`Sem termo`,
    modelo_hr$Cosseno,
    modelo_hr$`Polinomial simples`,
    modelo_unif$Cosseno,
    modelo_unif$`Polinomial simples`
  )

  # gerar uma lista com os modelos selecionados ordenados do melhor para o pior modelo
  lista_modelos_selecionados <- gerar_lista_modelos_selecionados(
  modelo_hr$`Sem termo`,
  modelo_hn$Cosseno,
  modelo_unif$Cosseno,
  modelo_unif$`Polinomial simples`,
  modelo_hn$`Sem termo`,
  nome_modelos_selecionados = selecao_funcao_deteccao_termo_ajuste
  )

  # teste de bondade de ajuste dos modelos e Q-Q plots
  bondade_ajuste <- testar_bondade_ajuste(
    lista_modelos_selecionados,
    plot = TRUE,
    chisq = TRUE,
    intervalos_distancia =  NULL
  )

  expect_equal(class(bondade_ajuste), "data.frame")
})

test_that("retorna 3 colunas", {

  # gerar dados filtrados para a uma espécie e uma UC e transformar para o
  # formato para a análise no pacote Distance
  dados_dasy_croc_tap_arap <- filtrar_dados(
    dados = monitora_aves_masto_florestal,
    nome_ucs = "resex_tapajos_arapiuns",
    nome_sps = "dasyprocta_croconota",
    validacao_obs = "especie"
  ) |>
    transformar_dados_formato_Distance()

  # ajustar modelos com funções chave diferentes
  # modelo Half-normal
  modelo_hn <- dados_dasy_croc_tap_arap |>
    ajustar_modelos_Distance(
      funcao_chave = "hn",
      truncamento = 15
    )

  # modelo Hazard-rate
  modelo_hr <- dados_dasy_croc_tap_arap |>
    ajustar_modelos_Distance(
      funcao_chave = "hr",
      truncamento = 15
    )

  # modelo Uniform
  modelo_unif <- dados_dasy_croc_tap_arap |>
    ajustar_modelos_Distance(
      funcao_chave = "unif",
      truncamento = 15
    )

  # gerar a tabela de seleção com o resumo comparativo dos modelos
  modelos_selecionados <- selecionar_funcao_deteccao_termo_ajuste(
    modelo_hn$`Sem termo`,
    modelo_hn$Cosseno,
    modelo_hn$`Hermite polinomial`,
    modelo_hr$`Sem termo`,
    modelo_hr$Cosseno,
    modelo_hr$`Polinomial simples`,
    modelo_unif$Cosseno,
    modelo_unif$`Polinomial simples`
  )

  # gerar uma lista com os modelos selecionados ordenados do melhor para o pior modelo
  lista_modelos_selecionados <- gerar_lista_modelos_selecionados(
  modelo_hr$`Sem termo`,
  modelo_hn$Cosseno,
  modelo_unif$Cosseno,
  modelo_unif$`Polinomial simples`,
  modelo_hn$`Sem termo`,
  nome_modelos_selecionados = selecao_funcao_deteccao_termo_ajuste
  )

  # teste de bondade de ajuste dos modelos e Q-Q plots
  bondade_ajuste <- testar_bondade_ajuste(
    lista_modelos_selecionados,
    plot = TRUE,
    chisq = TRUE,
    intervalos_distancia =  NULL
  )

  expect_equal(ncol(bondade_ajuste), 3)
})

test_that("retorna 2 colunas numéricas", {

  # gerar dados filtrados para a uma espécie e uma UC e transformar para o
  # formato para a análise no pacote Distance
  dados_dasy_croc_tap_arap <- filtrar_dados(
    dados = monitora_aves_masto_florestal,
    nome_ucs = "resex_tapajos_arapiuns",
    nome_sps = "dasyprocta_croconota",
    validacao_obs = "especie"
  ) |>
    transformar_dados_formato_Distance()

  # ajustar modelos com funções chave diferentes
  # modelo Half-normal
  modelo_hn <- dados_dasy_croc_tap_arap |>
    ajustar_modelos_Distance(
      funcao_chave = "hn",
      truncamento = 15
    )

  # modelo Hazard-rate
  modelo_hr <- dados_dasy_croc_tap_arap |>
    ajustar_modelos_Distance(
      funcao_chave = "hr",
      truncamento = 15
    )

  # modelo Uniform
  modelo_unif <- dados_dasy_croc_tap_arap |>
    ajustar_modelos_Distance(
      funcao_chave = "unif",
      truncamento = 15
    )

  # gerar a tabela de seleção com o resumo comparativo dos modelos
  modelos_selecionados <- selecionar_funcao_deteccao_termo_ajuste(
    modelo_hn$`Sem termo`,
    modelo_hn$Cosseno,
    modelo_hn$`Hermite polinomial`,
    modelo_hr$`Sem termo`,
    modelo_hr$Cosseno,
    modelo_hr$`Polinomial simples`,
    modelo_unif$Cosseno,
    modelo_unif$`Polinomial simples`
  )

  # gerar uma lista com os modelos selecionados ordenados do melhor para o pior modelo
  lista_modelos_selecionados <- gerar_lista_modelos_selecionados(
  modelo_hr$`Sem termo`,
  modelo_hn$Cosseno,
  modelo_unif$Cosseno,
  modelo_unif$`Polinomial simples`,
  modelo_hn$`Sem termo`,
  nome_modelos_selecionados = selecao_funcao_deteccao_termo_ajuste
  )

  # teste de bondade de ajuste dos modelos e Q-Q plots
  bondade_ajuste <- testar_bondade_ajuste(
    lista_modelos_selecionados,
    plot = TRUE,
    chisq = TRUE,
    intervalos_distancia =  NULL
  )

  expect_equal(bondade_ajuste |>
                 dplyr::select(
                   where(is.double)
                 ) |>
                 ncol()
               , 2)
})

test_that("retorna 2 colunas caracter", {

  # gerar dados filtrados para a uma espécie e uma UC e transformar para o
  # formato para a análise no pacote Distance
  dados_dasy_croc_tap_arap <- filtrar_dados(
    dados = monitora_aves_masto_florestal,
    nome_ucs = "resex_tapajos_arapiuns",
    nome_sps = "dasyprocta_croconota",
    validacao_obs = "especie"
  ) |>
    transformar_dados_formato_Distance()

  # ajustar modelos com funções chave diferentes
  # modelo Half-normal
  modelo_hn <- dados_dasy_croc_tap_arap |>
    ajustar_modelos_Distance(
      funcao_chave = "hn",
      truncamento = 15
    )

  # modelo Hazard-rate
  modelo_hr <- dados_dasy_croc_tap_arap |>
    ajustar_modelos_Distance(
      funcao_chave = "hr",
      truncamento = 15
    )

  # modelo Uniform
  modelo_unif <- dados_dasy_croc_tap_arap |>
    ajustar_modelos_Distance(
      funcao_chave = "unif",
      truncamento = 15
    )

  # gerar a tabela de seleção com o resumo comparativo dos modelos
  modelos_selecionados <- selecionar_funcao_deteccao_termo_ajuste(
    modelo_hn$`Sem termo`,
    modelo_hn$Cosseno,
    modelo_hn$`Hermite polinomial`,
    modelo_hr$`Sem termo`,
    modelo_hr$Cosseno,
    modelo_hr$`Polinomial simples`,
    modelo_unif$Cosseno,
    modelo_unif$`Polinomial simples`
  )

  # gerar uma lista com os modelos selecionados ordenados do melhor para o pior modelo
  lista_modelos_selecionados <- gerar_lista_modelos_selecionados(
  modelo_hr$`Sem termo`,
  modelo_hn$Cosseno,
  modelo_unif$Cosseno,
  modelo_unif$`Polinomial simples`,
  modelo_hn$`Sem termo`,
  nome_modelos_selecionados = selecao_funcao_deteccao_termo_ajuste
  )

  # teste de bondade de ajuste dos modelos e Q-Q plots
  bondade_ajuste <- testar_bondade_ajuste(
    lista_modelos_selecionados,
    plot = TRUE,
    chisq = TRUE,
    intervalos_distancia =  NULL
  )

  expect_equal(bondade_ajuste |>
                 dplyr::select(
                   where(is.character)
                 ) |>
                 ncol()
               , 1)
})

