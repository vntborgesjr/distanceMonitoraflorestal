# test_that("retorna uma lista", {
#
#   # gerar dados filtrados
#   dados_filtrados <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   )
#
#   # gerar os dados transformados com repeticao
#   dados_distance_com_repeticao <- transformar_dados_formato_Distance(dados_filtrados)
#
#   # modelo Half-normal
#   modelo_hn <- dados_distance_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "hn",
#       truncamento = 15
#     )
#
#   # modelo Hazard-rate
#   modelo_hr <- dados_distance_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "hr",
#       truncamento = 15
#     )
#
#   # modelo Uniform
#   modelo_unif <- dados_distance_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "unif",
#       truncamento = 15
#     )
#
#   # gerar a tabela de seleção com o resumo comparativo dos modelos
#   selecao_funcao_deteccao_termo_ajuste <- selecionar_funcao_deteccao_termo_ajuste(
#     modelo_hn$`Sem termo`,
#     modelo_hn$Cosseno,
#     modelo_hn$`Hermite polinomial`,
#     modelo_hr$`Sem termo`,
#     modelo_hr$Cosseno,
#     modelo_hr$`Polinomial simples`,
#     modelo_unif$Cosseno,
#     modelo_unif$`Polinomial simples`
#   )
#
#   # gerar uma lista nomeada com os modelos selecionados ordenados do melhor para o pior modelo
#   lista_modelos_selecionados <- gerar_lista_modelos_selecionados(
#     modelo_hr$`Sem termo`,
#     modelo_hn$Cosseno,
#     modelo_unif$Cosseno,
#     modelo_unif$`Polinomial simples`,
#     modelo_hn$`Sem termo`,
#     nome_modelos_selecionados = selecao_funcao_deteccao_termo_ajuste
#   )
#
#   expect_equal(is.list(lista_modelos_selecionados), TRUE)
# })
#
# test_that("retorna dsmodel", {
#
#   # gerar dados filtrados
#   dados_filtrados <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   )
#
#   # gerar os dados transformados com repeticao
#   dados_distance_com_repeticao <- transformar_dados_formato_Distance(dados_filtrados)
#
#   # modelo Half-normal
#   modelo_hn <- dados_distance_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "hn",
#       truncamento = 15
#     )
#
#   # modelo Hazard-rate
#   modelo_hr <- dados_distance_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "hr",
#       truncamento = 15
#     )
#
#   # modelo Uniform
#   modelo_unif <- dados_distance_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "unif",
#       truncamento = 15
#     )
#
#   # gerar a tabela de seleção com o resumo comparativo dos modelos
#   selecao_funcao_deteccao_termo_ajuste <- selecionar_funcao_deteccao_termo_ajuste(
#     modelo_hn$`Sem termo`,
#     modelo_hn$Cosseno,
#     modelo_hn$`Hermite polinomial`,
#     modelo_hr$`Sem termo`,
#     modelo_hr$Cosseno,
#     modelo_hr$`Polinomial simples`,
#     modelo_unif$Cosseno,
#     modelo_unif$`Polinomial simples`
#   )
#
#   # gerar uma lista nomeada com os modelos selecionados ordenados do melhor para o pior modelo
#   lista_modelos_selecionados <- gerar_lista_modelos_selecionados(
#     modelo_hr$`Sem termo`,
#     modelo_hn$Cosseno,
#     modelo_unif$Cosseno,
#     modelo_unif$`Polinomial simples`,
#     modelo_hn$`Sem termo`,
#     nome_modelos_selecionados = selecao_funcao_deteccao_termo_ajuste
#   )
#
#   expect_equal(class(lista_modelos_selecionados[[1]]), "dsmodel")
# })
#
# test_that("retorna ds e ddf", {
#
#   # gerar dados filtrados
#   dados_filtrados <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   )
#
#   # gerar os dados transformados com repeticao
#   dados_distance_com_repeticao <- transformar_dados_formato_Distance(dados_filtrados)
#
#   # modelo Half-normal
#   modelo_hn <- dados_distance_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "hn",
#       truncamento = 15
#     )
#
#   # modelo Hazard-rate
#   modelo_hr <- dados_distance_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "hr",
#       truncamento = 15
#     )
#
#   # modelo Uniform
#   modelo_unif <- dados_distance_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "unif",
#       truncamento = 15
#     )
#
#   # gerar a tabela de seleção com o resumo comparativo dos modelos
#   selecao_funcao_deteccao_termo_ajuste <- selecionar_funcao_deteccao_termo_ajuste(
#     modelo_hn$`Sem termo`,
#     modelo_hn$Cosseno,
#     modelo_hn$`Hermite polinomial`,
#     modelo_hr$`Sem termo`,
#     modelo_hr$Cosseno,
#     modelo_hr$`Polinomial simples`,
#     modelo_unif$Cosseno,
#     modelo_unif$`Polinomial simples`
#   )
#
#   # gerar uma lista nomeada com os modelos selecionados ordenados do melhor para o pior modelo
#   lista_modelos_selecionados <- gerar_lista_modelos_selecionados(
#     modelo_hr$`Sem termo`,
#     modelo_hn$Cosseno,
#     modelo_unif$Cosseno,
#     modelo_unif$`Polinomial simples`,
#     modelo_hn$`Sem termo`,
#     nome_modelos_selecionados = selecao_funcao_deteccao_termo_ajuste
#   )
#
#   expect_equal(class(lista_modelos_selecionados[[1]]$ddf), c("ds", "ddf"))
# })
#
# test_that("retorna dht", {
#
#   # gerar dados filtrados
#   dados_filtrados <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   )
#
#   # gerar os dados transformados com repeticao
#   dados_distance_com_repeticao <- transformar_dados_formato_Distance(dados_filtrados)
#
#   # modelo Half-normal
#   modelo_hn <- dados_distance_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "hn",
#       truncamento = 15
#     )
#
#   # modelo Hazard-rate
#   modelo_hr <- dados_distance_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "hr",
#       truncamento = 15
#     )
#
#   # modelo Uniform
#   modelo_unif <- dados_distance_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "unif",
#       truncamento = 15
#     )
#
#   # gerar a tabela de seleção com o resumo comparativo dos modelos
#   selecao_funcao_deteccao_termo_ajuste <- selecionar_funcao_deteccao_termo_ajuste(
#     modelo_hn$`Sem termo`,
#     modelo_hn$Cosseno,
#     modelo_hn$`Hermite polinomial`,
#     modelo_hr$`Sem termo`,
#     modelo_hr$Cosseno,
#     modelo_hr$`Polinomial simples`,
#     modelo_unif$Cosseno,
#     modelo_unif$`Polinomial simples`
#   )
#
#   # gerar uma lista nomeada com os modelos selecionados ordenados do melhor para o pior modelo
#   lista_modelos_selecionados <- gerar_lista_modelos_selecionados(
#     modelo_hr$`Sem termo`,
#     modelo_hn$Cosseno,
#     modelo_unif$Cosseno,
#     modelo_unif$`Polinomial simples`,
#     modelo_hn$`Sem termo`,
#     nome_modelos_selecionados = selecao_funcao_deteccao_termo_ajuste
#   )
#
#   expect_equal(class(lista_modelos_selecionados[[1]]$dht), "dht")
# })
#
# test_that("retorna call", {
#
#   # gerar dados filtrados
#   dados_filtrados <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   )
#
#   # gerar os dados transformados com repeticao
#   dados_distance_com_repeticao <- transformar_dados_formato_Distance(dados_filtrados)
#
#   # modelo Half-normal
#   modelo_hn <- dados_distance_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "hn",
#       truncamento = 15
#     )
#
#   # modelo Hazard-rate
#   modelo_hr <- dados_distance_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "hr",
#       truncamento = 15
#     )
#
#   # modelo Uniform
#   modelo_unif <- dados_distance_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "unif",
#       truncamento = 15
#     )
#
#   # gerar a tabela de seleção com o resumo comparativo dos modelos
#   selecao_funcao_deteccao_termo_ajuste <- selecionar_funcao_deteccao_termo_ajuste(
#     modelo_hn$`Sem termo`,
#     modelo_hn$Cosseno,
#     modelo_hn$`Hermite polinomial`,
#     modelo_hr$`Sem termo`,
#     modelo_hr$Cosseno,
#     modelo_hr$`Polinomial simples`,
#     modelo_unif$Cosseno,
#     modelo_unif$`Polinomial simples`
#   )
#
#   # gerar uma lista nomeada com os modelos selecionados ordenados do melhor para o pior modelo
#   lista_modelos_selecionados <- gerar_lista_modelos_selecionados(
#     modelo_hr$`Sem termo`,
#     modelo_hn$Cosseno,
#     modelo_unif$Cosseno,
#     modelo_unif$`Polinomial simples`,
#     modelo_hn$`Sem termo`,
#     nome_modelos_selecionados = selecao_funcao_deteccao_termo_ajuste
#   )
#
#   expect_equal(class(lista_modelos_selecionados[[1]]$call), "call")
# })
#
#
