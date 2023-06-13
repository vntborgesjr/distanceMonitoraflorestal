# test_that("retorna uma lista", {
#
#   # gerar dados filtrados para a uma espécie e uma UC e transformar para o formato para a análise no pacote Distance
#   dados_dasyp_croco_sem_repeticao <- filtrar_dados(
#     dados = monitora_aves_masto_florestal,
#     nome_ucs = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   ) |>
#     transformar_dados_formato_Distance(amostras_repetidas = FALSE)
#
#   # ajustar modelo com função chave Half-normal e todas os termos de ajsute possíveis
#   modelo_hn_dasyp_croco_resex_tap_arap <- dados_dasyp_croco_sem_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "hn",
#       termos_ajuste = NULL,
#       truncamento = "10%"
#     )
#
#   expect_equal(is.list(modelo_hn_dasyp_croco_resex_tap_arap), TRUE)
#
# })
#
# test_that("retorna uma lista de comprimento 3", {
#
#   # gerar dados filtrados para a uma espécie e uma UC e transformar para o formato para a análise no pacote Distance
#   dados_dasyp_croco_sem_repeticao <- filtrar_dados(
#     dados = monitora_aves_masto_florestal,
#     nome_ucs = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   ) |>
#     transformar_dados_formato_Distance(amostras_repetidas = FALSE)
#
#   # ajustar modelo com função chave Half-normal e todas os termos de ajsute possíveis
#   modelo_hn_dasyp_croco_resex_tap_arap <- dados_dasyp_croco_sem_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "hn",
#       termos_ajuste = NULL,
#       truncamento = "10%"
#     )
#
#   expect_equal(length(modelo_hn_dasyp_croco_resex_tap_arap), 3)
#
# })
