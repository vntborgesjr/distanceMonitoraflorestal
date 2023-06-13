# test_that("retorna uma tibble", {
#   expect_equal(dplyr::is.tbl(contar_n_obs_uc_ano()), TRUE)
# })
#
# test_that("retorna 161 linhas", {
#   expect_equal(nrow(contar_n_obs_uc_ano()), 161)
# })
#
# test_that("retorna 4 colunas", {
#   expect_equal(ncol(contar_n_obs_uc_ano()), 4)
# })
#
# test_that("retorna 8 linhas", {
#   dados_filtrados <- filtrar_dados(
#     nome_ucs = "resex_tapajos_arapiuns"
#   )
#   expect_equal(nrow(contar_n_obs_uc_ano(dados_filtrados)), 8)
# })
#
