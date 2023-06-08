test_that("retorna uma tibble", {
  expect_equal(dplyr::is.tbl(contar_n_obs_sp_uc_estacao_ano()), TRUE)
})

test_that("retorna 3940 linhas", {
  expect_equal(nrow(contar_n_obs_sp_uc_estacao_ano()), 3940)
})

test_that("retorna 7 colunas", {
  expect_equal(ncol(contar_n_obs_sp_uc_estacao_ano()), 7)
})

test_that("retorna 366 linhas", {
  dados_filtrados <- filtrar_dados(
    nome_ucs = "resex_tapajos_arapiuns"
  )
  expect_equal(nrow(contar_n_obs_sp_uc_estacao_ano(dados_filtrados)), 366)
})
