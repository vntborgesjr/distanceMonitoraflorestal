test_that("retorna uma tibble", {
  expect_equal(dplyr::is.tbl(contar_n_obs_sp_ano()), TRUE)
})

test_that("retorna 940 linhas", {
  expect_equal(nrow(contar_n_obs_sp_ano()), 940)
})

test_that("retorna 4 colunas", {
  expect_equal(ncol(contar_n_obs_sp_ano()), 4)
})

test_that("retorna 263 linhas", {
  dados_filtrados <- filtrar_dados(
    nome_ucs = "resex_tapajos_arapiuns"
  )
  expect_equal(nrow(contar_n_obs_sp_ano(dados_filtrados)), 263)
})

