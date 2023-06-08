test_that("filtragem por uma UC", {
  expect_equal(gerar_dados_filtrados(
    nome_ucs = "resex_tapajos_arapiuns"
  ) |>
    nrow(), 3576)
})

test_that("filtragem por uma UC", {
  expect_equal(gerar_dados_filtrados(
    nome_ucs = c("resex_tapajos_arapiuns", "resex_barreiro_das_antas")
  ) |>
    nrow(), 4136)
})

test_that("filtragem por uma especie", {
  expect_equal(gerar_dados_filtrados(
    nome_sps = "dasyprocta_croconota"
  ) |>
    nrow(), 2069)
})

test_that("filtragem por duas especies", {
  expect_equal(gerar_dados_filtrados(
    nome_sps = c("dasyprocta_croconota", "dasyprocta_iacki")
  ) |>
    nrow(), 2075)
})

test_that("filtragem por uma UC e uma especie", {
  expect_equal(gerar_dados_filtrados(
    nome_ucs = "resex_tapajos_arapiuns",
    nome_sps = "dasyprocta_croconota"
  ) |>
    nrow(), 1352)
})
