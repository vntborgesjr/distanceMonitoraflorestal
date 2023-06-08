testthat::test_that("retorna tibble", {
  testthat::expect_equal(dplyr::is.tbl(gerar_dados_selecionados()), TRUE)
})

testthat::test_that("retorna 19 colunas", {
  testthat::expect_equal(ncol(gerar_dados_selecionados()), 19)
})

testthat::test_that("retorna 19747 linhas", {
  testthat::expect_equal(nrow(gerar_dados_selecionados()), 19747)
})

testthat::test_that("retorna 8 fatores", {
  testthat::expect_equal(gerar_dados_selecionados() |>
                           dplyr::select(dplyr::where(is.factor)) |>
                           ncol(),
                         8
  )
})

testthat::test_that("retorna 10 double", {
  testthat::expect_equal(gerar_dados_selecionados() |>
                           dplyr::select(dplyr::where(is.double)) |>
                           ncol(),
                         10
  )
})

testthat::test_that("retorna 1 integer", {
  testthat::expect_equal(gerar_dados_selecionados() |>
                           dplyr::select(dplyr::where(is.integer)) |>
                           ncol(),
                         1
  )
})
