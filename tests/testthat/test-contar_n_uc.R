test_that("retorna um vetor", {
  expect_equal(is.vector(contar_n_uc()), TRUE)
})

test_that("retorna um vetor de comprimento 1", {
  expect_equal(length(contar_n_uc()), 1)
})

test_that("retorna 40 UC's", {
  expect_equal(contar_n_uc(), 40)
})

test_that("retorna 4 UC's", {
  dados_filtrados <- filtrar_dados(
    nome_sps = "dasyprocta_croconota"
  )
  expect_equal(contar_n_uc(dados_filtrados), 4)
})
