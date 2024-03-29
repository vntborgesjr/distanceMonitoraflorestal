% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contar_n_obs_uc_ano.R
\name{contar_n_obs_uc_ano}
\alias{contar_n_obs_uc_ano}
\title{Número total de observações em cada Unidade de Conservação por ano}
\usage{
contar_n_obs_uc_ano(dados = monitora_aves_masto_florestal)
}
\arguments{
\item{dados}{recebe a \code{tibble} gerada pela função \code{\link[=filtrar_dados]{filtrar_dados()}}. Por configuração, carrega a base de dados burtos de aves e médios e grandes mamíferos do Pojeto Monitora Componente Florestal \code{monitora_aves_masto_florestal}.}
}
\value{
Retorna um objeto do tipo \code{tibble} contendo quatro colunas, \code{nome_uc} contendo o nome da Unidade de Conservação, \code{nome_uc_abv} que contem o nome abreviado da Unidade de Conservação, \code{ano} que contem o ano da amostragem e \code{n} contendo o número de observações realizadas para cada Unidade de Conservação.
}
\description{
A função \code{contar_n_obs_uc_ano()} gera uma tabela contendo o número total de observações para cada Unidade de Conservação em cada ano.
}
\examples{
# número de observações para cada UC - dados brutos
contar_n_obs_uc_ano()

# número de observações para cada UC - dados filtrados
dados_filtrados <- filtrar_dados(
dados = monitora_aves_masto_florestal,
  nome_uc == "resex_tapajos_arapiuns"
)

contar_n_obs_uc_ano(dados_filtrados)
}
\author{
Vitor N. T. Borges-Junior
Luciana A. Fusinatto
}
