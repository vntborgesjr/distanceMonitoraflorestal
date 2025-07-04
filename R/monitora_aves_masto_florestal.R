# Documentacao da base de dados monitora_aves_masto_florestal -------------

#' Dados brutos de aves e mamiferos do Projeto Monitora Componente Florestal
#'
#' Traz os dados de aves e mamiferos de medio e grande porte obtidos por amostragem por distancia em 40 Unidades de Conservacao do Brasil entre os anos de 2014 e 2022.
#'
#' @format ## 'monitora_aves_masto_florestal'
#' Uma \code{tibble} que contem 27.887 linhas de 22 colunas:
#' \describe{
#'   \item{categoria_uc}{categoria da Unidade de Conservacao}
#'   \item{nome_uc}{nome da Unidade de Conservacao}
#'   \item{nome_uc_abv}{nome da Unidade de Conservacao abreviado}
#'   \item{codigo_uc}{codigo da Unidade de Conservacao}
#'   \item{nome_ea}{nome da Estacao Amostral}
#'   \item{numero_ea}{numero da Estacao Amostral dentro de uma Unidade de Conservacao}
#'   \item{data_amostragem}{ano, mes e dia em que a observacao foi realizada}
#'   \item{estacao}{estacao do ano em que a observacao foi realizada}
#'   \item{ano}{ano em que a observacao foi realizada}
#'   \item{esforco_dia}{distancia total percorrida em metros em um dia da amostragem na Estacao Amostral equivalente}
#'   \item{nome_classe}{nome da classe ao qual o individuo observado pertence}
#'   \item{nome_ordem}{nome da ordem ao qual o individuo observado pertence}
#'   \item{nome_familia}{nome do família ao qual o individuo observado pertence}
#'   \item{nome_genero}{nome do gênero ao qual o individuo observado pertence}
#'   \item{nome_sp}{nome do nivel taxonomico ao qual o individuo observado pertence, gênero sp. ou espécie}
#'   \item{nome_sp_abv}{nome abreviado do nivel taxonomico do individuo observado, ex. se o individuo foi identificado ao nivel de familia, corresponde a familia, se foi identificado ao nivel de genero, corresponde ao genero, se foi identificado ao nivel de especie, corresponde a especie}
#'   \item{validacao}{nivel taxonomico de validacao do individuo observado}
#'   \item{distancia}{distancia perpendicular em relacao ao amostrador na qual o individuo foi observado}
#'   \item{tamanho_grupo}{numero de individuos registrados na mesma observacao}
#'   \item{velocidade_km_h}{velocidade média do percurso do transecto}
#'   \item{tempo_censo}{tempo total de duracao do percurso de uma estacao amostral}
#'   \item{numero_observadores}{numero de observadores que precorriam o transecto na ocasiao da observacao}
#' }
#'
#' @source https://www.gov.br/icmbio/pt-br/assuntos/monitoramento/conteudo/dados
"monitora_aves_masto_florestal"
