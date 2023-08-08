
# distanceMonitoraflorestal

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

O pacote `distanceMonitoraflorestal` foi desenvolvido para atender uma demanda do ICMBio que necessita aplicar a base de dados do [Programa Monitora Componente Florestal CENAP/CEMAVE](https://www.gov.br/icmbio/pt-br/assuntos/monitoramento) para obter estimativas de densidade populacional de aves e médios e grandes mamíferos de 40 Unidades de Conservação do Brasil. O objetivo do pacote é facilitar a exploração e análise dos dados oferencendo ao usuário funções intuitivas, com nome e documentação em português. As funções cobrem tarefas de todas as etapas de um fluxo de análise de dados para obtenção de estimativas de densidade populacional a partir de dados obtidos por amostragem por distância: transformação, seleção de variáveis, visualização e análise dos dados, estas com saídas em formatos gráfico e de tabelas para serem utilizados em relatórios para a apresntação dos resultados. Exemplos de como utilizar o pacote são fonecidos nos vignettes:

## Instalação

Você pode instalar a versão de desenvolvimento do pacote `distanceMonitoraflorestal` utilizando o seguinte codigo:

``` r
devtools::install_github("vntborgesjr/distanceMonitoraflorestal")

library(distanceMonitoraflorestal)
```

## Exemplos

Exemplos de como utilizar o pacote contendo todas as etapas de análise (carregamento, transformação, exploração e análise dos dados) são fornecidos nos vignettes. Para acessá-los utilize:

``` r
vignette("distanceMonitoraflorestal")
```

Ou acesse o índice de vignettes no link [Articles](file:///home/vitor/Documentos/R-projects/piper3d/wwf/distanceMonitoraflorestal/docs/articles/distanceMonitoraflorestal.html), localizado na parte superior da página. Para consultar a documentação das funções, use o link [Reference](file:///home/vitor/Documentos/R-projects/piper3d/wwf/distanceMonitoraflorestal/docs/reference/index.html)
