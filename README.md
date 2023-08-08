
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

Exemplos de como utilizar o pacote contendo todas as etapas de análise (carregamento, transformação, exploração e análise dos dados) são fonecidos nos vignettes. Para acessá-los utilize:

``` r
vignette("distanceMonitoraflorestal")
```

- [Carregando-uma-base-de-dados-atualizada](doc/Carregando-uma-base-de-dados-atualizada.html);

- [Checagem-filtragem-e-transformacao-dos-dados](doc/Checagem-filtragem-e-transformacao-dos-dados.html);

- [Ajuste-dos-modelos-dist-exatas-com-repeticao](doc/Ajuste-dos-modelos-dist-exatas-com-repeticao.html);

- [Ajuste-dos-modelos-dist-exatas-sem-repeticao](doc/Ajuste-dos-modelos-dist-exatas-sem-repeticao.html);

- [Ajuste-dos-modelos-dist-agrup-com-repeticoes](doc/Ajuste-dos-modelos-dist-agrup-com-repeticoes.html)

- [Ajuste-dos-modelos-multiplas-covariaveis](doc/Ajuste-dos-modelos-multiplas-covariaveis.html);

- [Ajuste-dos-modelos-dist-estratificada-por-uc](doc/Ajuste-dos-modelos-dist-estratificada-por-uc.html);

- [Ajuste-dos-modelos-dist-estratificada-por-ano](doc/Ajuste-dos-modelos-dist-estratificada-por-ano.html)
