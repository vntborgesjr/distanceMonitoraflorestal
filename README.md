
# distanceMonitoraflorestal

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

O objetivo do distanceMonitoraflorestal é fornecer ferramentas que facilitem a seleção, visualização e análise de abundância e/ou densidade das espécies de aves e mamíferos de médio e grande porte, a partir dos dados brutos do Programa Monitora Componente Florestal do ICMBio/CENAP/CEMAVE. 

## Instalação

Você pode instalar a versão de desenvolvimento do distanceMonitoraflorestal utilizando o seguinte codigo:

``` r
devtools::install_github("vntborgesjr/distanceMonitoraflorestal")
```

## Exemplo

Esse é um exemplo basico de como você pode visualizar a distribuição de distâncias da espécie com o maior número de observações na base de dados, *Dasypoctra croconota* na Resex Tapajós-Arapiuns.

``` r
library(distanceMonitoraflorestal)
plotar_distribuicao_distancia_estatico()
```

