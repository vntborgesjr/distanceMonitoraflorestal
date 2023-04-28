
# distanceMonitoraflorestal

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

O objetivo do distanceMonitoraflorestal e fornecer ferramentas que facilitem a selecao, visualizacao e analise de densidade das especies de aves e medios e grandes mamiferos a partir dos dados brutos do Programa Monitora Componente Florestal do ICMBio/CENAP/CEMAVE. 

## Instalacao

Voce pode instalar a versao de desenvolvimento do distanceMonitoraflorestal utilizando o seguinte codigo:

``` r
devtools::install_github("distanceMonitoraflorestal")
```

## ExEmplo

Esse e um exemplo basico de como vc pode visualizar a distribuicao de distancias da especie com o maior numero de observacoes na base de dados, *Dasypoctra croconota* na Resex Tapajos-Arapiuns.

``` r
library(distanceMonitoraflorestal)
plotar_distribuicao_distancia_estatico()
```

