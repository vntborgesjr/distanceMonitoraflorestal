## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup--------------------------------------------------------------------
#  library(distanceMonitoraflorestal)

## ---- fig.height=15, fig.width=10---------------------------------------------
#  cutia_tap_arap |>
#    tidyr::drop_na(distance) |>
#  plotar_distribuicao_distancia_interativo(largura_caixa = 1)
#  

## ---- fig.height=15, fig.width=10---------------------------------------------
#  cutia_tap_arap |>
#    tidyr::drop_na(distance) |>
#  plotar_distribuicao_distancia_interativo(largura_caixa = 2)
#  

## ---- fig.height=15, fig.width=10---------------------------------------------
#  cutia_tap_arap |>
#    tidyr::drop_na(distance) |>
#  plotar_distribuicao_distancia_interativo(largura_caixa = 3)
#  

## ---- fig.height=15, fig.width=10---------------------------------------------
#  cutia_tap_arap |>
#    tidyr::drop_na(distance) |>
#  plotar_distribuicao_distancia_interativo(largura_caixa = 5)
#  

## ---- eval=FALSE--------------------------------------------------------------
#  cutia_tap_arap_bin1 <- cutia_tap_arap |>
#    definir_intervalos_distancia(
#      intervalos_distancia = seq(
#        from = 0,
#        to = 15,
#        by = 1.5
#        )
#    )

## ---- eval=FALSE--------------------------------------------------------------
#  cutia_tap_arap_bin2 <- cutia_tap_arap |>
#    definir_intervalos_distancia(
#      intervalos_distancia = c(
#        0,
#        seq(
#        from = 1,
#        to = 15,
#        by = 1.4
#        )
#      )
#    )
#  

## ---- eval=FALSE--------------------------------------------------------------
#  cutia_tap_arap_bin3 <- cutia_tap_arap |>
#    definir_intervalos_distancia(
#      intervalos_distancia = seq(
#        from = 0,
#        to = 15,
#        by = 2.5
#        )
#    )
#  

## ---- eval=FALSE--------------------------------------------------------------
#  cutia_tap_arap_bin1_unif <- cutia_tap_arap_bin1 |>
#    ajustar_modelos_Distance(
#      funcao_chave = "unif",
#      termos_ajuste = "cos",
#      truncamento = 15)
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  cutia_tap_arap_bin2_unif <- cutia_tap_arap_bin2 |>
#    ajustar_modelos_Distance(
#      funcao_chave = "unif",
#      termos_ajuste = "cos",
#      truncamento = 15)
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  cutia_tap_arap_bin3_unif <- cutia_tap_arap_bin3 |>
#    ajustar_modelos_Distance(
#      funcao_chave = "unif",
#      termos_ajuste = "cos",
#      truncamento = 15)
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  cutia_tap_arap_bin1_hn <- cutia_tap_arap_bin1 |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      truncamento = 15)
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  cutia_tap_arap_bin2_hn <- cutia_tap_arap_bin2 |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      truncamento = 15)
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  cutia_tap_arap_bin3_hn <- cutia_tap_arap_bin3 |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      truncamento = 15)
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  cutia_tap_arap_bin1_hr <- cutia_tap_arap_bin1 |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hr",
#      truncamento = 15)
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  cutia_tap_arap_bin2_hr <- cutia_tap_arap_bin2 |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hr",
#      truncamento = 15)
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  cutia_tap_arap_bin3_hr <- cutia_tap_arap_bin3 |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hr",
#      truncamento = 15)
#  

## ---- eval=FALSE--------------------------------------------------------------
#  fluxo3.1_selecao_funcao_deteccao_termo_ajuste <- selecionar_funcao_deteccao_termo_ajuste(
#    cutia_tap_arap_bin1_unif,
#    cutia_tap_arap_bin1_hn$`Sem termo`,
#    cutia_tap_arap_bin1_hn$Cosseno,
#    cutia_tap_arap_bin1_hn$`Hermite polinomial`,
#    cutia_tap_arap_bin1_hr$`Sem termo`,
#    cutia_tap_arap_bin1_hr$Cosseno,
#    cutia_tap_arap_bin1_hr$`Polinomial simples`,
#    distancia_categorizada = TRUE
#  )
#  
#  fluxo3.1_selecao_funcao_deteccao_termo_ajuste
#  

## ---- eval=FALSE--------------------------------------------------------------
#  fluxo3.2_selecao_funcao_deteccao_termo_ajuste <- selecionar_funcao_deteccao_termo_ajuste(
#    cutia_tap_arap_bin2_unif,
#    cutia_tap_arap_bin2_hn$`Sem termo`,
#    cutia_tap_arap_bin2_hn$Cosseno,
#    cutia_tap_arap_bin2_hn$`Hermite polinomial`,
#    cutia_tap_arap_bin2_hr$`Sem termo`,
#    cutia_tap_arap_bin2_hr$Cosseno,
#    cutia_tap_arap_bin2_hr$`Polinomial simples`,
#    distancia_categorizada = TRUE
#  )
#  
#  fluxo3.2_selecao_funcao_deteccao_termo_ajuste

## ---- eval=FALSE--------------------------------------------------------------
#  fluxo3.3_selecao_funcao_deteccao_termo_ajuste <- selecionar_funcao_deteccao_termo_ajuste(
#    cutia_tap_arap_bin3_unif,
#    cutia_tap_arap_bin3_hn$`Sem termo`,
#    cutia_tap_arap_bin3_hn$Cosseno,
#    cutia_tap_arap_bin3_hn$`Hermite polinomial`,
#    cutia_tap_arap_bin3_hr$`Sem termo`,
#    cutia_tap_arap_bin3_hr$Cosseno,
#    cutia_tap_arap_bin3_hr$`Polinomial simples`,
#    distancia_categorizada = TRUE
#  )
#  
#  fluxo3.3_selecao_funcao_deteccao_termo_ajuste

## ---- eval=FALSE--------------------------------------------------------------
#  # Gráficos de ajuste das funções de deteção às probabilidades de deteção
#  fluxo3.1_modelos_selecionados <- gerar_lista_modelos_selecionados(
#    cutia_tap_arap_bin1_hn$`Cosseno`,
#    cutia_tap_arap_bin1_unif,
#    cutia_tap_arap_bin1_hr$`Sem termo`,
#    cutia_tap_arap_bin1_hn$`Sem termo`,
#    nome_modelos_selecionados = fluxo3.1_selecao_funcao_deteccao_termo_ajuste
#  )
#  
#  # plotar a probabilidade de detecção observada (barras) e a esperada (linhas e pontos)
#  plotar_funcao_deteccao_modelos_selecionados(
#    fluxo3.1_modelos_selecionados
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  # Gráficos de ajuste das funções de deteção às probabilidades de deteção
#  fluxo3.2_modelos_selecionados <- gerar_lista_modelos_selecionados(
#    cutia_tap_arap_bin2_unif,
#    cutia_tap_arap_bin2_hr$`Sem termo`,
#    cutia_tap_arap_bin2_hn$`Cosseno`,
#    cutia_tap_arap_bin2_hn$`Sem termo`,
#    nome_modelos_selecionados = fluxo3.2_selecao_funcao_deteccao_termo_ajuste
#  )
#  
#  # plotar a probabilidade de detecção observada (barras) e a esperada (linhas e pontos)
#  plotar_funcao_deteccao_modelos_selecionados(fluxo3.2_modelos_selecionados)

## ---- eval=FALSE--------------------------------------------------------------
#  # Gráficos de ajuste das funções de deteção às probabilidades de deteção
#  fluxo3.3_modelos_selecionados <- gerar_lista_modelos_selecionados(
#    cutia_tap_arap_bin3_hn$`Cosseno`,
#    cutia_tap_arap_bin3_hr$Cosseno,
#    cutia_tap_arap_bin3_hr$`Polinomial simples`,
#    cutia_tap_arap_bin3_hr$`Sem termo`,
#    cutia_tap_arap_bin3_unif,
#    cutia_tap_arap_bin3_hn$`Sem termo`,
#    nome_modelos_selecionados = fluxo3.3_selecao_funcao_deteccao_termo_ajuste
#  )
#  
#  # plotar a probabilidade de detecção observada (barras) e a esperada (linhas e pontos)
#  plotar_funcao_deteccao_modelos_selecionados(fluxo3.3_modelos_selecionados)

## ---- results='hide'----------------------------------------------------------
#  bondade_ajuste_fluxo3.1 <- testar_bondade_ajuste(fluxo3.1_modelos_selecionados,
#    plot = TRUE,
#    chisq = TRUE,
#    intervalos_distancia =  seq(
#      from = 0,
#      to  = 15,
#      by = 1.5
#    )
#  )
#  
#  bondade_ajuste_fluxo3.1

## ---- eval=FALSE--------------------------------------------------------------
#  bondade_ajuste_fluxo3.2 <- testar_bondade_ajuste(fluxo3.2_modelos_selecionados,
#    plot = TRUE,
#    chisq = TRUE,
#    intervalos_distancia =
#      c(0, seq(1, 15, 1.4))
#    )
#  
#  
#  bondade_ajuste_fluxo3.2

## ---- eval=FALSE--------------------------------------------------------------
#  bondade_ajuste_fluxo3.3 <- testar_bondade_ajuste(fluxo3.3_modelos_selecionados,
#    plot = TRUE,
#    chisq = TRUE,
#    intervalos_distancia =  seq(
#      from = 0,
#      to  = 15,
#      by = 2.5
#    )
#  )
#  
#  bondade_ajuste_fluxo3.3

## ---- eval=FALSE--------------------------------------------------------------
#  # gerar resultados sobre a área, número de detecções, e taxa de encontro
#  fluxo3.1_caracteristicas_area_estudo_taxa_encontro <- fluxo3.1_modelos_selecionados |>
#    gerar_resultados_Distance(
#      resultado_selecao_modelos = fluxo3.1_selecao_funcao_deteccao_termo_ajuste,
#      tipo_de_resultado = "area_estudo"
#    )
#  
#  fluxo3.1_caracteristicas_area_estudo_taxa_encontro

## ---- eval=FALSE--------------------------------------------------------------
#  # gerar resultados sobre a área, número de detecções, e taxa de encontro
#  fluxo3.2_caracteristicas_area_estudo_taxa_encontro <- fluxo3.2_modelos_selecionados |>
#    gerar_resultados_Distance(
#      resultado_selecao_modelos = fluxo3.2_selecao_funcao_deteccao_termo_ajuste,
#      tipo_de_resultado = "area_estudo"
#    )
#  
#  fluxo3.2_caracteristicas_area_estudo_taxa_encontro

## ---- eval=FALSE--------------------------------------------------------------
#  # gerar resultados sobre a área, número de detecções, e taxa de encontro
#  fluxo3.3_caracteristicas_area_estudo_taxa_encontro <- fluxo3.3_modelos_selecionados |>
#    gerar_resultados_Distance(
#      resultado_selecao_modelos = fluxo3.3_selecao_funcao_deteccao_termo_ajuste,
#      tipo_de_resultado = "area_estudo"
#    )
#  
#  fluxo3.3_caracteristicas_area_estudo_taxa_encontro

## ---- eval=FALSE--------------------------------------------------------------
#  # gerar resultados sobre a abundância
#  fluxo3.1_caracteristicas_abundancia <- fluxo3.1_modelos_selecionados |>
#    gerar_resultados_Distance(
#      resultado_selecao_modelos = fluxo3.1_selecao_funcao_deteccao_termo_ajuste,
#      tipo_de_resultado = "abundancia"
#    )
#  
#  fluxo3.1_caracteristicas_abundancia

## ---- eval=FALSE--------------------------------------------------------------
#  # gerar resultados sobre a abundância
#  fluxo3.2_caracteristicas_abundancia <- fluxo3.2_modelos_selecionados |>
#    gerar_resultados_Distance(
#      resultado_selecao_modelos = fluxo3.2_selecao_funcao_deteccao_termo_ajuste,
#      tipo_de_resultado = "abundancia"
#    )
#  
#  fluxo3.2_caracteristicas_abundancia

## ---- eval=FALSE--------------------------------------------------------------
#  # gerar resultados sobre a abundância
#  fluxo3.3_caracteristicas_abundancia <- fluxo3.3_modelos_selecionados |>
#    gerar_resultados_Distance(
#      resultado_selecao_modelos = fluxo3.3_selecao_funcao_deteccao_termo_ajuste,
#      tipo_de_resultado = "abundancia"
#    )
#  
#  fluxo3.3_caracteristicas_abundancia

## ---- eval=FALSE--------------------------------------------------------------
#  # gerar resultados sobre a densidade
#  fluxo3.1_caracteristicas_densidade <- fluxo3.1_modelos_selecionados |>
#    gerar_resultados_Distance(
#      resultado_selecao_modelos = fluxo3.1_selecao_funcao_deteccao_termo_ajuste,
#      tipo_de_resultado = "densidade"
#    )
#  
#  fluxo3.1_caracteristicas_densidade

## ---- eval=FALSE--------------------------------------------------------------
#  # gerar resultados sobre a densidade
#  fluxo3.2_caracteristicas_densidade <- fluxo3.2_modelos_selecionados |>
#    gerar_resultados_Distance(
#      resultado_selecao_modelos = fluxo3.2_selecao_funcao_deteccao_termo_ajuste,
#      tipo_de_resultado = "densidade"
#    )
#  
#  fluxo3.2_caracteristicas_densidade

## ---- eval=FALSE--------------------------------------------------------------
#  # gerar resultados sobre a densidade
#  fluxo3.3_caracteristicas_densidade <- fluxo3.3_modelos_selecionados |>
#    gerar_resultados_Distance(
#      resultado_selecao_modelos = fluxo3.3_selecao_funcao_deteccao_termo_ajuste,
#      tipo_de_resultado = "densidade"
#    )
#  
#  fluxo3.3_caracteristicas_densidade

