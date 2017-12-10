Análise Exploratória Sobre Vinhos Brancos
================

Introdução
==========

Os dados analisados são provenientes do trabalho *Modeling wine preferences by data mining from physicochemical properties*, onde se encontra informações sobre as composições químicas e propriedades físicas da uma amostra de 4898 vinhos. Para cada vinho também é acompanhado uma avaliação sensorial com uma nota de 0 a 10.

Análise Univariada
==================

### Estrutura dos dados

``` r
str(df)
```

    ## Classes 'data.table' and 'data.frame':   4898 obs. of  12 variables:
    ##  $ fixed.acidity       : num  7 6.3 8.1 7.2 7.2 8.1 6.2 7 6.3 8.1 ...
    ##  $ volatile.acidity    : num  0.27 0.3 0.28 0.23 0.23 0.28 0.32 0.27 0.3 0.22 ...
    ##  $ citric.acid         : num  0.36 0.34 0.4 0.32 0.32 0.4 0.16 0.36 0.34 0.43 ...
    ##  $ residual.sugar      : num  20.7 1.6 6.9 8.5 8.5 6.9 7 20.7 1.6 1.5 ...
    ##  $ chlorides           : num  0.045 0.049 0.05 0.058 0.058 0.05 0.045 0.045 0.049 0.044 ...
    ##  $ free.sulfur.dioxide : num  45 14 30 47 47 30 30 45 14 28 ...
    ##  $ total.sulfur.dioxide: num  170 132 97 186 186 97 136 170 132 129 ...
    ##  $ density             : num  1.001 0.994 0.995 0.996 0.996 ...
    ##  $ pH                  : num  3 3.3 3.26 3.19 3.19 3.26 3.18 3 3.3 3.22 ...
    ##  $ sulphates           : num  0.45 0.49 0.44 0.4 0.4 0.44 0.47 0.45 0.49 0.45 ...
    ##  $ alcohol             : num  8.8 9.5 10.1 9.9 9.9 10.1 9.6 8.8 9.5 11 ...
    ##  $ quality             : int  6 6 6 6 6 6 6 6 6 6 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
summary(df)
```

    ##  fixed.acidity    volatile.acidity  citric.acid     residual.sugar  
    ##  Min.   : 3.800   Min.   :0.0800   Min.   :0.0000   Min.   : 0.600  
    ##  1st Qu.: 6.300   1st Qu.:0.2100   1st Qu.:0.2700   1st Qu.: 1.700  
    ##  Median : 6.800   Median :0.2600   Median :0.3200   Median : 5.200  
    ##  Mean   : 6.855   Mean   :0.2782   Mean   :0.3342   Mean   : 6.391  
    ##  3rd Qu.: 7.300   3rd Qu.:0.3200   3rd Qu.:0.3900   3rd Qu.: 9.900  
    ##  Max.   :14.200   Max.   :1.1000   Max.   :1.6600   Max.   :65.800  
    ##    chlorides       free.sulfur.dioxide total.sulfur.dioxide
    ##  Min.   :0.00900   Min.   :  2.00      Min.   :  9.0       
    ##  1st Qu.:0.03600   1st Qu.: 23.00      1st Qu.:108.0       
    ##  Median :0.04300   Median : 34.00      Median :134.0       
    ##  Mean   :0.04577   Mean   : 35.31      Mean   :138.4       
    ##  3rd Qu.:0.05000   3rd Qu.: 46.00      3rd Qu.:167.0       
    ##  Max.   :0.34600   Max.   :289.00      Max.   :440.0       
    ##     density             pH          sulphates         alcohol     
    ##  Min.   :0.9871   Min.   :2.720   Min.   :0.2200   Min.   : 8.00  
    ##  1st Qu.:0.9917   1st Qu.:3.090   1st Qu.:0.4100   1st Qu.: 9.50  
    ##  Median :0.9937   Median :3.180   Median :0.4700   Median :10.40  
    ##  Mean   :0.9940   Mean   :3.188   Mean   :0.4898   Mean   :10.51  
    ##  3rd Qu.:0.9961   3rd Qu.:3.280   3rd Qu.:0.5500   3rd Qu.:11.40  
    ##  Max.   :1.0390   Max.   :3.820   Max.   :1.0800   Max.   :14.20  
    ##     quality     
    ##  Min.   :3.000  
    ##  1st Qu.:5.000  
    ##  Median :6.000  
    ##  Mean   :5.878  
    ##  3rd Qu.:6.000  
    ##  Max.   :9.000

``` r
df_scale <- as.data.frame(lapply(df, scale))
ggplot(stack(df_scale), aes(x = ind, y = values)) + 
    geom_violin() +
    labs(title ="Distribuição das Variáveis de Forma Normalizada", 
         x = "Variáveis", 
         y = "Valor Normalizado") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-3-1.png)

Pelo gráfico da distribuição das variáveis na mesma escaladas podemos observar alguns casos com comportamento que re-assemelham a distribuição normal, como o ácidos fixos do vinho `fixed.acidity`, o pH e os sulfatos `sulphates`. Outra observação que podemos notar é a ocorrência de valores extremos principalmente na densidade `density` e no dióxido de enxofre livre `free.sulfur.dioxide` que precisam ser avaliados individualmente.

A informação mais importante que temos é a avaliação sensorial da qualidade do vinho `quality`.

``` r
ggplot(df, aes(x = quality))  +
    geom_histogram(bins = 7) +
    labs(title ="Histograma das notas obtidas dos vinhos", 
         x = "Notas Obtidas na Qualidade (quality)", 
         y = "Contagem de Ocorrencias")
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
ggplot(df, aes(quality)) + 
    stat_ecdf() + 
    geom_vline(xintercept = quantile(df$quality, 0.05), col = "red", linetype = "dashed") +
    geom_vline(xintercept = quantile(df$quality, 0.95), col = "red", linetype = "dashed") +
    labs(title ="ECDF notas obtidas dos vinhos \ncom limites do intervalo de 90% dos dados", 
         x = "Notas Obtidas na Qualidade (quality)", 
         y = "Frequancia")
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-5-1.png)

Grande parte das notas se concentram principalmente entre as notas de 5 a 7 (contendo mais de 90% dos casos) mesmo sendo a nota em uma escala de 0 a 10. Podemos ainda perceber a ausência de vinhos com notas de 1, 2 e 10.

### Avaliação das propriedades fisicas e quimícas

``` r
ggplot(df, aes(x = pH))  +
    geom_histogram(bins = 40) +
    geom_vline(xintercept = 3, col = "red", linetype = "dashed") +
    labs(title ="Histograma do pH encontrado nos vinhos", 
         x = "pH", 
         y = "Contagem de Ocorrencias")
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-6-1.png)

O pH encontrado possui valores dentro da variabilidade normal dos vinhos (entre 3 e 4), sendo poucos casos com valores abaixo de 3 com uma acidez mais elevada.

``` r
ggplot(df, aes(x = pH))  +
    geom_density(fill = "gray") +
    labs(title ="Frequencia do pH encontrado nos vinhos versus curva normal", 
         x = "pH", 
         y = "Frequencia") +
    stat_function(fun = dnorm, args = list(mean = mean(df$pH), sd = sd(df$pH)), col = "red")
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-7-1.png)

Podemos ter observar a semelhança ao comparar contra a curva normal teórica com os valores amostrais da média e desvio padrão.

``` r
ggplot(df, aes(x = alcohol))  +
    geom_histogram(bins = 35) +
    labs(title ="Histograma da consentração de álcool", 
         x = "Consentração de álcool em %", 
         y = "Contagem de Ocorrencias")
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
ggplot(df, aes(x = chlorides))  +
    geom_histogram(bins = 40) +
    labs(title ="Histograma da consentração de cloretos (sais)", 
         x = "Consentração em g/l", 
         y = "Contagem de Ocorrencias")
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-9-1.png)

A concentração de sais possui uma concentração muito grande de ocorrências no intervalo de 0 a 0.1, assim para poder observar melhor a distribuição da cauda, plotamos novamente com a escala da contagem modificada pelo log.

``` r
ggplot(df, aes(x = chlorides))  +
    geom_histogram(bins = 40) +
    coord_trans(x = 'log10') +
    labs(title ="Histograma da consentração de cloretos (sais)", 
         x = "Consentração em g/l", 
         y = "Contagem de Ocorrencias")
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
ggplot(df, aes(x = residual.sugar))  +
    geom_histogram(bins = 100) +
    labs(title ="Histograma da consentração de açucar residual", 
         x = "Açucar Residual (g/l)", 
         y = "Contagem de Ocorrencias")
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-11-1.png)

Na concentração de açucares, como nas concentrações de sais, uma concentração dos valores, principalmente abaixo de 10g/l, porém deviso a escala não conseguimos observar quantos vinhos possuem extremos.

``` r
ggplot(df, aes(x = residual.sugar))  +
    geom_histogram(bins = 50) +
    scale_x_log10() +
    labs(title ="Histograma da consentração de açucar residual", 
         x = "Açucar Residual log (g/l)", 
         y = "Contagem de Ocorrencias")
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-12-1.png)

Novamente alterando a escala da contagem para log, podemos observar a ocorrência de um vinho com uma quantidade extremamente alta em contraste aos demais. A distribuição também apresenta aspecto bi-modal que pode ser investigado uma relação com outras variáveis.

Observando uma a referencia do [Wikipédia](https://en.wikipedia.org/wiki/Sweetness_of_wine) podemos observar que essa ocorrência que observamos é a unica onde possui a classificação de vinho doce (&gt;40g/l).

``` r
# update of df to remove the sweet wine
df <- df[residual.sugar < 45]

sweetness_ranges <- unique(c(-Inf, 4, 12, Inf))
sweetness_labels <- c("Dry", "Medium dry", "Medium")
sweetness_class <- cut(df$residual.sugar, breaks = sweetness_ranges, labels = sweetness_labels)
df <- cbind(df, sweetness_class)
```

Criamos o agrupamento dos tipos de vinho referente a quantidade de açúcar residual.

``` r
ggplot(df, aes(x = sweetness_class, y = residual.sugar)) +
    geom_jitter(alpha = 0.1, size = 0.9) +
    geom_boxplot(alpha = 0.1, col = "red") +
    labs(title ="BoxPlot da consentração de açucar residual por categoria", 
         x = "Classificação do vinho", 
         y = "Açucar Residual (g/l)")
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
ggplot(df, aes(x = residual.sugar, color = as.factor(sweetness_class))) +
    geom_density(alpha = 0.1, size = 1) +
    labs(title ="Gráfico de densidade da consentração de açucar residual por categoria", 
         x = "Açucar Residual (g/l)", 
         y = "% de Ocorrência",
         color = 'Classificação do vinho')
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-15-1.png)

Podemos observar que os vinhos secos possuem uma dispersão muito menor em relação aos demais.

Análise Bivariada
=================

``` r
chart.Correlation(select(df, -sweetness_class))
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-16-1.png)

``` r
ggplot(df, aes(y = alcohol, x = as.factor(quality), fill = as.factor(quality))) + 
    geom_boxplot() +
    labs(title ="Boxplot da Distribuição da consentração de alcool por qualidade", 
         x = "Qualidade", 
         y = "Consentração de alcool (%)") +
    theme(legend.position="none")
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-17-1.png)

Iniciando pela

``` r
ggplot(df, aes(x = residual.sugar, color = as.factor(quality)))  +
    geom_density() +
    scale_x_log10() +
    scale_colour_brewer(palette = 'Spectral') +
    labs(title ="", 
         x = "Açucar Residual log (g/l)", 
         y = "Frequência")
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-18-1.png)

``` r
ggplot(df, aes(x = residual.sugar, y = density)) + 
    geom_jitter(alpha = 0.1) +
    geom_smooth(method = 'lm')
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-19-1.png)

Análise Multivariada
====================

``` r
colors_spectral <- rev(brewer.pal(11, 'Spectral'))
ggplot(df, aes(x = residual.sugar, y = density, color = quality)) + 
    geom_jitter(alpha = 0.3, size = 1.5) +
    scale_colour_gradientn(colours = colors_spectral) +
    geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, linetype = 'dashed')
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-20-1.png)

``` r
ggplot(df, aes(x = quality, y = residual.sugar, color = density)) + 
    geom_jitter(alpha = 0.8) +
    scale_colour_gradientn(colours = colors_spectral)
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-21-1.png)

``` r
ggplot(df, aes(x = quality, y = chlorides, color = alcohol)) + 
    geom_jitter(alpha = 0.8) +
    scale_colour_gradientn(colours = colors_spectral)
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-22-1.png)

``` r
library(glmnet)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     expand

    ## Loading required package: foreach

    ## Loaded glmnet 2.0-13

``` r
cv_fit_lasso <- cv.glmnet(as.matrix(select(df, -quality, -sweetness_class)),
                          as.matrix(select(df, quality)))
cv_fit_coef <- coef(cv_fit_lasso, s = "lambda.1se")

cv_fit_coef_df <- as.data.frame(as.matrix(cv_fit_coef)) %>%
    rename(lasso = '1') %>%
    rownames_to_column(var = 'variable')

ggplot(filter(cv_fit_coef_df, variable != '(Intercept)'), aes(x = variable, y = lasso)) +
    geom_point() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title ="Regressão Lasso", 
         x = "Variáveis", 
         y = "Lasso")
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-23-1.png)

Incluímos uma análise de regressão por Lasso para ajudar a identificar variáveis que podem agregar mais no momento de determinar um modelo para os dados. Podemos ver um destaque para a densidade, algo que foi observado anteriormente em conjunto com o açúcar residual.

Gráficos Finais e Sumário
=========================

Nos últimos gráficos vamos explorar mais como as características se distinguem dentro dos grupos de vinhos `Dry`, `Medium dry` e `Medium`.

``` r
ggplot(df, aes(x = alcohol, y = density, col = sweetness_class)) +
    geom_jitter(alpha = 0.3) +
    stat_smooth(method = 'lm')
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-24-1.png)

É interessante observar como as relações entre álcool e densidade se distinguem quando observados entre os grupos de vinhos.

``` r
ggplot(df, aes(x = free.sulfur.dioxide, y = total.sulfur.dioxide, col = sweetness_class)) +
    scale_x_log10() +
    geom_jitter() +
    stat_smooth(method = 'lm')
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-25-1.png)

Para a relação entre o total de dióxido de enxofre e a quantidade livre, não é possível observar uma boa distinção dos grupos, porem pelas regressões lineares, podemos observar um alinhamento maior entre os grupos `Meidum` e `Medium dry`.

``` r
ggplot(df, aes(x = volatile.acidity, y = fixed.acidity, col = sweetness_class)) +
    geom_jitter(alpha = 0.6) 
```

![](final_prject_files/figure-markdown_github/unnamed-chunk-26-1.png)

Reflexão
========

Referências
-----------

P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. Modeling wine preferences by data mining from physicochemical properties. In Decision Support Systems, Elsevier, 47(4):547-553. ISSN: 0167-9236.

Disponível: [Elsevier](http://dx.doi.org/10.1016/j.dss.2009.05.016) [Pre-press pdf](http://www3.dsi.uminho.pt/pcortez/winequality09.pdf) [bib](http://www3.dsi.uminho.pt/pcortez/dss09.bib)
