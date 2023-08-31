library( "readODS" )
library( "tidyverse" )
library( "readxl" )
library( "foreign" )
library( "httr" )
library( "xml2" )
library( "rvest" )
library( ff )
#library( ffbase )
library( dplyr )
library( plyr )
library( basedosdados )
library( DescTools )
library( haven )
library( lfe )
library( fixest )
library( ggplot2 )
library( gridExtra )
library( sjPlot )
library( sjmisc )
library( sjlabelled )
library( modelsummary )

setwd( "G:/Trabalho/Dissertacao/Datasets/Sorteios/" )
# setwd( "/media/kaique/Arquivos/Trabalho/Dissertacao/Datasets/Sorteios" )

municipios_sorteados_0138 <- read.csv( "municipios_sorteados_01_38.csv", sep = "," )
municipios_sorteados_0138 <- municipios_sorteados_0138[ , -c( 1 ) ]

setwd( "G:/Trabalho/Dissertacao/Datasets/TSE" )
# setwd( "/media/kaique/Arquivos/Trabalho/Dissertacao/Datasets/TSE" )

tse_basedosdados <- read.csv( "tse_resultados_bdd_00_12.csv", sep = "," )
tse_basedosdados <- tse_basedosdados[ , -c( 1 ) ]

# table( banco_tse$SG_PARTIDO, banco_tse$Reeleicao )

setwd( "G:/Trabalho/Dissertacao/Datasets/IBGE/" )

codigos_municipios_ibge <- read_xls( "codigos_municipios.xls" )
codigos_municipios_ibge$`Código Município Completo` <- as.numeric( codigos_municipios_ibge$`Código Município Completo` )

match_codigosmun_tse <- match( codigos_municipios_ibge$`Código Município Completo`, tse_basedosdados$id_municipio )

codigos_municipios_ibge$sigla_uf <- tse_basedosdados[ c( match_codigosmun_tse ), "sigla_uf" ]

municipios_sorteados_0138 <- left_join( municipios_sorteados_0138, codigos_municipios_ibge[ , c( "Código Município Completo", "sigla_uf", "Nome_Município" ) ], by = c( "uf" = "sigla_uf", "municipio" = "Nome_Município" ) )

# FIX: exemplo -> candidatos eleitos em 2004 só assumem em 2005, mas aqui aparecem como incumbentes
municipios_sorteados_0138[ 'ano_eleicao' ] <- municipios_sorteados_0138$year - ( municipios_sorteados_0138$year %% 4 )

municipios_sorteados_0129 <- subset( municipios_sorteados_0138, sorteio <= 29 )

# NOTA: Adicionar População ao banco FPM, depois filtrar para populações entre 6800 e 60000
setwd( "G:/Trabalho/Dissertacao/Datasets/TesouroN/" )

fpm <- read.csv( "fpm_2001_2008.csv", sep = ";", fileEncoding = "latin1" )
# teste_fpm <- as.data.frame( gsub( "[^0-9,]", "", fpm$Valor.Consolidado ) )
# teste_fpm$`gsub("[^0-9,]", "", fpm$Valor.Consolidado)` <- parse_number( teste_fpm$`gsub("[^0-9,]", "", fpm$Valor.Consolidado)`, locale = readr::locale( decimal_mark = "," ) )

match_sorteios_fpm <- match( municipios_sorteados_0138$`Código Município Completo`, fpm$Código.IBGE )
match_sorteios_0129_fpm <- match( municipios_sorteados_0129$`Código Município Completo`, fpm$Código.IBGE )

municipios_sorteados_0138[ 'codigo_siafi' ] <- fpm[ match_sorteios_fpm, "Código.SIAFI" ]
municipios_sorteados_0129[ 'codigo_siafi' ] <- fpm[ match_sorteios_0129_fpm, "Código.SIAFI" ]

# teste2 <- left_join( municipios_sorteados_0138, tse_basedosdados[ , c( "id_municipio", "nome", "nome_urna", "sigla_partido", "Ideologia", "sigla_uf",
#                                                                        "resultado_00", "resultado_04", "resultado_08", "resultado_12", "reeleicao_04",
#                                                                        "reeleicao_08", "reeleicao_12" ) ], 
#                      by = c( "Código Município Completo" = "id_municipio", "uf" = "sigla_uf" ) )

sorteio_0138 <- left_join( municipios_sorteados_0138, tse_basedosdados[ , c( "id_municipio", "sigla_uf", "nome", "nome_urna", "sigla_partido", "Ideologia",
                                                                       "Ideologia_Ampla", "Ideologia_Tarouco" , "resultado_00", "resultado_04", "resultado_08",
                                                                       "reeleicao_04", "reeleicao_08" ) ], 
                           by = c( "Código Município Completo" = "id_municipio", "uf" = "sigla_uf" ) )

sorteio_0129 <- left_join( municipios_sorteados_0129, tse_basedosdados[ , c( "id_municipio", "sigla_uf", "nome", "nome_urna", "sigla_partido", "Ideologia",
                                                                       "Ideologia_Ampla", "Ideologia_Tarouco" , "resultado_00", "resultado_04", "resultado_08",
                                                                       "reeleicao_04", "reeleicao_08" ) ], 
                           by = c( "Código Município Completo" = "id_municipio", "uf" = "sigla_uf" ) )

### ANÁLISE: BROLLO ###

# NOTA: eu tenho acesso aos FPM no banco de dados do Tesouro, posso usar pra dar join ou match
setwd( "G:/Trabalho/Dissertacao/Datasets/Brollo/" )

large_brollo <- read_dta( "AER_largesample.dta" )
small_brollo <- read_dta( "AER_smallsample.dta" )

small_brollo$id_city <- as.numeric( small_brollo$id_city )

# sprintf( "%.25f", small_brollo[ c( 466 ), c( 1, 2, 81:83 ) ] )
# sprintf( "%.25f", small_brollo[ c( 467 ), c( 1, 2, 81:83 ) ] )
# sprintf( "%.25f", small_brollo[ c( 468 ), c( 1, 2, 81:83 ) ] )

# ADD: "rerun" or "reelected"
small_large <- left_join( small_brollo[ , c( "term", "pop", "pop_2", "pop_3", "pop_cat", "fpm", "literacy", "urb", "broad", "narrow", "fraction_narrow", "fraction_broad", "reelected" ) ],
                          large_brollo[ , c( "term", "pop", "pop_2", "pop_3", "pop_cat", "fpm", "id_city", "uf", "regions" ) ],
                          by = c( "term", "pop", "pop_2", "pop_3", "pop_cat", "fpm" ) )

small_large <- small_large[ complete.cases( small_large ), ]
small_large <- small_large[ !duplicated( small_large ), ]

sorteios_brollo <- left_join( small_large, sorteio_0138, by = c( "id_city" = "codigo_siafi" ) )
sorteios_brollo <- subset( sorteios_brollo, sorteio <= 29 )
sorteios_brollo <- sorteios_brollo[ !duplicated( sorteios_brollo ), ]
# sorteios_brollo <- sorteios_brollo[ complete.cases( sorteios_brollo ), ] 

# sorteios_brollo <- sorteios_brollo %>% mutate( reeleicao_04 = case_when( reeleicao_04 == "derrotado" ~ 0, reeleicao_04 == "reeleito" ~ 1 ),
#                                                reeleicao_08 = case_when( reeleicao_08 == "derrotado" ~ 0, reeleicao_08 == "reeleito" ~ 1 ),
#                                                reeleicao_12 = case_when( reeleicao_12 == "derrotado" ~ 0, reeleicao_12 == "reeleito" ~ 1 ) )

# sorteios_brollo_2001 <- subset( sorteios_brollo, term == 2001 )
# sorteios_brollo_2005 <- subset( sorteios_brollo, term == 2005 )

brollo_broad <- glm( reelected ~ ( Ideologia * broad ) + fpm + literacy, family = binomial( link = 'logit' ), data = sorteios_brollo )
brollo_narrow <- glm( reelected ~ ( Ideologia * narrow ) + fpm + literacy, family = binomial( link = 'logit' ), data = sorteios_brollo )
brollo_narrow_ampla <- glm( reelected ~ ( Ideologia_Ampla * narrow ) + fpm + literacy, family = binomial( link = 'logit' ), data = sorteios_brollo )
brollo_narrow_tar <- glm( reelected ~ ( Ideologia_Tarouco * narrow ) + fpm + literacy, family = binomial( link = 'logit' ), data = sorteios_brollo )

brollo_narrow_01 <- glm( reelected ~ ( Ideologia * narrow ) + fpm + literacy, family = binomial( link = 'logit' ), data = sorteios_brollo_2001 )
brollo_narrow_05 <- glm( reelected ~ ( Ideologia * narrow ) + fpm + literacy, family = binomial( link = 'logit' ), data = sorteios_brollo_2005 )

# brollo_broad_04 <- glm( reeleicao_04 ~ ( Ideologia * broad ) + fpm + literacy, family = binomial( link = 'logit' ), data = teste_sorteios_brollo )
# brollo_narrow_04 <- glm( reeleicao_04 ~ ( Ideologia * narrow ) + fpm + literacy, family = binomial( link = 'logit' ), data = teste_sorteios_brollo )
# 
# brollo_broad_08 <- glm( reeleicao_08 ~ ( Ideologia * broad ) + fpm + literacy, family = binomial( link = 'logit' ), data = teste_sorteios_brollo )
# brollo_narrow_08 <- glm( reeleicao_08 ~ ( Ideologia * narrow ) + fpm + literacy, family = binomial( link = 'logit' ), data = teste_sorteios_brollo )

lm_brollo <- lm( fraction_narrow ~ Ideologia + fpm + literacy, data = teste_sorteios_brollo )

## Tabelas de Regressão - Início ##

# tab_model( brollo_narrow,
#            pred.labels = c( "Intercepto", "Direita", "Esquerda", "Corrupção Restrita", "Transf. de Fundos",
#                             "Alfabetização", "Direita * Corrupção", "Esquerda * Corrupção" ),
#            string.pred = "Independentes", string.ci = "Inter. de Conf", string.p = "P-Valor", string.est = "Raz. de Chan",
#            show.r2 = FALSE )
# 
# tab_model( brollo_narrow_tar,
#            pred.labels = c( "Intercepto", "Direita", "Esquerda", "Corrupção Restrita", "Transf. de Fundos",
#                             "Alfabetização", "Direita * Corrupção", "Esquerda * Corrupção" ),
#            string.pred = "Independentes", string.ci = "Inter. de Conf", string.p = "P-Valor", string.est = "Raz. de Chan",
#            show.r2 = FALSE )

modelsummary( list( "Razão de Chances" = brollo_narrow, "Coeficientes" = brollo_narrow ),
              coef_rename = c( "Intercepto", "Direita", "Esquerda", "Corrupção Restrita", "Transf. de Fundos", "Alfabetização", "Direita * Corrupção", "Esquerda * Corrupção" ),
              estimate = "{estimate} [{conf.low}, {conf.high}]", statistic = c( "{p.value}" ), exponentiate = c( TRUE, FALSE ), gof_omit = "AIC|BIC|Log.Lik|F|RMSE",
              shape = term ~ statistic )

modelsummary( list( "Razão de Chances" = brollo_narrow_ampla, "Coeficientes" = brollo_narrow_ampla ),
              coef_rename = c( "Intercepto", "Centro Direita", "Centro Esquerda", "Direita", "Esquerda", "Extrema Direita", "Extrema Esquerda",
                               "Corrupção Restrita", "Transf. de Fundos", "Alfabetização", "Centro Direita * Corrupção", "Centro Esquerda * Corrupção",
                               "Direita * Corrupção", "Esquerda * Corrupção", "Extrema Direita * Corrupção", "Extrema Esquerda * Corrupção" ),
              estimate = "{estimate} [{conf.low}, {conf.high}]", statistic = c( "{p.value}" ), exponentiate = c( TRUE, FALSE ), gof_omit = "AIC|BIC|Log.Lik|F|RMSE",
              shape = term ~ statistic )

modelsummary( list( "Razão de Chances" = brollo_narrow_tar, "Coeficientes" = brollo_narrow_tar ),
              coef_rename = c( "Intercepto", "Direita", "Esquerda", "Corrupção Restrita", "Transf. de Fundos", "Alfabetização", "Direita * Corrupção", "Esquerda * Corrupção" ),
              estimate = "{estimate} [{conf.low}, {conf.high}]", statistic = c( "{p.value}" ), exponentiate = c( TRUE, FALSE ), gof_omit = "AIC|BIC|Log.Lik|F|RMSE",
              shape = term ~ statistic )

## Tabelas de Regressão - FIM ##

### ANÁLISE: BROLLO - FIM ###