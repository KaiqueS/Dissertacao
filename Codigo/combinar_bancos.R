library("readODS")
library("tidyverse")
library("readxl")
library("foreign")
library( "httr" )
library( "xml2" )
library( "rvest" )
library( ff )
#library( ffbase )
library( dplyr )
library( plyr )
library( basedosdados )
library( DescTools )

setwd( "G:/Trabalho/Dissertacao/Datasets/TSE" )
setwd( "G:/Trabalho/Dissertacao/Datasets/IBGE/" )
setwd( "G:/Trabalho/Dissertacao/Datasets/Ferraz/Stata/" )
setwd( "G:/Trabalho/Dissertacao/Datasets/Sorteios/" )

options( digits = 22 )

banco_ibge_populacao <- read.csv( "ibge_populacao_bdd.csv", sep = "," )
#banco_ibge_populacao <- subset( banco_ibge_populacao, ano == 2000 )
banco_ibge_populacao <- banco_ibge_populacao %>% mutate( ln_pop = log( banco_ibge_populacao$populacao ) )

sprintf( "%.20f", small_banco_brollo[ 837, c( "pop" ) ] )

banco_ibge_populacao_0104 <- subset( banco_ibge_populacao, subset = ano == 2000 | ano == 2001 | ano == 2002 )
banco_ibge_populacao_0508 <- subset( banco_ibge_populacao, subset = ano == 2004 | ano == 2005 | ano == 2006 )

# TODO: média populacional para anos 2001 = 2000, 2001, 2002
#                                    2005 = 2004, 2005, 2006
teste_media <- banco_ibge_populacao %>% 
               group_by( id_municipio ) %>%
               mutate( avg_0104 = case_when( ano >= 2000 & ano <= 2002 ~ mean( banco_ibge_populacao$populacao ) ),
                       avg_0508 = case_when( ano >= 2004 & ano <= 2006 ~ mean( banco_ibge_populacao$populacao ) ) ) 

which( banco_ibge_populacao == 697, arr.ind = TRUE )

# teste_media_0104 <- aggregate( banco_ibge_populacao_0104$populacao, list( banco_ibge_populacao_0104$id_municipio ), mean )
# colnames( teste_media_0104 )[ colnames( teste_media_0104 ) == "Group.1" ] <- "id_municipio"
# colnames( teste_media_0104 )[ colnames( teste_media_0104 ) == "x" ] <- "pop"
# teste_media_0104 <- subset( teste_media_0104, pop >= 6846 & pop <= 50202 )
# 
# teste_media_0508 <- aggregate( banco_ibge_populacao_0508$populacao, list( banco_ibge_populacao_0508$id_municipio ), mean )
# colnames( teste_media_0508 )[ colnames( teste_media_0508 ) == "Group.1" ] <- "id_municipio"
# colnames( teste_media_0508 )[ colnames( teste_media_0508 ) == "x" ] <- "pop"
# teste_media_0508 <- subset( teste_media_0508, pop >= 6846 & pop <= 50202 )

teste_media_0104 <- aggregate( ibge_0104$populacao, list( ibge_0104$id_city_ibge ), mean )
colnames( teste_media_0104 )[ colnames( teste_media_0104 ) == "Group.1" ] <- "id_municipio"
colnames( teste_media_0104 )[ colnames( teste_media_0104 ) == "x" ] <- "pop"
teste_media_0104 <- subset( teste_media_0104, pop >= 6846 & pop <= 50202 )

teste_media_0508 <- aggregate( ibge_0508$populacao, list( ibge_0508$id_city_ibge ), mean )
colnames( teste_media_0508 )[ colnames( teste_media_0508 ) == "Group.1" ] <- "id_municipio"
colnames( teste_media_0508 )[ colnames( teste_media_0508 ) == "x" ] <- "pop"
teste_media_0508 <- subset( teste_media_0508, pop >= 6846 & pop <= 50202 )

teste_media_0108 <- aggregate( ibge_0108$populacao, list( ibge_0108$id_city_ibge ), mean )
colnames( teste_media_0108 )[ colnames( teste_media_0108 ) == "Group.1" ] <- "id_municipio"
colnames( teste_media_0108 )[ colnames( teste_media_0108 ) == "x" ] <- "pop"
teste_media_0108 <- subset( teste_media_0108, pop >= 6846 & pop <= 50202 )

match_avg_pop_brollo_ibge <- match( small_banco_brollo$pop, teste_media_0108$pop )
match_avg_pop_brollo_ibge <- match( teste_media_0108$pop, small_banco_brollo$pop )
match_avg_pop_brollo_ibge <- match_avg_pop_brollo_ibge[ complete.cases( match_avg_pop_brollo_ibge ) ]
  
brollo_ibge_0104 <- left_join( small_brollo_2001, teste_media_0104[ , c( "pop", "id_municipio" ) ], by = "pop" )
brollo_ibge_0508 <- left_join( small_brollo_2005, teste_media_0508[ , c( "pop", "id_municipio" ) ], by = "pop" )

match_brollo_ibge_0108_id_city <- match( small_banco_brollo$pop, ibge_0108$populacao )
match_brollo_ibge_0108_id_city <- match_brollo_ibge_0108_id_city[ complete.cases( match_brollo_ibge_0108_id_city ) ]
match_brollo_ibge_0108 <- match( ibge_0108$populacao, small_banco_brollo$pop )
match_brollo_ibge_0108 <- match_brollo_ibge_0108[ complete.cases( match_brollo_ibge_0108 ) ]

brollo_ibge_0108 <- small_banco_brollo[ match_brollo_ibge_0108, ]

sum( is.na( brollo_ibge_0104$id_municipio ) )
sum( is.na( brollo_ibge_0508$id_municipio ) )
sum( is.na( brollo_ibge_0108$broad ) )
sum( is.na( brollo_ibge_0108$narrow ) )

which( brollo_ibge_0108 == is.na( brollo_ibge_0108 ), arr.ind = TRUE )

tse_basedosdados <- read.csv( "tse_eleicoes_basedosdados.csv", sep = "," )
tse_basedosdados <- subset( tse_basedosdados, cargo == "prefeito" )
tse_basedosdados <- subset( tse_basedosdados, subset = ano == 2004 | ano == 2008 | ano == 2012 )

tse_ibge <- left_join( tse_basedosdados, banco_ibge_populacao[ , c( "id_municipio", "populacao" ) ], by = "id_municipio" )

coluna_teste <- data.frame( log( banco_ibge_populacao$populacao ) )
teste_log_ferraz <- data.frame( log( data_table1$pop ) )