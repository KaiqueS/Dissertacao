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
library( haven )

setwd( "G:/Trabalho/Dissertacao/Datasets/Sorteios/" )

municipios_sorteados_0138 <- read.csv( "municipios_sorteados_01_38.csv", sep = "," )
municipios_sorteados_0138 <- municipios_sorteados_0138[ , -c( 1 ) ]

setwd( "G:/Trabalho/Dissertacao/Datasets/TSE" )

# tse_basedosdados <- read.csv( "tse_eleicoes_basedosdados.csv", sep = "," )
# tse_basedosdados <- subset( tse_basedosdados, cargo == "prefeito" )
# tse_basedosdados <- subset( tse_basedosdados, subset = ano == 2000 | ano == 2004 | ano == 2008 | ano == 2012 )

tse_source_00_08 <- read.csv( "eleicoes_00_08.csv", sep = "," )
tse_source_00_08 <- tse_source_00_08[ , -1 ]

setwd( "G:/Trabalho/Dissertacao/Datasets/IBGE/" )

codigos_municipios_ibge <- read_xls( "codigos_municipios.xls" )
codigos_municipios_ibge$`Código Município Completo` <- as.numeric( codigos_municipios_ibge$`Código Município Completo` )

match_codigosmun_tse <- match( codigos_municipios_ibge$`Código Município Completo`, tse_basedosdados$id_municipio )

codigos_municipios_ibge$sigla_uf <- tse_basedosdados[ c( match_codigosmun_tse ), "sigla_uf" ]

municipios_sorteados_0138 <- left_join( municipios_sorteados_0138, codigos_municipios_ibge[ , c( "Código Município Completo", "sigla_uf", "Nome_Município" ) ], by = c( "uf" = "sigla_uf", "municipio" = "Nome_Município" ) )
municipios_sorteados_0138$ano_eleicao <- municipios_sorteados_0138$year - ( municipios_sorteados_0138$year %% 4 )

teste <- left_join( municipios_sorteados_0138, tse_basedosdados[ , c( "id_municipio", "nome_urna", "sigla_partido", "ano" ) ], by = c( "Código Município Completo" = "id_municipio", "ano_eleicao" = "ano" ) )

#sorteio_brollo <- subset( municipios_sorteados_0138, sorteio <= 29 )
#sorteio_ferraz <- subset( municipios_sorteados_0138, sorteio >= 22 & sorteio <= 38 )

# tse_brollo <- subset( tse_basedosdados, cargo == "prefeito" )
# tse_brollo <- subset( tse_brollo, subset = ano == 2000 | ano == 2004 )
# 
# tse_ferraz <- subset( tse_basedosdados, cargo == "prefeito" )
# tse_ferraz <- subset( tse_ferraz, subset = ano == 2004 | ano == 2008 | ano == 2012 )

# NOTA: os dados do TSE da BaseDosDados são horríveis. Tenho que raspar os dados direto do site do TSE.

setwd( "G:/Trabalho/Dissertacao/Datasets/IBGE/ibge_pop_censo_source/" )

ibge_pop_source <- read.csv( "ibge_populacao_censo_2000.csv", sep = "," )

setwd( "G:/Trabalho/Dissertacao/Datasets/IBGE/ibge_source/" )

setwd( "G:/Trabalho/Dissertacao/Datasets/Brollo/" )

small_brollo <- read_dta( "AER_smallsample.dta" )

setwd( "G:/Trabalho/Dissertacao/Datasets/Ferraz/Stata/" )
setwd( "G:/Trabalho/Dissertacao/Datasets/TesouroN/" )

