library( "readODS" )
library( "tidyverse" )
library( "readxl" )
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
library( lfe )
library( fixest )

### TODO: checar se no dataset derivado de Brollo os sorteios são os que ela usa no trabalho dela, i.e., sorteios 1-29
### TODO: talvez usar a pontuação dos partidos em Bolognesi, ao invés de esquerda-centro-direita, só pra ver no que dá

setwd( "G:/Trabalho/Dissertacao/Datasets/Sorteios/" )

municipios_sorteados_0138 <- read.csv( "municipios_sorteados_01_38.csv", sep = "," )
municipios_sorteados_0138 <- municipios_sorteados_0138[ , -c( 1 ) ]

setwd( "G:/Trabalho/Dissertacao/Datasets/TSE" )

tse_basedosdados <- read.csv( "tse_resultados_bdd_00_12.csv", sep = "," )
# tse_basedosdados <- subset( tse_basedosdados, subset = ano == 2000 | ano == 2004 | ano == 2008 | ano == 2012 )

# tse_source_00_08 <- read.csv( "eleicoes_00_08.csv", sep = "," )
# tse_source_00_08 <- tse_source_00_08[ , -1 ]

setwd( "G:/Trabalho/Dissertacao/Datasets/IBGE/" )

codigos_municipios_ibge <- read_xls( "codigos_municipios.xls" )
codigos_municipios_ibge$`Código Município Completo` <- as.numeric( codigos_municipios_ibge$`Código Município Completo` )

match_codigosmun_tse <- match( codigos_municipios_ibge$`Código Município Completo`, tse_basedosdados$id_municipio )

codigos_municipios_ibge$sigla_uf <- tse_basedosdados[ c( match_codigosmun_tse ), "sigla_uf" ]

municipios_sorteados_0138 <- left_join( municipios_sorteados_0138, codigos_municipios_ibge[ , c( "Código Município Completo", "sigla_uf", "Nome_Município" ) ], by = c( "uf" = "sigla_uf", "municipio" = "Nome_Município" ) )

# FIX: exemplo -> candidatos eleitos em 2004 só assumem em 2005, mas aqui aparecem como incumbentes
municipios_sorteados_0138[ 'ano_eleicao' ] <- municipios_sorteados_0138$year - ( municipios_sorteados_0138$year %% 4 )

teste2 <- left_join( municipios_sorteados_0138, tse_basedosdados[ , c( "id_municipio", "nome", "nome_urna", "sigla_partido", "ano", "Ideologia", "sigla_uf", "resultado" ) ], 
                     by = c( "Código Município Completo" = "id_municipio", "ano_eleicao" = "ano", "uf" = "sigla_uf" ) )

sum(is.na(teste$nome_urna))

#sorteio_brollo <- subset( municipios_sorteados_0138, sorteio <= 29 )
#sorteio_ferraz <- subset( municipios_sorteados_0138, sorteio >= 22 & sorteio <= 38 )

# tse_brollo <- subset( tse_basedosdados, cargo == "prefeito" )
# tse_brollo <- subset( tse_brollo, subset = ano == 2000 | ano == 2004 )
# 
# tse_ferraz <- subset( tse_basedosdados, cargo == "prefeito" )
# tse_ferraz <- subset( tse_ferraz, subset = ano == 2004 | ano == 2008 | ano == 2012 )

# NOTA: os dados do TSE da BaseDosDados são horríveis. Tenho que raspar os dados direto do site do TSE.

ibge_pop_bdd <- read.csv( "ibge_populacao_bdd.csv", sep = "," )

# NOTA: Adicionar População ao banco FPM, depois filtrar para populações entre 6800 e 60000
setwd( "G:/Trabalho/Dissertacao/Datasets/TesouroN/" )

fpm <- read.csv( "fpm_2001_2008.csv", sep = ";", fileEncoding = "latin1" )
# teste_fpm <- as.data.frame( gsub( "[^0-9,]", "", fpm$Valor.Consolidado ) )
# teste_fpm$`gsub("[^0-9,]", "", fpm$Valor.Consolidado)` <- parse_number( teste_fpm$`gsub("[^0-9,]", "", fpm$Valor.Consolidado)`, locale = readr::locale( decimal_mark = "," ) )

match_sorteios_fpm <- match( municipios_sorteados_0138$`Código Município Completo`, fpm$Código.IBGE )

municipios_sorteados_0138[ 'codigo_siafi' ] <- fpm[ match_sorteios_fpm, "Código.SIAFI" ]

setwd( "G:/Trabalho/Dissertacao/Datasets/IBGE/ibge_pop_censo_source/" )

ibge_pop_censo <- read.csv( "ibge_populacao_censo_2000.csv", sep = "," )

setwd( "G:/Trabalho/Dissertacao/Datasets/IBGE/ibge_source/" )

ibge_pop <- read.csv( "ibge_0108.csv", sep = "," )

ibge_media_0104 <- subset( ibge_pop, ano <= 2002 )
ibge_media_0104[ 'media_pop' ] <- with( subset( ibge_pop, ano <= 2002 ), ave( populacao, id_city_ibge ) )
ibge_media_0104 <- ibge_media_0104[ !duplicated( ibge_media_0104$id_city_ibge ), ]
ibge_media_0104[ , "ano" ] <- 2001
#ibge_media_0104$populacao <- round( ibge_media_0104$populacao )

ibge_media_0508 <- subset( ibge_pop, ano >= 2004 )
ibge_media_0508[ 'media_pop' ] <- with( subset( ibge_pop, ano >= 2004 ), ave( populacao, id_city_ibge ) )
ibge_media_0508 <- ibge_media_0508[ !duplicated( ibge_media_0508$id_city_ibge ), ]
ibge_media_0508[ , "ano" ] <- 2005

# NOTA: eu tenho acesso aos FPM no banco de dados do Tesouro, posso usar pra dar join ou match
setwd( "G:/Trabalho/Dissertacao/Datasets/Brollo/" )

large_brollo <- read_dta( "AER_largesample.dta" )
small_brollo <- read_dta( "AER_smallsample.dta" )

small_large <- left_join( small_brollo[ , c( "term", "pop", "pop_2", "pop_3", "pop_cat", "fpm", "literacy", "urb", "broad", "narrow", "fraction_narrow", "fraction_broad" ) ],
                          large_brollo[ , c( "term", "pop", "pop_2", "pop_3", "pop_cat", "fpm", "id_city", "uf", "regions" ) ],
                          by = c( "term", "pop", "pop_2", "pop_3", "pop_cat", "fpm" ) )

small_large <- small_large[ complete.cases( small_large ), ]
small_large <- small_large[ !duplicated( small_large ), ]

teste_sorteios_brollo <- left_join( small_large, teste2, by = c( "id_city" = "codigo_siafi" ) )
teste_sorteios_brollo <- teste_sorteios_brollo[ complete.cases( teste_sorteios_brollo ), ] 

model_brollo <- glm( broad ~ Ideologia + fpm + literacy, family = binomial( link = 'logit' ), data = teste_sorteios_brollo )

lm_brollo <- lm( fraction_narrow ~ Ideologia + fpm + literacy, data = teste_sorteios_brollo )

feols_brollo <- feols( fraction_narrow ~ Ideologia + fpm + literacy | municipio, teste_sorteios_brollo )

summary( feols_brollo, vcov_cluster( "municipio" ) )

summary( model_brollo )
summary( lm_brollo )

brollo_2001 <- subset( small_brollo, term == 2001 )
#brollo_2001$pop <- round( brollo_2001$pop )

brollo_2005 <- subset( small_brollo, term == 2005 )

teste <- match( brollo_2001$pop, ibge_pop$populacao )
teste <- teste[ complete.cases( teste ) ]

setwd( "G:/Trabalho/Dissertacao/Datasets/Ferraz/Stata/" )

ferraz_table1 <- read_dta( "table1.dta" )
ferraz_table1[ 'round_lpop' ] <- round( log( ferraz_table1$pop ) )
ferraz_table1$tx_analf18m <- ferraz_table1$tx_analf18m * 100

ferraz_table2 <- read_dta( "table2.dta" )
ferraz_table2[ 'round_lpop' ] <- round( ferraz_table2$lpop )
ferraz_table2[ 'pop' ] <- round( exp( ferraz_table2$lpop ) )

match_teste <- left_join( ferraz_table2, ferraz_table1, by = c( "gini", "tx_analf18m", "no_os", "treatment", "pop" ) )
match_teste <- match_teste[ complete.cases( match_teste ), ]

ibge_pop_ferraz <- match( ferraz_table2$pop, ibge_pop$populacao )
ibge_pop_ferraz <- ibge_pop_ferraz[ complete.cases( ibge_pop_ferraz ) ]

ibge_bdd_ferraz <- match( ferraz_table2$pop, ibge_pop_bdd$populacao )
ibge_bdd_ferraz <- ibge_bdd_ferraz[ complete.cases( ibge_bdd_ferraz ) ]

ibge_censo_ferraz <- match( ferraz_table2$pop, ibge_pop_censo$populacao )
ibge_censo_ferraz <- ibge_censo_ferraz[ complete.cases( ibge_censo_ferraz ) ]

ferraz_sorteios <- left_join( teste2, ibge_pop_bdd[ , c( "id_municipio", "populacao" ) ], by = c( "Código Município Completo" = "id_municipio" ) )

agora_vai <- left_join( ferraz_table2, ferraz_sorteios[ , c( "populacao", "sorteio", "municipio", "uf", "Código Município Completo" , "nome", "sigla_partido", "Ideologia", "resultado" ) ], by = c( "pop" = "populacao", "sorteio" ) )
agora_vai <- agora_vai[ complete.cases( agora_vai ), ]

reg_lin <- lm( lfalha_total ~ Ideologia, data = agora_vai  )
anova_model <- aov( lfalha_total ~ Ideologia, data = agora_vai )

# TODO: fazer gráficos mostrando o número de partidos de esquerda-centro-direita no brasil ao longo dos períodos estudados
#       e colocar no texto da dissertação
# IDEIA: e se eu usar as pontuações dos partidos no espectro esquerda-direita, ao invés de ideologia como categoria?
#        Acho que dá merda. O modelo pode considerar que há uma ordinalidade na variável.

summary( reg_lin )
summary( anova_model )

plot( anova_model )




