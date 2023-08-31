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
library( ggplot2 )
library( basedosdados )
library( DescTools )
library( purrr )
library( stringi )

#Sys.setlocale( "LC_ALL", "Portuguese_Brazil.1252" )

setwd( "G:/Trabalho/Dissertacao/Datasets/TSE" ) # Desktop
setwd( "/media/kaique/Arquivos/Trabalho/Dissertacao/Datasets/TSE/" ) # Ubuntu
setwd( "E:/Trabalho/Dissertacao/Datasets/TSE/" ) # Notebook

### TSE BASEDOSDADOS ###

# Candidatos
set_billing_id( "graphite-argon-368423" )

candidatos_query <- bdplyr("graphite-argon-368423.tse_candidatos_basedosdados.tse_candidatos_basedosdados")

df_candidatos <- bd_collect( candidatos_query )
df_candidatos <- subset( df_candidatos, cargo == "prefeito" )
df_candidatos <- subset( df_candidatos, subset = ano == 2000 | ano == 2004 | ano == 2008 | ano == 2012 )

colunas_df_candidatos <- c( "id_candidato_bd", "id_municipio", "id_municipio_tse", "sequencial" )
df_candidatos[ colunas_df_candidatos ] <- sapply( df_candidatos[ colunas_df_candidatos ], as.numeric )

# write.csv( df_candidatos, "tse_candidatos_basedosdados.csv" )

# Resultados Eleições Municipais
resultados_query <- bdplyr( "graphite-argon-368423.tse_candidatos_basedosdados.bdd_tse_resultados_candidato_municipio" )

df_resultados <- bd_collect( resultados_query )
df_resultados <- subset( df_resultados, cargo == "prefeito" )
df_resultados <- subset( df_resultados, subset = ano == 2000 | ano == 2004 | ano == 2008 | ano == 2012 )

colunas_df_resultados <- c( "id_candidato_bd", "id_municipio", "id_municipio_tse", "sequencial_candidato" )
df_resultados[ colunas_df_resultados ] <- sapply( df_resultados[ colunas_df_resultados ], as.numeric )

match_candidato_resultado <- match( df_resultados$id_candidato_bd, df_candidatos$id_candidato_bd )

df_resultados[ 'nome' ] <- df_candidatos[ match_candidato_resultado, "nome" ]
df_resultados[ 'nome_urna' ] <- df_candidatos[ match_candidato_resultado, "nome_urna" ]
df_resultados[ 'sequencial_candidato' ] <- df_candidatos[ match_candidato_resultado, "sequencial" ]

df_resultados$sigla_partido <- replace( df_resultados$sigla_partido, df_resultados$sigla_partido == "PPS", "CIDADANIA" )
df_resultados$sigla_partido <- replace( df_resultados$sigla_partido, df_resultados$sigla_partido == "PT do B", "AVANTE" )
df_resultados$sigla_partido <- replace( df_resultados$sigla_partido, df_resultados$sigla_partido == "PSDC", "DC" )
df_resultados$sigla_partido <- replace( df_resultados$sigla_partido, df_resultados$sigla_partido == "PAN", "PTB" )
df_resultados$sigla_partido <- replace( df_resultados$sigla_partido, df_resultados$sigla_partido == "PRP", "PATRIOTA" )
df_resultados$sigla_partido <- replace( df_resultados$sigla_partido, df_resultados$sigla_partido == "PRB", "REPUBLICANOS" )
df_resultados$sigla_partido <- replace( df_resultados$sigla_partido, df_resultados$sigla_partido == "PRN", "PTC" )
df_resultados$sigla_partido <- replace( df_resultados$sigla_partido, df_resultados$sigla_partido == "PFL", "DEM" )
df_resultados$sigla_partido <- replace( df_resultados$sigla_partido, df_resultados$sigla_partido == "PMDB", "MDB" )
df_resultados$sigla_partido <- replace( df_resultados$sigla_partido, df_resultados$sigla_partido %in% c( "DEM", "PSL" ), "UNIÃO" )
df_resultados$sigla_partido <- replace( df_resultados$sigla_partido, df_resultados$sigla_partido %in% c( "PRONA", "PR", "PST", "PGT" ), "PL" )
df_resultados$sigla_partido <- replace( df_resultados$sigla_partido, df_resultados$sigla_partido %in% c( "PSC", "PTN", "PHS" ), "PODE" )

# NOTA: Classificação dos partidos no espectro esquerda-direita de acordo com Bolognesi(2023).
#       Os recortes são: esquerda <= 0-4,49; centro >= 4,5 e <= 5,5; direita >= 5,51
# UPDATE: PP = Progressistas
df_resultados <- df_resultados %>% mutate( Ideologia = case_when( sigla_partido %in% c( "PSTU", "PCO", "PCB", "PSOL", "PC do B", "PT", "PDT", "PSB" ) ~ "Esquerda",
                                                                  sigla_partido %in% c( "CIDADANIA", "PV" ) ~ "Centro",
                                                                  sigla_partido %in% c( "PTB", "AVANTE", "PMN", "MDB", "PSD", "PSDB", "PODE", "PRTB", "PROS",
                                                                                        "PATRIOTA", "REPUBLICANOS", "PL", "PTC", "DC", "PP", "UNIÃO" ) ~ "Direita" ) )

# NOTA: Classificação dos partidos no espectro ext_esquerda-ext_direita de acordo com Bolognesi(2023).
#       Os recortes são: esquerda <= 0-4,49; centro >= 4,5 e <= 5,5; direita >= 5,51
# Data: 08/23
df_resultados <- df_resultados %>% mutate( Ideologia_Ampla = case_when( sigla_partido %in% c( "PSTU", "PCO", "PCB", "PSOL" ) ~ "Ext_Esquerda",
                                                                        sigla_partido %in% c( "PC do B", "PT" ) ~ "Esquerda",
                                                                        sigla_partido %in% c( "PDT", "PSB" ) ~ "Cnt_Esquerda",
                                                                        sigla_partido %in% c( "CIDADANIA", "PV" ) ~ "Centro",
                                                                        sigla_partido %in% c( "PTB", "AVANTE", "PMN" ) ~ "Cnt_Direita",
                                                                        sigla_partido %in% c( "MDB", "PSD", "PSDB", "PODE", "PRTB", "PROS","REPUBLICANOS", "PL", "PTC", "DC", "PP" ) ~ "Direita",
                                                                        sigla_partido %in% c( "PATRIOTA", "UNIÃO" ) ~ "Ext_Direita" ) )

# NOTA: Classificação dos partidos no espectro esquerda-direita de acordo com Tarouco e Madeira(2015).
#       Os recortes são: esquerda < 3,5; centro >= 3,5 e <= 4,5; direita > 4,5
# Data: 08/23
df_resultados <- df_resultados %>% mutate( Ideologia_Tarouco = case_when( sigla_partido %in% c( "PCO", "PSTU", "PSOL", "PCB", "PC do B", "PT", "PSB", "PDT" ) ~ "Esquerda",
                                                                          sigla_partido %in% c( "PV", "CIDADANIA", "MDB", "PMN" ) ~ "Centro",
                                                                          sigla_partido %in% c( "PSDB", "AVANTE", "PTB" ,"PTC", "PODE", "REPUBLICANOS", "PRTB", "DC",
                                                                                                "PL", "PATRIOTA", "PP", "UNIÃO" ) ~ "Direita" ) )

# REMOÇÃO: PPB, o partido não mais existe.
df_resultados <- subset( df_resultados, sigla_partido != "PPB" )

# df_resultados <- read.csv( "tse_resultados_bdd_00_12.csv", sep = "," )

df_resultados <- df_resultados[ !duplicated( df_resultados$nome ), -which( names( df_resultados ) %in% c( "ano", "resultado" ) ) ]

# TODO: criar um banco para cada eleição para depois juntá-los em um único com a época do resultado obtido pelo candidato
resultados_00 <- subset( df_resultados, ano == 2000 )
colnames( resultados_00 )[ colnames( resultados_00 ) == "resultado" ] <- "resultado_00"
match_00 <- match( df_resultados$id_candidato_bd, resultados_00$id_candidato_bd )

resultados_04 <- subset( df_resultados, ano == 2004 )
colnames( resultados_04 )[ colnames( resultados_04 ) == "resultado" ] <- "resultado_04"
match_04 <- match( df_resultados$id_candidato_bd, resultados_04$id_candidato_bd )

resultados_08 <- subset( df_resultados, ano == 2008 )
colnames( resultados_08 )[ colnames( resultados_08 ) == "resultado" ] <- "resultado_08"
match_08 <- match( df_resultados$id_candidato_bd, resultados_08$id_candidato_bd )

resultados_12 <- subset( df_resultados, ano == 2012 )
colnames( resultados_12 )[ colnames( resultados_12 ) == "resultado" ] <- "resultado_12"
match_12 <- match( df_resultados$id_candidato_bd, resultados_12$id_candidato_bd )

df_resultados[ "resultado_00" ] <- resultados_00[ match_00, "resultado_00" ]
df_resultados[ "resultado_04" ] <- resultados_04[ match_04, "resultado_04" ]
df_resultados[ "resultado_08" ] <- resultados_08[ match_08, "resultado_08" ]
df_resultados[ "resultado_12" ] <- resultados_12[ match_12, "resultado_12" ]

# UPDATE: mudei a codificação de 0-1 para "reeleito"-"derrotado"
df_resultados <- df_resultados %>% mutate( reeleicao_04 = case_when( resultado_00 != "nao eleito" & resultado_04 != "nao eleito" ~ "reeleito", resultado_00 != "nao eleito" & resultado_04 == "nao eleito" ~ "derrotado" ),
                                           reeleicao_08 = case_when( resultado_04 != "nao eleito" & resultado_08 != "nao eleito" ~ "reeleito", resultado_04 != "nao eleito" & resultado_08 == "nao eleito" ~ "derrotado" ),
                                           reeleicao_12 = case_when( resultado_08 != "nao eleito" & resultado_12 != "nao eleito" ~ "reeleito", resultado_08 != "nao eleito" & resultado_12 == "nao eleito" ~ "derrotado" ) )

write.csv( df_resultados, "tse_resultados_bdd_00_12.csv" )

### TSE BASEDOSDADOS - FIM ###

### Gráficos
tse_basedosdados <- read.csv( "tse_resultados_bdd_00_12.csv", sep = "," )
tse_basedosdados <- tse_basedosdados[ , -c( 1 ) ]

# Taxa de reeleição por partido - 2004 e 2008
taxa_reel_04 <- tse_basedosdados[ complete.cases( tse_basedosdados$reeleicao_04 ), ]

taxa_reel_04 <- aggregate( reeleicao_04 ~ sigla_partido, data = taxa_reel_04, function( x ) { sum( x == "reeleito" ) / length( taxa_reel_04$reeleicao_04 ) } )

taxa_reel_04 <- ggplot( taxa_reel_04, aes( x = reorder( sigla_partido, reeleicao_04 ), y = reeleicao_04, fill = sigla_partido ) ) +
                        geom_col( ) + 
                        ggtitle( "Taxa de Reeleição 2004 - Partido" ) +
                        theme( plot.title = element_text( hjust = 0.5 ), axis.text.x = element_text( size = 7 ) ) +
                        labs( x = "Partido", y = "Taxa de Reeleição" ) +
                        guides( fill = "none" ) +
                        scale_x_discrete( label = function( x ) abbreviate( x, minlength = 5 ) )

taxa_reel_08 <- tse_basedosdados[ complete.cases( tse_basedosdados$reeleicao_08 ), ]

taxa_reel_08 <- aggregate( reeleicao_08 ~ sigla_partido, data = taxa_reel_08, function( x ) { sum( x == "reeleito" ) / length( taxa_reel_08$reeleicao_08 ) } )

taxa_reel_08 <- ggplot( taxa_reel_08, aes( x = reorder( sigla_partido, reeleicao_08 ), y = reeleicao_08, fill = sigla_partido ) ) +
                        geom_col( ) + 
                        ggtitle( "Taxa de Reeleição 2008 - Partido" ) +
                        theme( plot.title = element_text( hjust = 0.5 ), axis.text.x = element_text( size = 7 ) ) +
                        labs( x = "Partido", y = "Taxa de Reeleição" ) +
                        guides( fill = "none" ) +
                        scale_x_discrete( label = function( x ) abbreviate( x, minlength = 5 ) )

# Figura 2
grid.arrange( taxa_reel_04, taxa_reel_08 )

# Taxa de reeleição por ideologia - 2004 e 2008
taxa_reel_ideo_04 <- tse_basedosdados[ complete.cases( tse_basedosdados$reeleicao_04 ), ]

taxa_reel_ideo_04_base <- aggregate( reeleicao_04 ~ Ideologia, data = taxa_reel_ideo_04, function( x ) { sum( x == "reeleito" ) / length( taxa_reel_ideo_04$reeleicao_04 ) } )
taxa_reel_ideo_04_ampla <- aggregate( reeleicao_04 ~ Ideologia_Ampla, data = taxa_reel_ideo_04, function( x ) { sum( x == "reeleito" ) / length( taxa_reel_ideo_04$reeleicao_04 ) } )
taxa_reel_ideo_04_tar <- aggregate( reeleicao_04 ~ Ideologia_Tarouco, data = taxa_reel_ideo_04, function( x ) { sum( x == "reeleito" ) / length( taxa_reel_ideo_04$reeleicao_04 ) } )

taxa_reel_ideo_04_base <- ggplot( taxa_reel_ideo_04_base, aes( x = reorder( Ideologia, reeleicao_04 ), y = reeleicao_04, fill = Ideologia ) ) +
                                  geom_col( ) + 
                                  ggtitle( "Taxa de Reeleição 2004 - Ideologia" ) +
                                  theme( plot.title = element_text( hjust = 0.5 ) ) +
                                  labs( x = "Ideologia", y = "Taxa de Reeleição" ) +
                                  guides( fill = "none" )

taxa_reel_ideo_04_ampla <- ggplot( taxa_reel_ideo_04_ampla, aes( x = reorder( Ideologia_Ampla, reeleicao_04 ), y = reeleicao_04, fill = Ideologia_Ampla ) ) +
                                   geom_col( ) + 
                                   ggtitle( "Taxa de Reeleição 2004 - Ideologia" ) +
                                   theme( plot.title = element_text( hjust = 0.5 ) ) +
                                   labs( x = "Ideologia", y = "Taxa de Reeleição" ) +
                                   guides( fill = "none" ) +
                                   scale_x_discrete( labels = c( "Centro", "Esquerda", "Centro-Direita", "Centro-Esquerda", "Extrema-Direita", "Direita" ) )

taxa_reel_ideo_04_tar <- ggplot( taxa_reel_ideo_04_tar, aes( x = reorder( Ideologia_Tarouco, reeleicao_04 ), y = reeleicao_04, fill = Ideologia_Tarouco ) ) +
                                 geom_col( ) + 
                                 ggtitle( "Taxa de Reeleição 2004 - Ideologia" ) +
                                 theme( plot.title = element_text( hjust = 0.5 ) ) +
                                 labs( x = "Ideologia", y = "Taxa de Reeleição" ) +
                                 guides( fill = "none" )

taxa_reel_ideo_08 <- tse_basedosdados[ complete.cases( tse_basedosdados$reeleicao_08 ), ]

taxa_reel_ideo_08_base <- aggregate( reeleicao_08 ~ Ideologia, data = taxa_reel_ideo_08, function( x ) { sum( x == "reeleito" ) / length( taxa_reel_ideo_08$reeleicao_08 ) } )
taxa_reel_ideo_08_ampla <- aggregate( reeleicao_08 ~ Ideologia_Ampla, data = taxa_reel_ideo_08, function( x ) { sum( x == "reeleito" ) / length( taxa_reel_ideo_08$reeleicao_08 ) } )
taxa_reel_ideo_08_tar <- aggregate( reeleicao_08 ~ Ideologia_Tarouco, data = taxa_reel_ideo_08, function( x ) { sum( x == "reeleito" ) / length( taxa_reel_ideo_08$reeleicao_08 ) } )

taxa_reel_ideo_08_base <- ggplot( taxa_reel_ideo_08_base, aes( x = reorder( Ideologia, reeleicao_08 ), y = reeleicao_08, fill = Ideologia ) ) +
                                  geom_col( ) + 
                                  ggtitle( "Taxa de Reeleição 2008 - Ideologia" ) +
                                  theme( plot.title = element_text( hjust = 0.5 ) ) +
                                  labs( x = "Ideologia", y = "Taxa de Reeleição" ) +
                                  guides( fill = "none" )

taxa_reel_ideo_08_ampla <- ggplot( taxa_reel_ideo_08_ampla, aes( x = reorder( Ideologia_Ampla, reeleicao_08 ), y = reeleicao_08, fill = Ideologia_Ampla ) ) +
                                   geom_col( ) + 
                                   ggtitle( "Taxa de Reeleição 2008 - Ideologia" ) +
                                   theme( plot.title = element_text( hjust = 0.5 ) ) +
                                   labs( x = "Ideologia", y = "Taxa de Reeleição" ) +
                                   guides( fill = "none" ) +
                                   scale_x_discrete( labels = c( "Centro", "Centro-Direita", "Esquerda", "Centro-Esquerda", "Extrema-Direita", "Direita" ) )

taxa_reel_ideo_08_tar <- ggplot( taxa_reel_ideo_08_tar, aes( x = reorder( Ideologia_Tarouco, reeleicao_08 ), y = reeleicao_08, fill = Ideologia_Tarouco ) ) +
                                 geom_col( ) + 
                                 ggtitle( "Taxa de Reeleição 2008 - Ideologia" ) +
                                 theme( plot.title = element_text( hjust = 0.5 ) ) +
                                 labs( x = "Ideologia", y = "Taxa de Reeleição" ) +
                                 guides( fill = "none" )
# Figura 3
grid.arrange( taxa_reel_ideo_04_base, taxa_reel_ideo_08_base )

# Figura 4
grid.arrange( taxa_reel_ideo_04_ampla, taxa_reel_ideo_08_ampla )

# Figura 5
grid.arrange( taxa_reel_ideo_04_tar, taxa_reel_ideo_08_tar )
