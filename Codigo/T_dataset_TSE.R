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

setwd( "G:/Trabalho/Dissertacao/Datasets/TSE" )
setwd( "/media/kaique/Arquivos/Trabalho/Dissertacao/Datasets/TSE/" )
setwd( "E:/Trabalho/Dissertacao/Datasets/TSE/" )

### TSE SOURCE 2000, 2004, 2008, 2012 ###

lista_eleicoes <- list( "https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2000.zip",
                        "https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2004.zip",
                        "https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2008.zip" )

lapply( lista_eleicoes, function( x ) { download.file( x, basename( x ) )
                                        purrr::walk( basename( x ), ~ unzip( zipfile = basename( x ), 
                                                     exdir = str_c( "", tools::file_path_sans_ext( .x ) ) ) ) } )

# NOTE: os arquivos da eleição de 2012, disponibilizados na seção de resultados da página https://dadosabertos.tse.jus.br/dataset/resultados-2012 estão em formato .txt, além
#       não conterem as colunas bem nomeadas. Portanto, este banco em específico será carregado manualmente. Os dados podem ser obtidos na página alternativa do TSE:
#       https://sig.tse.jus.br/ords/dwapr/r/seai/sig-eleicao-arquivo/confirma%C3%A7%C3%A3o-conjunto-de-dados?session=5536803444904 - com o filtro Cargo = Prefeito.
#

# eleicoes_estados_2012 <- list.files( pattern = ".txt", full.names = FALSE )
# 
# lista_eleicoes12_txt <- lapply( eleicoes_estados_2012, function( x ) read.table( x, sep = ";", fileEncoding = "latin1" ) )
# 
# brasil_2012 <- Reduce( function( x, y ) merge( x, y, all = TRUE ), lista_eleicoes12_txt )
# brasil_2012 <- subset( brasil_2012, V16 == "PREFEITO" )

# lista_pastas <- list.dirs( "./votacao_candidato_munzona_2000", "./votacao_candidato_munzona_2004", "./votacao_candidato_munzona_2008" )
# lista_pastas <- grep( "./votacao_candidato_munzona_", lista_pastas, value = TRUE )

lista_data_eleicoes_00_08 <- list( "./votacao_candidato_munzona_2000/votacao_candidato_munzona_2000_BRASIL.csv",
                                   "./votacao_candidato_munzona_2004/votacao_candidato_munzona_2004_BRASIL.csv",
                                   "./votacao_candidato_munzona_2008/votacao_candidato_munzona_2008_BRASIL.csv" )

datasets_eleicoes_00_08 <- lapply( lista_data_eleicoes_00_08, function( x ) read.csv( x, sep = ";", encoding = "latin1" )  )

eleicoes_00_08 <- Reduce( function( x, y ) merge( x, y, all = TRUE ), datasets_eleicoes_00_08 )
eleicoes_00_08 <- subset( eleicoes_00_08, DS_CARGO == "Prefeito" )

eleicoes_00_08 <- read.csv( "eleicoes_00_08.csv", sep = "," )

# NOTA: PPS -> Cidadania; PFL -> DEM; DEM + PSL -> UNIÃO; PT do B -> Avante; PSDC -> DC(2017);
#       PRONA -> PL; PR -> PL; PST -> PL; PGT -> PL; PAN -> PTB; PSD -> PTB; PRN -> PTC -> AGIR(2022);
#       PRP -> PATRIOTA; PSC -> PODEMOS(2022); PTN -> PODEMOS(2017); PHS - PODEMOS; PMDB -> MDB
#       -----------> PRB -> REPUBLICANOS; PROS -> SOLIDARIEDADE(NÃO HOMOLOGADO); PPB( EXTINTO ) <--------- Não inclusos
eleicoes_00_08$SG_PARTIDO <-  replace( eleicoes_00_08$SG_PARTIDO, eleicoes_00_08$SG_PARTIDO == "PPS", "CIDADANIA" )
eleicoes_00_08$SG_PARTIDO <-  replace( eleicoes_00_08$SG_PARTIDO, eleicoes_00_08$SG_PARTIDO == "PT do B", "AVANTE" )
eleicoes_00_08$SG_PARTIDO <-  replace( eleicoes_00_08$SG_PARTIDO, eleicoes_00_08$SG_PARTIDO == "PSDC", "DC" )
eleicoes_00_08$SG_PARTIDO <-  replace( eleicoes_00_08$SG_PARTIDO, eleicoes_00_08$SG_PARTIDO == "PAN", "PTB" )
eleicoes_00_08$SG_PARTIDO <-  replace( eleicoes_00_08$SG_PARTIDO, eleicoes_00_08$SG_PARTIDO == "PRP", "PATRIOTA" )
eleicoes_00_08$SG_PARTIDO <-  replace( eleicoes_00_08$SG_PARTIDO, eleicoes_00_08$SG_PARTIDO == "PRN", "PTC" )
eleicoes_00_08$SG_PARTIDO <-  replace( eleicoes_00_08$SG_PARTIDO, eleicoes_00_08$SG_PARTIDO == "PFL", "DEM" )
eleicoes_00_08$SG_PARTIDO <-  replace( eleicoes_00_08$SG_PARTIDO, eleicoes_00_08$SG_PARTIDO == "PMDB", "MDB" )
eleicoes_00_08$SG_PARTIDO <-  replace( eleicoes_00_08$SG_PARTIDO, eleicoes_00_08$SG_PARTIDO %in% c( "DEM", "PSL" ), "UNIÃO" )
eleicoes_00_08$SG_PARTIDO <-  replace( eleicoes_00_08$SG_PARTIDO, eleicoes_00_08$SG_PARTIDO %in% c( "PRONA", "PR", "PST", "PGT" ), "PL" )
eleicoes_00_08$SG_PARTIDO <-  replace( eleicoes_00_08$SG_PARTIDO, eleicoes_00_08$SG_PARTIDO %in% c( "PSC", "PTN", "PHS" ), "PODE" )


# Bolognesi(2023)
eleicoes_00_08 <- eleicoes_00_08 %>% mutate( Ideologia = case_when( SG_PARTIDO %in% c( "PSTU", "PCO", "PCB", "PSOL", "PC do B", "PT" ) ~ "Esquerda",
                                                                    SG_PARTIDO %in% c( "PDT", "PSB", "CIDADANIA", "PV", "PTB" ) ~ "Centro",
                                                                    SG_PARTIDO %in% c( "AVANTE", "PMN", "MDB", "PSD", "PSDB", "PODE", "PRTB", "PROS",
                                                                                       "PATRIOTA", "REPUBLICANOS", "PL", "PTC", "DC", "PP", "UNIÃO" ) ~ "Direita" ) )

# FALTANTES: PROS, REPUBLICANOS, PPB
sum( is.na( eleicoes_00_08$Ideologia ) )

# REMOÇÃO: PPB, o partido não mais existe.
eleicoes_00_08 <- subset( eleicoes_00_08, SG_PARTIDO != "PPB" )

# Ta certo isso? Não teve UM PREFEITO ELEITO?
eleicoes_00 <- subset( eleicoes_00_08, ANO_ELEICAO == 2000 )
colnames( eleicoes_00 )[ colnames( eleicoes_00 ) == "DS_SIT_TOT_TURNO" ] = "Resultado_00"
colnames( eleicoes_00 )[ colnames( eleicoes_00 ) == "QT_VOTOS_NOMINAIS" ] = "Votos_Nominais_00"
Votos_Nominais_00 <- aggregate( Votos_Nominais_00 ~ NM_CANDIDATO, data = eleicoes_00, sum )
eleicoes_00 <- eleicoes_00[ !duplicated( eleicoes_00$NM_CANDIDATO ),  ]
eleicoes_00$Votos_Nominais_00 <- Votos_Nominais_00

unique( eleicoes_00_08[ , c( "DS_SIT_TOT_TURNO", "ANO_ELEICAO" = 2000 ) ] )

eleicoes_04 <- subset( eleicoes_00_08, ANO_ELEICAO == 2004 )
colnames( eleicoes_04 )[ colnames( eleicoes_04 ) == "DS_SIT_TOT_TURNO" ] = "Resultado_04"
colnames( eleicoes_04 )[ colnames( eleicoes_04 ) == "QT_VOTOS_NOMINAIS" ] = "Votos_Nominais_04"
Votos_Nominais_04 <- aggregate( Votos_Nominais_04 ~ NM_CANDIDATO, data = eleicoes_04, sum )
eleicoes_04 <- eleicoes_04[ !duplicated( eleicoes_04$NM_CANDIDATO ),  ]
eleicoes_04$Votos_Nominais_04 <- Votos_Nominais_04

eleicoes_08 <- subset( eleicoes_00_08, ANO_ELEICAO == 2008 )
colnames( eleicoes_08 )[ colnames( eleicoes_08 ) == "DS_SIT_TOT_TURNO" ] = "Resultado_08"
colnames( eleicoes_08 )[ colnames( eleicoes_08 ) == "QT_VOTOS_NOMINAIS" ] = "Votos_Nominais_08"
Votos_Nominais_08 <- aggregate( Votos_Nominais_08 ~ NM_CANDIDATO, data = eleicoes_08, sum )
eleicoes_08 <- eleicoes_08[ !duplicated( eleicoes_08$NM_CANDIDATO ),  ]
eleicoes_08$Votos_Nominais_08 <- Votos_Nominais_08

lista_eleicoes_000408 <- list( eleicoes_00, eleicoes_04, eleicoes_08 )

teste <- Reduce( function( x, y ) merge( x, y, by , all = TRUE ), lista_eleicoes_000408 )

# Nova coluna: Reeleição. Se eleito 2016 E eleito 2020 -> Reeleito
# se não, se eleito 2016 E não 2020 -> Derrota_Incumbente
# Usei != "NÃO ELEITO" pois candidatos eleitos no 2do turno são classificados como "2º TURNO"
eleicoes_00_08 <- eleicoes_00_08 %>% mutate( Reeleicao = case_when( Resultado_16 != "NÃO ELEITO" & Resultado_20 != "NÃO ELEITO" ~ "Reeleito",
                                                                    Resultado_16 != "NÃO ELEITO" & Resultado_20 == "NÃO ELEITO" ~ "Derrota_Incumbente" ) )

eleicoes_00_08 <- read.csv( "eleicoes_00_08.csv", sep = "," )
eleicoes_00_08 <- eleicoes_00_08[ , -1 ]



write.csv( eleicoes_00_08, "eleicoes_00_08.csv" )

rm( list = ls( ) )

### TSE SOURCE 2000, 2004, 2008, 2012 - FIM ###

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
df_resultados$sigla_partido <- replace( df_resultados$sigla_partido, df_resultados$sigla_partido == "PRN", "PTC" )
df_resultados$sigla_partido <- replace( df_resultados$sigla_partido, df_resultados$sigla_partido == "PFL", "DEM" )
df_resultados$sigla_partido <- replace( df_resultados$sigla_partido, df_resultados$sigla_partido == "PMDB", "MDB" )
df_resultados$sigla_partido <- replace( df_resultados$sigla_partido, df_resultados$sigla_partido %in% c( "DEM", "PSL" ), "UNIÃO" )
df_resultados$sigla_partido <- replace( df_resultados$sigla_partido, df_resultados$sigla_partido %in% c( "PRONA", "PR", "PST", "PGT" ), "PL" )
df_resultados$sigla_partido <- replace( df_resultados$sigla_partido, df_resultados$sigla_partido %in% c( "PSC", "PTN", "PHS" ), "PODE" )

# NOTA: Classificação dos partidos no espectro esquerda-direita de acordo com Bolognesi(2023).
df_resultados <- df_resultados %>% mutate( Ideologia = case_when( sigla_partido %in% c( "PSTU", "PCO", "PCB", "PSOL", "PC do B", "PT" ) ~ "Esquerda",
                                                                  sigla_partido %in% c( "PDT", "PSB", "CIDADANIA", "PV", "PTB" ) ~ "Centro",
                                                                  sigla_partido %in% c( "AVANTE", "PMN", "MDB", "PSD", "PSDB", "PODE", "PRTB", "PROS",
                                                                                        "PATRIOTA", "REPUBLICANOS", "PL", "PTC", "DC", "PP", "UNIÃO" ) ~ "Direita" ) )

# REMOÇÃO: PPB, o partido não mais existe.
df_resultados <- subset( df_resultados, sigla_partido != "PPB" )

# df_resultados <- read.csv( "tse_resultados_bdd_00_12.csv", sep = "," )

teste <- df_resultados %>% mutate( resultado_00 = case_when( ano == 2000 ~ resultado ),
                                   resultado_04 = case_when( ano == 2004 ~ resultado ),
                                   resultado_08 = case_when( ano == 2008 ~ resultado ),
                                   resultado_12 = case_when( ano == 2012 ~ resultado ) )

# TODO: criar um banco para cada eleição para depois juntá-los em um único com a época do resultado obtido pelo candidato
resultados_00 <- subset( df_resultados, ano == 2000 )
colnames( resultados_00 )[ colnames( resultados_00 ) == "resultado" ] <- "resultado_00"

resultados_04 <- subset( df_resultados, ano == 2004 )
colnames( resultados_04 )[ colnames( resultados_04 ) == "resultado" ] <- "resultado_04"

resultados_08 <- subset( df_resultados, ano == 2008 )
colnames( resultados_08 )[ colnames( resultados_08 ) == "resultado" ] <- "resultado_08"

resultados_12 <- subset( df_resultados, ano == 2012 )
colnames( resultados_12 )[ colnames( resultados_12 ) == "resultado" ] <- "resultado_12"

resultados_0004 <- c( resultados_00, resultados_04 ) %>% 
                      mutate( reeleicao_04 = case_when( ( resultados_00$nome == resultados_04$nome &
                                                          resultados_00$resultado_00 != "nao eleito" &
                                                          resultados_04$resultado_04 != "nao eleito" ) ~ 1,
                                                          TRUE ~ 0 ) )

resultados_0004 <- left_join( resultados_04, resultados_00[ , c( 2:18 ) ], 
                              by = c( colnames( resultados_00[ , 2:18 ] ) ) )

teste <- Reduce( function( x, y ) merge( x, y, by = c( "nome", "id_candidato_bd" ) ), list( resultados_00, resultados_04, resultados_08, resultados_12 ) )

# Não vai dar certo porque os resultados estão em linhas diferentes para cada coluna.
teste <- df_resultados %>% group_by( nome ) %>%  mutate( reeleicao_04 = case_when( resultado_00 != "nao eleito" & resultado_04 != "nao eleito" ~ 1, resultado_00 != "nao eleito" & resultado_04 == "nao eleito" ~ 0 ),
                                                         reeleicao_08 = case_when( resultado_04 != "nao eleito" & resultado_08 != "nao eleito" ~ 1, resultado_04 != "nao eleito" & resultado_08 == "nao eleito" ~ 0 ),
                                                         reeleicao_12 = case_when( resultado_08 != "nao eleito" & resultado_12 != "nao eleito" ~ 1, resultado_08 != "nao eleito" & resultado_12 == "nao eleito" ~ 0 ) )

teste$resultado_00[ 51845 ] == "nao eleito"

# Falta fazer as colunas de reeleição. Para isso: separar os bancos por ano e juntar de dois em dois

write.csv( df_resultados, "tse_resultados_bdd_00_12.csv" )

### TSE BASEDOSDADOS - FIM ###

# Tirar todos que não são prefeitos
# Pegar o banco de dados contendo informações sobre os municípios

# Problema: existem vários nomes repetidos nas bases de 16 e 20. Entretanto, nem todas as colunas
#           são iguais. São entradas diferentes ou tem algum motivo para fazer várias entradas da
#           mesma?

# TODO: Talvez colunas para quantidade de votos em cada eleição? - Feito, mas checar se a totalização de votos está certa

### TSE 2016 - SOURCE ###

# Ler banco, recodificar colunas de resultados e votos nominais, remover não prefeitos
TSE_2016 <- read.csv( "TSE_2016.csv", sep = ";", encoding = "Portuguese_Brazil.1252" )
TSE_2016 <- subset( TSE_2016, DS_CARGO != "Vereador" )
colnames( TSE_2016 )[ colnames( TSE_2016 ) == "DS_SIT_TOT_TURNO" ] = "Resultado_16"
colnames( TSE_2016 )[ colnames( TSE_2016 ) == "QT_VOTOS_NOMINAIS" ] = "Votos_16"

# Identificar duplicatas, somar os votos das mesmas
Soma_votos_16 <- aggregate( Votos_16 ~ NM_CANDIDATO, data = TSE_2016, sum )

match_16 <- match( TSE_2016$NM_CANDIDATO, Soma_votos_16$NM_CANDIDATO )
match_16 <- match_16[ complete.cases( match_16 ) ]
match_16 <- match_16[ !duplicated( match_16 ) ]

# Recolocar a soma dos votos no banco original
TSE_2016 <- TSE_2016[ !duplicated( TSE_2016$NM_CANDIDATO ), ]
TSE_2016$Votos_16 <- Soma_votos_16$Votos_16[ match_16 ]

# TSE 2020 #

# Ler banco, recodificar colunas de resultados e votos nominais, remover não prefeitos
TSE_2020 <- read.csv( "TSE_2020.csv", sep = ";", encoding = "Portuguese_Brazil.1252" )
TSE_2020 <- subset( TSE_2020, DS_CARGO != "Vereador" )
colnames( TSE_2020 )[ colnames( TSE_2020 ) == "DS_SIT_TOT_TURNO" ] = "Resultado_20"
colnames( TSE_2020 )[ colnames( TSE_2020 ) == "QT_VOTOS_NOMINAIS" ] = "Votos_20"
TSE_2020 <- aggregate( Votos_20 ~ NM_CANDIDATO, data = TSE_2020, sum )

# Identificar duplicatas, somar os votos das mesmas
Soma_votos_20 <- aggregate( Votos_20 ~ NM_CANDIDATO, data = TSE_2020, sum )

match_20 <- match( TSE_2020$NM_CANDIDATO, Soma_votos_20$NM_CANDIDATO )
match_20 <- match_20[ complete.cases( match_20 ) ]
match_20 <- match_20[ !duplicated( match_20 ) ]

# Recolocar a soma dos votos no banco original
TSE_2020 <- TSE_2020[ !duplicated( TSE_2020$NM_CANDIDATO ), ]
TSE_2020$Votos_20 <- Soma_votos_20$Votos_20[ match_20 ]

# TSE COMBINADO #

# Mergir bancos de 2016 e 2020 para criar variável de reeleição
# Remover NA e retirar quem não foi eleito em 2016
TSE_Combinado <- merge( x = TSE_2020, y = TSE_2016[ , c("NM_CANDIDATO", "Resultado_16", "Votos_16" )], by = "NM_CANDIDATO", all = TRUE )#[ , c( "NM_CANDIDATO", "Resultado_16", "Resultado_20" ) ]
TSE_Combinado <- TSE_Combinado[ complete.cases( TSE_Combinado[ , c( "Resultado_16", "Resultado_20" ) ] ), ]
TSE_Combinado <- TSE_Combinado[ !duplicated( TSE_Combinado$NM_CANDIDATO ), ]
TSE_Combinado <- subset( TSE_Combinado, Resultado_16 != "NÃO ELEITO" )

#unique( TSE_Combinado$Resultado_16 )

# Nova coluna: Reeleição. Se eleito 2016 E eleito 2020 -> Reeleito
# se não, se eleito 2016 E não 2020 -> Derrota_Incumbente
# Usei != "NÃO ELEITO" pois candidatos eleitos no 2do turno são classificados como "2º TURNO"
TSE_Combinado <- TSE_Combinado %>% mutate( Reeleicao = case_when( Resultado_16 != "NÃO ELEITO" & Resultado_20 != "NÃO ELEITO" ~ "Reeleito",
                                                                  Resultado_16 != "NÃO ELEITO" & Resultado_20 == "NÃO ELEITO" ~ "Derrota_Incumbente" ) )

# Salvar banco tratado
write.csv( TSE_Combinado, "tse_combinado_16-20.csv" )

# TSE + IDEOLOGIA #

banco_tse <- read.csv( "tse_combinado_16-20.csv", sep = "," )

# NOTA: PPS -> Cidadania; PFL -> DEM; DEM + PSL -> UNIÃO; PR -> PL; PRB -> REPUBLICANOS, PROS -> SOLIDARIEDADE( NÃO HOMOLOGADO )
banco_tse$SG_PARTIDO <-  replace( banco_tse$SG_PARTIDO, banco_tse$SG_PARTIDO == "DEM", "UNIÃO" )
banco_tse$SG_PARTIDO <-  replace( banco_tse$SG_PARTIDO, banco_tse$SG_PARTIDO == "PSL", "UNIÃO" )
#banco_tse$SG_PARTIDO <-  replace( banco_tse$SG_PARTIDO, banco_tse$SG_PARTIDO == "PROS", "SOLIDARIEDADE" )

# TODO: Adicionar uma coluna classificando os partidos entre Esquerda e Direita
#       TALVEZ Esquerda-Centro-Direita! Conversar com Nara sobre. Usar Zucco e
#       Power(2015) na classificação. Também usei Bolognesi(2023) -> DONE
# TODO: Ler Zucco e Samuels(2013) sobre Partidarismo! Importante para a dissertação
banco_tse <- banco_tse %>% mutate( Ideologia = case_when( SG_PARTIDO %in% c( "PSOL", "PC do B", "PT" ) ~ "Esquerda",
                                                          SG_PARTIDO %in% c( "PDT", "PSB", "REDE", "CIDADANIA", "PV" ) ~ "Centro",
                                                          SG_PARTIDO %in% c( "PTB", "AVANTE", "PMN", "PMB", "MDB", "PSD", "PSDB", "PODE", "PROS", 
                                                                             "REPUBLICANOS", "PL", "PTC", "PP", "PSC", "UNIÃO", "PATRIOTA" ) ~ "Direita" ) )

# banco_tse <- banco_tse %>% mutate( Ideologia = case_when( SG_PARTIDO == "PSOL" ~ "Esquerda", SG_PARTIDO == "PC do B" ~ "Esquerda", SG_PARTIDO == "PT" ~ "Esquerda",
#                                                           SG_PARTIDO == "PDT" ~ "Centro", SG_PARTIDO == "PSB" ~ "Centro", SG_PARTIDO == "REDE" ~ "Centro", SG_PARTIDO == "CIDADANIA" ~ "Centro", SG_PARTIDO == "PV" ~ "Centro",
#                                                           SG_PARTIDO == "PTB" ~ "Direita", SG_PARTIDO == "AVANTE" ~ "Direita", SG_PARTIDO == "PMN" ~ "Direita", SG_PARTIDO == "PMB" ~ "Direita", SG_PARTIDO == "MDB" ~ "Direita",
#                                                           SG_PARTIDO == "PSD" ~ "Direita", SG_PARTIDO == "PSDB" ~ "Direita", SG_PARTIDO == "PODE" ~ "Direita", SG_PARTIDO == "PROS" ~ "Direita", SG_PARTIDO == "REPUBLICANOS" ~ "Direita",
#                                                           SG_PARTIDO == "PL" ~ "Direita", SG_PARTIDO == "PTC" ~ "Direita", SG_PARTIDO == "PP" ~ "Direita", SG_PARTIDO == "PSC" ~ "Direita", SG_PARTIDO == "UNIÃO" ~ "Direita",
#                                                           SG_PARTIDO == "PATRIOTA" ~ "Direita" ) )

banco_tse <- banco_tse[ complete.cases( banco_tse[ , c( "Ideologia" ) ] ), ]

write.csv( banco_tse, "tse_combinado_16-20_ideologia.csv" )

banco_tse <- read.csv( "tse_combinado_16-20.csv", sep = "," )

# TODO: O banco de Brollo tem 1202 observações. Realizar amostragem estratificada no banco do TSE.
#       Estratificar de acordo com as proporções partidárias. Usar o número de candidatos em cada
#       partido, ao invés do número de candidatos reeleitos

banco_tse[ , c( "SG_PARTIDO", "Ideologia" ) ]
sum( is.na( banco_tse$Ideologia ) )

unique( banco_tse$SG_PARTIDO )

barplot( prop.table( table( banco_tse$SG_PARTIDO ) ) )

# Quantidade de candidatos em cada partido
ggplot( banco_tse, aes( x = SG_PARTIDO ) ) +
        geom_bar( aes( fill = SG_PARTIDO ) ) +
        theme( axis.text.x = element_blank( ) )

# Quantidade de incumbentes derrotados e reeleitos
ggplot( banco_tse, aes( Reeleicao ) ) +
        geom_bar( aes( fill = SG_PARTIDO ), position = "dodge" ) +
        labs( title = "Derrotados E Reeleitos - Partido", x = "Reeleição", y = "Quantidade", fill = "Sigla Partidária" )

ggplot( banco_tse, aes( x = Ideologia ) ) +
        geom_bar( aes( fill = Ideologia ) ) +
        theme( axis.text.x = element_blank( ) )

ggplot( banco_tse, aes( Reeleicao ) ) +
        geom_bar( aes( fill = Ideologia ), position = "dodge" ) +
        labs( title = "Derrotados E Reeleitos - Partido", x = "Reeleição", y = "Quantidade", fill = "Ideologia" )

table( banco_tse$SG_PARTIDO, banco_tse$Reeleicao )

glimpse( banco_tse[ , c( "SG_PARTIDO", "Resultado_16" ) ] )

#length( unique( TSE_2016$NM_CANDIDATO ) )
#length( unique( TSE_2020$NM_CANDIDATO ) )
#length( unique( TSE_Combinado$NM_CANDIDATO ) )
#length( unique( TSE_2016$NM_CANDIDATO ) ) + length( unique( TSE_2020$NM_CANDIDATO ) ) - length( intersect( TSE_2016$NM_CANDIDATO, TSE_2020$NM_CANDIDATO ) )
#length( TSE_Combinado$NM_CANDIDATO ) - sum( is.na( TSE_Combinado$Resultado_16 ) ) - sum( is.na( TSE_Combinado$Resultado_20 ) )


