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

### TSE SOURCE 2000, 2004, 2008, 2012 ###

lista_eleicoes <- list( "https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2000.zip",
                        "https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2004.zip",
                        "https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2008.zip" )

lapply( lista_eleicoes, function( x ) { download.file( x, basename( x ) ) purrr::walk( basename( x ), ~ unzip( zipfile = basename( x ), 
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