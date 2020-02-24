## Estatistica básica e aplicada, pt.2
## Author: Gabrielle Navarro
## Updated in: 24/02/2020

####################
# COMPARACOES PERS #
####################
# Identificar os individuos que possuem escores diferentes entre as fases
ind_score_d <- subset(comparacao_escores_BF, comparacao_escores_BF$IGUALDADE == 'FALSO')


# Nas tabelas principais retirar os individuos que apresentaram divergencias
differ_w0 <- subset(PERS_W0_V3, PERS_W0_V3$Subject_ID %in% ind_score_d$Subject_ID)
differ_w1 <- subset(PERS_W1_V3, PERS_W1_V3$Subject_ID %in% ind_score_d$Subject_ID)

# Adiciona a identificacao da fase na coluna 1
as.data.frame(differ_w0$Subject_ID <- sub('$', '_W0', differ_w0$Subject_ID)) 
as.data.frame(differ_w1$Subject_ID <- sub('$', '_W1', differ_w1$Subject_ID))

# Merge das tabelas para facilitar as diferencas
PERS_DIVERGENCIAS_V2 <- rbind(differ_w0, differ_w1)

# Salvar a tabela
write.table(PERS_DIVERGENCIAS_V2, file = 'pers_divergencias_v2.ods', sep = '\t', row.names = F)

## Parte 2 - Contagem de individuos de POA e SP
w0_maior_w1 <- subset(pers_divergencias_v2, pers_divergencias_v2$COERENCIA == "Checar!")

differ_poa_pt2 <- nrow(pers_divergencias_v2_pt2[pers_divergencias_v2_pt2$Subject_ID < 12000,])
differ_sp_pt2 <- nrow(pers_divergencias_v2_pt2[pers_divergencias_v2_pt2$Subject_ID > 12000,])

#####################################################
## OBTENÇÃO E CATEGORAZIÇÃO DA ÉPOCA DE NASCIMENTO ##
#####################################################
# Carregar a tabela e os pacotes para o ambiente
library(readr)
season_of_birth <- read_delim("~/Documents/Gabe/project/database_ambiental/season_of_birth.csv", "\t", escape_double = FALSE, trim_ws = TRUE)

# Checar o tipo de cada coluna
sapply(season_of_birth, mode)

# Tirar as informações sobre os dias e anos, ficando apenas com os meses
as.data.frame(season_of_birth$childbirthdate <- sub('.[[/]]*', ' ', season_of_birth$childbirthdate))
as.data.frame(season_of_birth$childbirthdate <- sub('[0-9]*.', ' ', season_of_birth$childbirthdate))
as.data.frame(season_of_birth$childbirthdate <- sub('[[/]]*.', '_', season_of_birth$childbirthdate))
as.data.frame(season_of_birth$childbirthdate <- sub('_[0-9]*.', ' ', season_of_birth$childbirthdate))

# Tornar todas as letras maisculas
as.data.frame(season_of_birth$childbirthdate <- toupper(season_of_birth$childbirthdate))

# Criar os grupos: Janeiro a Maio - 0, Junho a dezembro - 0.068
season_of_birth$logs_ODs <- season_of_birth$childbirthdate

as.data.frame(season_of_birth$logs_ODs <- sub('JAN', 0, season_of_birth$logs_ODs))
as.data.frame(season_of_birth$logs_ODs <- sub('FEB', 0, season_of_birth$logs_ODs))
as.data.frame(season_of_birth$logs_ODs <- sub('MAR', 0, season_of_birth$logs_ODs))
as.data.frame(season_of_birth$logs_ODs <- sub('APR', 0, season_of_birth$logs_ODs))
as.data.frame(season_of_birth$logs_ODs <- sub('MAY', 0, season_of_birth$logs_ODs))
as.data.frame(season_of_birth$logs_ODs <- sub('JUN', 0.068, season_of_birth$logs_ODs))
as.data.frame(season_of_birth$logs_ODs <- sub('JUL', 0.068, season_of_birth$logs_ODs))
as.data.frame(season_of_birth$logs_ODs <- sub('AUG', 0.068, season_of_birth$logs_ODs))
as.data.frame(season_of_birth$logs_ODs <- sub('SEP', 0.068, season_of_birth$logs_ODs))
as.data.frame(season_of_birth$logs_ODs <- sub('OCT', 0.068, season_of_birth$logs_ODs))
as.data.frame(season_of_birth$logs_ODs <- sub('NOV', 0.068, season_of_birth$logs_ODs))
as.data.frame(season_of_birth$logs_ODs <- sub('DEC', 0.068, season_of_birth$logs_ODs))

# Converter as colunas para 'numeric' e faz a checagem o tipo de cada coluna
season_of_birth$logs_ODs <- as.numeric(as.character(season_of_birth$logs_ODs))
sapply(season_of_birth, mode)

# Salvar arquivo
write.table(season_of_birth, file = '~/Documents/Gabe/project/database_ambiental/season_of_birth.csv', sep = "\t", quote = FALSE, col.names = TRUE, row.names = FALSE)
write.table(season_of_birth, file = '~/Documents/Gabe/project/database_ambiental/season_of_birth.xlsx', sep = "\t", quote = FALSE, col.names = TRUE, row.names = FALSE)

################################################
## OBTENCAO E CATEGORAZICAO DA IDADE PATERNA ##
###############################################

##############################
## NO MOMENTO DO NASCIMENTO ##
# Passo 1 - Carregar a tabela e os pacotes para o ambiente
library(readr)
idade_paterna_v2 <- read_delim("idade_paterna_v2.csv", "\t", escape_double = FALSE, trim_ws = TRUE)

# Passo 2 - Checar o tipo de cada coluna (OPCIONAL)
sapply(idade_paterna_v2, mode)

# Passo 3 - Tirar as informações sobre os dias e meses
## pais
as.data.frame(idade_paterna_v2$fbirthdate <- sub('.*[A-Z]', ' ', idade_paterna_v2$fbirthdate))
as.data.frame(idade_paterna_v2$fbirthdate <- sub('.*[a-z]', ' ', idade_paterna_v2$fbirthdate))
as.data.frame(idade_paterna_v2$fbirthdate <- sub('.*[/]', ' ', idade_paterna_v2$fbirthdate))

## filhos
as.data.frame(idade_paterna_v2$childbirthdate <- sub('.*[A-Z]', ' ', idade_paterna_v2$childbirthdate))
as.data.frame(idade_paterna_v2$childbirthdate <- sub('.*[a-z]', ' ', idade_paterna_v2$childbirthdate))
as.data.frame(idade_paterna_v2$childbirthdate <- sub('.*[*]', ' ', idade_paterna_v2$childbirthdate))

# Passo 4 - Converter as colunas para 'numeric e fazer a checagem o tipo de cada coluna
idade_paterna_v2$fbirthdate <- as.numeric(as.character(idade_paterna_v2$fbirthdate))
idade_paterna_v2$childbirthdate <- as.numeric(as.character(idade_paterna_v2$childbirthdate))

sapply(idade_paterna_v2, mode)

# Passo 5 - Cálculo da idade paterna no nascimento
idade_paterna_v2$at_birth <- (idade_paterna_v2$childbirthdate) - (idade_paterna_v2$fbirthdate)

# Passo 6 - Adicionar os odds ratio correspondentes 
idade_paterna_v2$at_birth_odds <- idade_paterna_v2$at_birth

idade_paterna_v2$at_birth_odds[idade_paterna_v2$at_birth_odds < 55] <- 0.25
idade_paterna_v2$at_birth_odds[idade_paterna_v2$at_birth_odds > 55 & idade_paterna_v2$at_birth_odds == 55] <- 0.80

# Passo 7 - Salvar arquivo
write.table(idade_paterna_v2, file = 'idade_paterna_resultados.csv', sep = " ", quote = FALSE, col.names = TRUE, row.names = FALSE)
write.table(idade_paterna_v2, file = 'idade_paterna_resultados.xlsx', sep = "\t", quote = FALSE, col.names = TRUE, row.names = FALSE)

#############################
## NO MOMENTO DA CONCEPCAO ##
# Passo 1 - Carregar a tabela e os pacotes para o ambiente
library(readr)
library(lubridate)
paternal_age_at_conception <- read_delim("paternal_age_at_conception.csv", "\t", escape_double = FALSE, trim_ws = TRUE)

# Passo 2 - Trocar os nomes dos meses pelos numeros correspondentes, caso se faca necessario
#           Isso e importante pois o calculo funciona melhor dessa forma
#           Fazer isso tanto para a data de nascimento dos pais quanto para a data dos filhos
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'JAN'] <- 01
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'FEB'] <- 02
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'MAR'] <- 03
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'APR'] <- 04
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'MAY'] <- 05
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'JUN'] <- 06
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'JUL'] <- 07
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'AUG'] <- 08
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'SEP'] <- 09
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'OCT'] <- 10
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'NOV'] <- 11
paternal_age_at_conception_OF$fbirthdate[paternal_age_at_conception_OF$fbirthdate == 'DEC'] <- 12

# Passo 3 - Passar os anos com dois digitos para quatro digitos
library(magrittr)
# Pais
paternal_age_at_conception_OF$fbirthdate <- 
  as.Date(paternal_age_at_conception_OF$fbirthdate, format = "%d/%m/%y") %>% format("19%y%m%d") %>% as.Date("%Y%m%d")

# Filhos
paternal_age_at_conception_OF$childbirthdate <- 
  as.Date(paternal_age_at_conception_OF$childbirthdate, format = "%d/%m/%y") %>% format("20%y%m%d") %>% as.Date("%Y%m%d")

as.data.frame(paternal_age_at_conception_OF$childbirthdate <- sub('2099', '1999', paternal_age_at_conception_OF$childbirthdate))
as.data.frame(paternal_age_at_conception_OF$childbirthdate <- sub('2098', '1998', paternal_age_at_conception_OF$childbirthdate))
as.data.frame(paternal_age_at_conception_OF$childbirthdate <- sub('2097', '1997', paternal_age_at_conception_OF$childbirthdate))
as.data.frame(paternal_age_at_conception_OF$childbirthdate <- sub('2096', '1996', paternal_age_at_conception_OF$childbirthdate))
as.data.frame(paternal_age_at_conception_OF$childbirthdate <- sub('2095', '1995', paternal_age_at_conception_OF$childbirthdate))

# Passo 4 - Subtrair nove meses antes do nascimento das criancas, para saber o momento da concepcao
library(mondate)
paternal_age_at_conception_OF$concepcao <- mondate(paternal_age_at_conception_OF$childbirthdate) - 9
paternal_age_at_conception_OF$concepcao <- as.Date(paternal_age_at_conception_OF$concepcao)

# Passo 5 - Calcular a idade paterna no momento da concepcao
paternal_age_at_conception_OF$concepcao_idade <- year(strptime(paternal_age_at_conception_OF$concepcao, format = "%Y-%m-%d")) -
  year(strptime(paternal_age_at_conception_OF$fbirthdate, format = "%Y-%m-%d"))

# Passo 6 - Adicionar os odds ratio correspondentes
paternal_age_at_conception_OF$odds_correspondente[paternal_age_at_conception_OF$concepcao_idade < 55] <- 0.25
paternal_age_at_conception_OF$odds_correspondente[paternal_age_at_conception_OF$concepcao_idade > 55] <- 0.80
paternal_age_at_conception_OF$odds_correspondente[paternal_age_at_conception_OF$concepcao_idade == 55] <- 0.80

# Passo 7 - Salvar o arquivo
write.table(paternal_age_at_conception_OF, file = 'idade_paterna_concepcao_resultados.csv', sep = " ", quote = FALSE, col.names = TRUE, row.names = FALSE)

