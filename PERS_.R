######################################
###### CÁLCULO DO PERS (W0, W1) ######
######################################

# Carregar a tabela e os pacotes para o ambiente
library(readr)
library(dplyr)
library(formattable)
teste_varamb_W0 <- read_csv("~/Documents/Gabe/project/database_ambiental/teste_varamb_W0.csv")

# Renomear as colunas !!CASO PRECISE!!
names(teste_varamb_W0) <- c('ID', 'varamb_2', 'varamb_3', 'varamb_4', 'varamb_5', 'varamb_6', 'varamb_7')

#Substituir os valores NA por 0
varambs_W0_2500$varamb1_B[which(is.na(varambs_W0_2500$varamb1_B))] <- 0
varambs_W0_2500$varamb2_2_B[which(is.na(varambs_W0_2500$varamb2_2_B))] <- 0
varambs_W0_2500$varamb3_B[which(is.na(varambs_W0_2500$varamb3_B))] <- 0
varambs_W0_2500$varamb4_B[which(is.na(varambs_W0_2500$varamb4_B))] <- 0
varambs_W0_2500$varamb5_B[which(is.na(varambs_W0_2500$varamb5_B))] <- 0
varambs_W0_2500$varamb6_B[which(is.na(varambs_W0_2500$varamb6_B))] <- 0
varambs_W0_2500$varamb7_2_B[which(is.na(varambs_W0_2500$varamb7_2_B))] <- 0
varambs_W0_2500$varamb8_B[which(is.na(varambs_W0_2500$varamb8_B))] <- 0
varambs_W0_2500$varamb9_B[which(is.na(varambs_W0_2500$varamb9_B))] <- 0

# Os passos abaixo só foram realizados para que a substituição das respostas positivas pelos odds ratios respectivos desse certo
varambs_W0_2500$varamb1_B[varambs_W0_2500$varamb1_B == 1] <- 2
varambs_W0_2500$varamb2_2_B[varambs_W0_2500$varamb2_2_B == 1] <- 2
varambs_W0_2500$varamb3_B[varambs_W0_2500$varamb3_B == 1] <- 2
varambs_W0_2500$varamb4_B[varambs_W0_2500$varamb4_B == 1] <- 2
varambs_W0_2500$varamb5_B[varambs_W0_2500$varamb5_B == 1] <- 2
varambs_W0_2500$varamb6_B[varambs_W0_2500$varamb6_B == 1] <- 2
varambs_W0_2500$varamb7_2_B[varambs_W0_2500$varamb7_2_B == 1] <- 2

# Checar o tipo de cada coluna
sapply(varambs_W0_2500, mode)

# Estipular o OR de cada variavel ambiental para cada individuo
## para a varamb_1 - Urbanicidade
for(i in varambs_W0_2500$varamb1_B) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    varambs_W0_2500$varamb1_B[varambs_W0_2500$varamb1_B == 2] <- 0.54
    print('Valor substituido por 0.54')
  }
}

## para a varamb_2_2 - Uso de drogas
for(i in varambs_W0_2500$varamb2_2_B) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    varambs_W0_2500$varamb2_2_B[varambs_W0_2500$varamb2_2_B == 2] <- 0.56
    print('Valor substituido por 0.56')
  }
}

## para a varamb_3 - Complicações obstétricas e pré-natais
for(i in varambs_W0_2500$varamb3_B) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    varambs_W0_2500$varamb3_B[varambs_W0_2500$varamb3_B == 2] <- 0.69
    print('Valor substituido por 0.69')
  }
}

## para a varamb_4 - Abuso físico
for(i in varambs_W0_2500$varamb4_B) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    varambs_W0_2500$varamb4_B[varambs_W0_2500$varamb4_B == 2] <- 1.08
    print('Valor substituido por 1.08')
  }
}

## para a varamb_5 - Abuso sexual
for(i in varambs_W0_2500$varamb5_B) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    varambs_W0_2500$varamb5_B[varambs_W0_2500$varamb5_B == 2] <- 0.87
    print('Valor substituido por 0.87')
  }
}

## para a varamb_6 - Negligência
for(i in varambs_W0_2500$varamb6_B) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    varambs_W0_2500$varamb6_B[varambs_W0_2500$varamb6_B == 2] <- 1.06
    print('Valor substituido por 1.06')
  }
}

## para a varamb_7 - Perda e/ou Separação dos Pais
for(i in varambs_W0_2500$varamb7_2_B) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    varambs_W0_2500$varamb7_2_B[varambs_W0_2500$varamb7_2_B == 2] <- 0.53
    print('Valor substituido por 0.53')
  }
}

# Checar o tipo de cada coluna
sapply(varambs_W0_2500, mode)

# Criar uma coluna com a soma de todas varambs, resultando assim no PERS (somar todos os valores e dividir por 6)
varambs_W0_2500$SOMA <- rowSums(cbind(varambs_W0_2500$varamb1_B, varambs_W0_2500$varamb2_2_B, varambs_W0_2500$varamb3_B, 
                                      varambs_W0_2500$varamb4_B, varambs_W0_2500$varamb5_B, varambs_W0_2500$varamb6_B, 
                                      varambs_W0_2500$varamb7_2_B, varambs_W0_2500$varamb8_B, varambs_W0_2500$varamb9_B))

# Calcular o PERS
varambs_W0_2500$PERS <- (varambs_W0_2500$SOMA) / 9
varambs_W0_2500$PERS_P <- scales::percent(varambs_W0_2500$PERS)

# Salvar a tabela
write.table(varambs_W0_2500, file = '~/Documents/Gabe/dados_/_2500/_varambs/PERS_2500_W0_resultados.xlsx', sep = "\t", quote = FALSE, col.names = TRUE, row.names = FALSE)

