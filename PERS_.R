######################################
########## CALCULO DO PERS ###########
######################################

# Passo 1 - Carregar a tabela e os pacotes para o ambiente
library(readr)
library(dplyr)
library(formattable)
ARQUIVO_CALCULO <- read.delim2(file = "arquivo_respostas.csv", header=FALSE)

# Passo 2 - Caso precise renomear as colunas seguir o passo a seguir, 
#           caso não seja necessário seguir para o passo 3
names(ARQUIVO_CALCULO) <- c('ID', 'varamb_1', 'varamb_2', 'varamb_3', 'varamb_4', 'varamb_5', 
                            'varamb_6', 'varamb_7')

# Passo 3 - Substituir os valores 'NA' por 0
ARQUIVO_CALCULO$varamb_1[which(is.na(ARQUIVO_CALCULO$varamb_1))] <- 0
ARQUIVO_CALCULO$varamb_2[which(is.na(ARQUIVO_CALCULO$varamb_2))] <- 0
ARQUIVO_CALCULO$varamb_3[which(is.na(ARQUIVO_CALCULO$varamb_3))] <- 0
ARQUIVO_CALCULO$varamb_4[which(is.na(ARQUIVO_CALCULO$varamb_4))] <- 0
ARQUIVO_CALCULO$varamb_5[which(is.na(ARQUIVO_CALCULO$varamb_5))] <- 0
ARQUIVO_CALCULO$varamb_6[which(is.na(ARQUIVO_CALCULO$varamb_6))] <- 0
ARQUIVO_CALCULO$varamb_7[which(is.na(ARQUIVO_CALCULO$varamb_7))] <- 0
ARQUIVO_CALCULO$varamb_8[which(is.na(ARQUIVO_CALCULO$varamb_8))] <- 0
ARQUIVO_CALCULO$varamb_9[which(is.na(ARQUIVO_CALCULO$varamb_9))] <- 0

# Passo 4 - A fim de evitar erros no passo referente aos odds ratio,
#           os comandos descritos nesse passo são realizados para que 
#           a substituição das respostas positivas pelos odds ratios respectivos desse certo.
ARQUIVO_CALCULO$varamb_1[ARQUIVO_CALCULO$varamb_1 == 1] <- 2
ARQUIVO_CALCULO$varamb_2[ARQUIVO_CALCULO$varamb_2 == 1] <- 2
ARQUIVO_CALCULO$varamb_3[ARQUIVO_CALCULO$varamb_3 == 1] <- 2
ARQUIVO_CALCULO$varamb_4[ARQUIVO_CALCULO$varamb_4 == 1] <- 2
ARQUIVO_CALCULO$varamb_5[ARQUIVO_CALCULO$varamb_5 == 1] <- 2
ARQUIVO_CALCULO$varamb_6[ARQUIVO_CALCULO$varamb_6 == 1] <- 2
ARQUIVO_CALCULO$varamb_7[ARQUIVO_CALCULO$varamb_7 == 1] <- 2
ARQUIVO_CALCULO$varamb_8[ARQUIVO_CALCULO$varamb_8 == 1] <- 2
ARQUIVO_CALCULO$varamb_9[ARQUIVO_CALCULO$varamb_9 == 1] <- 2

# Passo 5 - Checar o tipo de cada coluna
sapply(ARQUIVO_CALCULO, mode)

# Passo 6 - Estipular o OR de cada variavel ambiental para cada individuo
# Varamb_1 - Season of birth 
#            (fazer esse 'for' caso o 'season of birth' tenha sido obtido em conjunto com as outras variaveis)
for(i in ARQUIVO_CALCULO$varamb_1) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    ARQUIVO_CALCULO$varamb_1[ARQUIVO_CALCULO$varamb_1 == 2] <- 0.068
    print('Valor substituido por 0.068')
  }
}

# Varamb_2 - Urbanicidade
for(i in ARQUIVO_CALCULO$varamb_2) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    ARQUIVO_CALCULO$varamb_2[ARQUIVO_CALCULO$varamb_2 == 2] <- 0.54
    print('Valor substituido por 0.54')
  }
}

# Varamb_3 - Uso de drogas
for(i in ARQUIVO_CALCULO$varamb_3) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    ARQUIVO_CALCULO$varamb_3[ARQUIVO_CALCULO$varamb_3 == 2] <- 0.56
    print('Valor substituido por 0.56')
  }
}

# Varamb_4 - Idade paterna ao nascimento
#            (fazer esse 'for' caso o 'paternal age' tenha sido obtido em conjunto com as outras variaveis)
for(i in ARQUIVO_CALCULO$varamb_4) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    ARQUIVO_CALCULO$varamb_4[ARQUIVO_CALCULO$varamb_4 == 2] <- 0.25/0.80
    print('Valor substituido por 0.25/0.80')
  }
}

# Varamb_5 - Obstetric & Pre-natal complications
for(i in ARQUIVO_CALCULO$varamb_5) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    ARQUIVO_CALCULO$varamb_5[ARQUIVO_CALCULO$varamb_5 == 2] <- 0.69
    print('Valor substituido por 0.69')
  }
}

# Varamb_6 -  Abuso físico
for(i in ARQUIVO_CALCULO$varamb_6) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    ARQUIVO_CALCULO$varamb_6[ARQUIVO_CALCULO$varamb_6 == 2] <- 1.08
    print('Valor substituido por 1.08')
  }
}

# Varamb_7 - Abuso sexual
for(i in ARQUIVO_CALCULO$varamb_7) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    ARQUIVO_CALCULO$varamb_7[ARQUIVO_CALCULO$varamb_7 == 2] <- 0.87
    print('Valor substituido por 0.87')
  }
}

# Varamb_8 - Negligencia
for(i in ARQUIVO_CALCULO$varamb_8) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    ARQUIVO_CALCULO$varamb_8[ARQUIVO_CALCULO$varamb_8 == 2] <- 1.06
    print('Valor substituido por 1.06')
  }
}

# Varamb_9 - Perda/Separação dos pais
for(i in ARQUIVO_CALCULO$varamb_9) {
  if (i == 0){
    print('Valor mantido zero')
  }
  else {
    ARQUIVO_CALCULO$varamb_9[ARQUIVO_CALCULO$varamb_9 == 2] <- 0.53
    print('Valor substituido por 0.53')
  }
}

# Passo 7 - Checar o tipo de cada coluna
sapply(ARQUIVO_CALCULO, mode)

# Passo 8 - Criar uma coluna com a soma de todos os OR, resultando assim no pré-PERS
ARQUIVO_CALCULO$SOMA <- rowSums(cbind(ARQUIVO_CALCULO$varamb_1, ARQUIVO_CALCULO$varamb_2, 
                                      ARQUIVO_CALCULO$varamb_3, ARQUIVO_CALCULO$varamb_4, 
                                      ARQUIVO_CALCULO$varamb_5, ARQUIVO_CALCULO$varamb_6, 
                                      ARQUIVO_CALCULO$varamb_7, ARQUIVO_CALCULO$varamb_8, 
                                      ARQUIVO_CALCULO$varamb_9))

# Passo 9 - Calcular o PERS
ARQUIVO_CALCULO$PERS <- (ARQUIVO_CALCULO$SOMA) / 9

# Passo 10 - Salvar a tabela
write.table(ARQUIVO_CALCULO, file = 'arquivo_pers.xlsx', sep = "\t", quote = FALSE, col.names = TRUE, row.names = FALSE)

