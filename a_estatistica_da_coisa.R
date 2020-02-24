## Estatistica básica e aplicada
## Author: Gabrielle Navarro
## Updated in: 24/02/2020

###########
# PACOTES #
###########

library(corrplot)
library(stats)
library(RColorBrewer)
library(Hmisc)
library(fitdistrplus)

#######################
# DESCRICAO DOS DADOS #
#######################
describe(DADOS_W0_SCZ_ancEUR$CAPE)
describe(DADOS_W0_SCZ_ancEUR$PERS)
describe(DADOS_W0_SCZ_ancEUR$PRS_SCZ)

#########################
# GRÁFICOS DE DENSIDADE #
#########################
dens_cape <- density(DADOS_W0_SCZ_ancEUR$CAPE)
plot(dens_cape, main = "Kernel Density of CAPE Score")
polygon(dens_cape, col = "grey", border = "black")

dens_pers <- density(DADOS_W0_SCZ_ancEUR$PERS)
plot(dens_pers, main = "Kernel Density of PERS Score")
polygon(dens_pers, col = "grey", border = "black")

dens_PRS_SCZ <- density(DADOS_W0_SCZ_ancEUR$PRS_SCZ)
plot(dens_PRS_SCZ, main = "Kernel Density of PRS_SCZ Score")
polygon(dens_PRS_SCZ, col = "grey", border = "black")

############################
# GRÁFICOS DE DISTRIBUIÇÃO #
############################
descdist(DADOS_W0_SCZ_ancEUR$CAPE, discrete = FALSE)
descdist(DADOS_W0_SCZ_ancEUR$PERS, discrete = FALSE)
descdist(DADOS_W0_SCZ_ancEUR$PRS_SCZ, discrete = FALSE)

#########################
# NORMALIDADE DOS DADOS #
#########################
shapiro.test(DADOS_W0_SCZ_ancEUR$CAPE)
shapiro.test(DADOS_W0_SCZ_ancEUR$PERS)
shapiro.test(DADOS_W0_SCZ_ancEUR$PRS_SCZ)

###############
# CORRELACOES #
###############
# 1. CAPE x PERS
correlacao_1 <- cor.test(DADOS_W0_SCZ_ancEUR$CAPE, DADOS_W0_SCZ_ancEUR$PERS, method = "spearman")
correlacao_1 # visualizar o resultado
correlacao_1_p <- correlacao_1$p.value # pegar o valor de p da correlacao
correlacao_1_p
correlacao_1_e <- correlacao_1$estimate # pegar o coeficiente de correlacao
correlacao_1_e

# 2. CAPE x PRS_SCZ
correlacao_2 <- cor.test(DADOS_W0_SCZ_ancEUR$CAPE, DADOS_W0_SCZ_ancEUR$PRS_SCZ, method = "spearman")
correlacao_2 # visualizar o resultado
correlacao_2_p <- correlacao_2$p.value # pegar o valor de p da correlacao
correlacao_2_p
correlacao_2_e <- correlacao_2$estimate # pegar o coeficiente de correlacao
correlacao_2_e

# 3. PERS x PRS_SCZ + PCs
correlacao_3 <- cor.test(DADOS_W0_SCZ_ancEUR$PERS, DADOS_W0_SCZ_ancEUR$PRS_SCZ, method = "spearman")
correlacao_3 # visualizar o resultado
correlacao_3_p <- correlacao_3$p.value # pegar o valor de p da correlacao
correlacao_3_p
correlacao_3_e <- correlacao_3$estimate # pegar o coeficiente de correlacao
correlacao_3_e

# 4. TODOS COM TODOS
DADOS_W0_SCZ_ancEUR_SUBSET <- DADOS_W0_SCZ_ancEUR[,c("PRS_SCZ","PC1","PC2","PC3","PC4","PC5","PC6",
                          "PC8","PC9","PC10","PERS","CAPE")]

correlacao_4 <- cor(DADOS_W0_SCZ_ancEUR_SUBSET, method = "spearm", use = "everything")
correlacao_4 # visualizar o resultado

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(correlacao_4, method="color", col=col(200),  
         type="upper", order="alphabet", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

##############
# REGRESSOES #
##############
# 1. CAPE ~ PRS_SCZ + PC_1 + ... + PC_10 + PERS
regressao_pt1 <- lm(DADOS_W0_SCZ_ancEUR$CAPE ~ DADOS_W0_SCZ_ancEUR$PRS_SCZ + DADOS_W0_SCZ_ancEUR$PC1 + 
                       DADOS_W0_SCZ_ancEUR$PC2 + DADOS_W0_SCZ_ancEUR$PC3 + DADOS_W0_SCZ_ancEUR$PC4 + 
                       DADOS_W0_SCZ_ancEUR$PC5 + DADOS_W0_SCZ_ancEUR$PC6 + DADOS_W0_SCZ_ancEUR$PC7 + 
                       DADOS_W0_SCZ_ancEUR$PC8 + DADOS_W0_SCZ_ancEUR$PC9 + DADOS_W0_SCZ_ancEUR$PC10 + 
                       DADOS_W0_SCZ_ancEUR$PERS)
summary(regressao_pt1)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(regressao_pt1) # diagnostic plots

# 2. CAPE ~ PRS_SCZ + PC_1 + ... + PC_10
regressao_pt2 <- lm(DADOS_W0_SCZ_ancEUR$CAPE ~ DADOS_W0_SCZ_ancEUR$PRS_SCZ + DADOS_W0_SCZ_ancEUR$PC1 + 
                          DADOS_W0_SCZ_ancEUR$PC2 + DADOS_W0_SCZ_ancEUR$PC3 + DADOS_W0_SCZ_ancEUR$PC4 + 
                          DADOS_W0_SCZ_ancEUR$PC5 + DADOS_W0_SCZ_ancEUR$PC6 + DADOS_W0_SCZ_ancEUR$PC7 + 
                          DADOS_W0_SCZ_ancEUR$PC8 + DADOS_W0_SCZ_ancEUR$PC9 + DADOS_W0_SCZ_ancEUR$PC10)
summary(regressao_pt2)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(regressao_pt2) # diagnostic plots

# 3. CAPE ~ PERS
regressao_pt3 <- lm(DADOS_W0_SCZ_ancEUR$CAPE ~ DADOS_W0_SCZ_ancEUR$PERS)
summary(regressao_pt3)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(regressao_pt3) # diagnostic plots

# 4. CAPE ~ (PERS * PRS)
regressao_pt4 <- lm(DADOS_W0_SCZ_ancEUR$CAPE ~ (DADOS_W0_SCZ_ancEUR$PERS * DADOS_W0_SCZ_ancEUR$PRS_SCZ))
summary(regressao_pt4)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(regressao_pt4) # diagnostic plots

# 5. CAPE ~ PRS_SCZ + PC_1 + ... + PC_10 + PERS + (PERS * PRS)
regressao_pt5 <- lm(DADOS_W0_SCZ_ancEUR$CAPE ~ DADOS_W0_SCZ_ancEUR$PRS_SCZ + DADOS_W0_SCZ_ancEUR$PC1 + 
                                                   DADOS_W0_SCZ_ancEUR$PC2 + DADOS_W0_SCZ_ancEUR$PC3 + DADOS_W0_SCZ_ancEUR$PC4 + 
                                                   DADOS_W0_SCZ_ancEUR$PC5 + DADOS_W0_SCZ_ancEUR$PC6 + DADOS_W0_SCZ_ancEUR$PC7 + 
                                                   DADOS_W0_SCZ_ancEUR$PC8 + DADOS_W0_SCZ_ancEUR$PC9 + DADOS_W0_SCZ_ancEUR$PC10 + 
                                                   DADOS_W0_SCZ_ancEUR$PERS + (DADOS_W0_SCZ_ancEUR$PERS * DADOS_W0_SCZ_ancEUR$PRS_SCZ))
summary(regressao_pt5)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(regressao_pt5) # diagnostic plots

#############################################################
# PEGAR OS VALORES DE BETA DE UMA REGRESSAO LINEAR MULTIPLA #
#############################################################
# Pelo pacote lm.beta
library(lm.beta)
lm.final.beta <- lm.beta(regressao_w1_corrigida_10pcs)
print(lm.final.beta)
summary(lm.final.beta)
coef(lm.final.beta)

# Pelo pacote QuantPsyc
library(QuantPsyc)
lm.beta(nova_regressao_1704)

