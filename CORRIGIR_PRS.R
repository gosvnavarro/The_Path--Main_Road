#################
# PRS CORRIGIDO #
#################

# O PRS corrigido nada mais e que os resíduos da regressão 
#   que leva em consideracao o prs e os pcs de interesse
PRScorrigido <- lm(prs_pcs$PRS ~ prs_pcs$V3 + prs_pcs$V4 + prs_pcs$V5 + prs_pcs$V6 +
                        prs_pcs$V7 + prs_pcs$V8 + prs_pcs$V9 + prs_pcs$V10 + prs_pcs$V11 + prs_pcs$V12)


PRScorrigido_v2 <- lm(prs_pcs$CAPE ~ prs_pcs$V3 + prs_pcs$V4 + prs_pcs$V5 + prs_pcs$V6 +
                        prs_pcs$V7 + prs_pcs$V8 + prs_pcs$V9 + prs_pcs$V10 + prs_pcs$V11 + prs_pcs$V12)

PRScorrigido_v3 <- lm(prs_pcs$PRS ~ prs_pcs$PRS + prs_pcs$V3 + prs_pcs$V4 + prs_pcs$V5 + prs_pcs$V6 +
                        prs_pcs$V7 + prs_pcs$V8 + prs_pcs$V9 + prs_pcs$V10 + prs_pcs$V11 + prs_pcs$V12)

residuos_prs <- residuals(PRScorrigido)
residuos_prs_v2 <- residuals(PRScorrigido_v2)
residuos_prs_v3 <- residuals(PRScorrigido_v3)

head(residuos_prs)
head(residuos_prs_v2)
head(residuos_prs_v3)

#----
resultado_cor4 <- cor(correlacao_4, method = "spearman")
corrplot.mixed(resultado_cor4, lower.col = "black", number.cex = .7, order = "hclust", addrect = 2)

resultado_cor5 <- cor(correlacao_5, method = "spearman")
corrplot.mixed(resultado_cor5, lower.col = "black", number.cex = .7, order = "hclust", addrect = 2)

resultado_cor6 <- cor(correlacao_6, method = "spearman")
corrplot.mixed(resultado_cor6, lower.col = "black", number.cex = .7, order = "hclust", addrect = 2)