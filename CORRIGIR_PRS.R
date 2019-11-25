#################
# PRS CORRIGIDO #
#################

# O PRS corrigido nada mais e que os resíduos de uma regressão linear,
#   este leva em consideracao o PRS (Best_Score) e os PCs (PCA)
PRScorrigido <- lm(ARQUIVO$PRS ~ ARQUIVO$PC_1 + ARQUIVO$PC_2 + ARQUIVO$PC_3 + ARQUIVO$PC_4 +
                        ARQUIVO$PC_5 + ARQUIVO$PC_6 + ARQUIVO$PC_7 + ARQUIVO$PC_8 + ARQUIVO$PC_9 + ARQUIVO$PC_10)


residuos_prs <- residuals(PRScorrigido)
View(residuos_prs)
