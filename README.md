# MASTER
Coletânea de scripts usados para as manipulações e análises dos dados genéticos de uma coorte referentes ao projeto de mestrado. Abaixo estão descritos os script e informações importantes a respeito do mesmo.

- QC_.txt
  * Script para o controle de qualidade (QC) de dados provenientes da genotipagem de amostras biológicas;
  * Baseado no QC realizado pelo Broad Institute;
  * Realizado pela linha de comando (terminal/prompt).
  
- IMPUT_S.txt
  * Script referente a pré e pós imputação dos dados resultantes do QC;
  * Esse script é para iputações de dados feitas em Sanger;
  * Possui QC próprio;
  * É importante salientar que a imputação dos dados só deve ser feita se o chip usado para a genotipagem não for o mais indicado para a sua população;
  * Realizado pela linha de comando (terminal/prompt).
  
 - PRS_.txt
    * PRS = Escore poligênico de risco;
    * Em desenvolvimento!!
    * Realizado pela linha de comando (terminal/prompt).

- PERS_.R
  * PERS = Escore de fatores ambientais de risco;
  * É importante que os dados utilizados para o cálculo (tabela de entrada) estejam de forma binária (SIM/NÂO) ou com os odds ratio respectivos (se for o caso);
  * Antes de subir o arquivo ao R, recomenda-se salvá-lo em formato '.csv';
  * A ordem aqui descrita das váriaveis ambientais segue a ordem desenvolvida no artigo de referência ('The "polyenviromic risk score": Aggregating environmental risk factors predicts conversion to psychosis in familial high-risk subjects').
