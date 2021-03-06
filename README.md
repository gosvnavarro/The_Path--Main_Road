Coletânea de scripts usados para as manipulações e análises dos dados genéticos de uma coorte referentes ao projeto de mestrado. Abaixo estão descritos os script e informações importantes a respeito do mesmo.

#### qc.txt
  * Script para o controle de qualidade (QC) de dados provenientes da genotipagem de amostras biológicas;
  * Baseado no QC realizado pelo Broad Institute;
  * Plink versão 2;
  * Realizado pela linha de comando (terminal/prompt).
  
#### sanger_imputation.txt
  * Script referente a pré e pós imputação dos dados resultantes do QC;
  * Esse script é para imputações de dados feitas por Sanger;
  * Possui QC próprio;
  * É importante salientar que a imputação dos dados só deve ser feita se o chip usado para a genotipagem não for o mais indicado para a sua população;
  * Realizado pela linha de comando (terminal/prompt).
  
 #### prs_&_pcs.txt
  * PRS = Escore poligênico de risco;
  * PRSice versão 2
  * Realizado pela linha de comando (terminal/prompt).

#### pers_score.R
  * PERS = Escore de fatores ambientais de risco;
  * É importante que os dados utilizados para o cálculo (tabela de entrada) estejam de forma binária (SIM/NÃO) ou com os odds ratio respectivos (se for o caso);
  * Antes de subir o arquivo ao R, recomenda-se salvá-lo em formato '.csv';
  * A ordem aqui descrita das váriaveis ambientais segue a ordem desenvolvida no artigo de referência ('The "polyenviromic risk score": Aggregating environmental risk factors predicts conversion to psychosis in familial high-risk subjects', Padmanabhan, J.L., et al. 2016).
  
#### a_estatistica_da_coisa/a_estatistica_da_coisa_pt2.R
  * Análises estatisticas (correlação, regressão, etc.) em R;
  * Antes de subir o arquivo ao R, recomenda-se salvá-lo em formato '.csv';
