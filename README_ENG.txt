Collection of scripts used for the manipulation and analysis of genetic data from a cohort related to the master's project. 
Below are described the scripts and important information about it.

## qc.txt
  * Script for the quality control (QC) of data from the genotyping of biological samples;
  * Based on the QC conducted by the Broad Institute;
  * Plink version 2;
  * Performed by the command line (terminal / prompt).
  
#### sanger_imputation.txt
  * Script regarding the pre and post imputation of data resulting from the QC;
  * This script is for data imputations made by Sanger;
  * Own QC;
  * It is important to point out that the imputation of data should only be done if the chip used for genotyping is not the most suitable for its population;
  * Performed by the command line (terminal / prompt).
  
 #### prs_score.txt
  * PRS = Polygenic risk score;
  * PRSice version 2
  * Performed by the command line (terminal / prompt).

#### pers_score.R
  * PERS = Score of environmental risk factors;
  * It is important that the data used for the calculation (input table) are in binary form (YES / NO) or with the respective odds ratios (if applicable);
  * Before uploading the file to R, it is recommended to save it in '.csv' format;
  * The order described here of the environmental variables follows the order developed in the reference article ('The "polyenviromic risk score": Aggregating environmental risk factors predicts conversion to psychosis in familial high-risk subjects', Padmanabhan, JL, et al. 2016) .
  
#### a_statistica_da_coisa / a_estatistica_da_coisa_pt2.R
  * Statistical analysis (correlation, regression, etc.) in R;
  * Before uploading the file to R, it is recommended to save it in '.csv' format;
