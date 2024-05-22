Environment:

- Python 3.10.13 with Tensorflow (latest version available for such Python version)
- R 4.4.0
Note: install both Python and R packages on demand, when asked or informed by programs

For all program codes: adjust the working and saving directories accordingly 

Data Files: 
MGH_COVID_OLINK_NPX.csv - the original MGH data provided - place this file inside /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/OlinkAnalyze/extdata

MGH_COVID_Clinical_Info.csv - The clinical info with pre-existing diseases for all patients. Place this file in same directory where the program of the item 1 is located

Organs_Olink_Insight_with_Initials - Protein Uniprot code and descriptions - Place this file in same directory where the program of the item 1 is located



1) Run the following R program, 

- Qual_Tese_1_MGH_Data_Prep_BMI_pre_existing.R - Calculates ANOVA for the COVID-19-positive patients based on Age, BMI and Pre-existing diseases 

2) Run the following Python program to build the MHE data preparation:

Test_LoadKerasModel.py - This program builds the MHE encoded data for the Deep-Learning model training and evaluation

3) Run the GridSearchCV programs to test for the best hyperparameters and weights

GridSearchCV_Deep_Learning_ALL_EarlyStopping_Balanced_Tese.py - GridsearchCV first pass
GridSearchCV_Deep_Learning_ALL_EarlyStopping_Balanced_Tese_1stRefinement.py - GridsearchCV second pass

4) For the best model saved by item 3 (second pass), run the following program to train and evaluate the model with all metrics described in the paper

Test_LoadKerasModel.py - This program trains the model by loading the best model elected by GridsearchCV, and evaluates with all metrics

5) Run the following programs to create the graphics displayed in the paper:

- Jaccard_Results.R - Jaccard Similarity, Hamming Loss and Wasserstein Charts
- Heatmap_Significant_Proteins.R - Heatmap with ANOVA calculations for Patients Cohort
- Patients_Conditions_Heatmap.R - Heatmap with Cohort Characteristics
- JSON_GridSearchCV.R - Graphic with evolution of F1-Macro along each of the 55 k-folds
- Potential_Biomarkers_Organs.R - plots the human tissue groups based on the potential biomarkers found in ML model training and the gganatogram plot


6) CSVs directory - contains the metrics from item 5 and the complete list of potential biomarkers found, according to paper results
