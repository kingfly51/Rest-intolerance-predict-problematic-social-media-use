Study 1: Data Analysis Pipeline
This repository contains the code and data for Study 1, which includes correlation analysis, machine learning modeling (XGBoost and other classifiers), SHAP interpretation, and Latent Profile Analysis (LPA).

File Structure
Code Files
	study1.R
		Main analysis script for:
			Correlation matrix visualization
            		XGBoost model training/validation
            		SHAP analysis (feature importance)
            		LPA result visualization
            		Generates most results and figures reported in the study.
	machine_learning.py
      		 Trains and validates 6 machine learning models (e.g., logistic regression, random forests).
     		 Uses train_set.csv and test_set.csv as input.
	BSMAS_LPA.inp
		Mplus script for Latent Profile Analysis (LPA).
		 Input data: MAS.dat (formatted for Mplus).


Data Files
All datasets are stored in the data/ folder:

File Name	Purpose
data_for_correlation.xlsx        	Correlation matrix heatmap visualization.
data_for_XGBoost.xlsx	        Source data for XGBoost modeling; generates train_set.csv/test_set.csv.
MASLPA4.xlsx	        Data for LPA visualization (e.g., profile plots).
train_set.csv	       Training data for machine learning models (generated from XGBoost preprocessing).
test_set.csv	      Test data for machine learning models.
MAS.dat	        Input data for Mplus LPA analysis (formatted for Mplus).



Usage Instructions
	R Analysis (study1.R)
		Ensure all required R packages  and R4.4 are installed (see study1.R for dependencies).
		Run the script sequentially to reproduce figures/results.
	Python ML Models (machine_learning.py)
		Requires Python 3.12.2 with libraries: sklearn, pandas.
		Input: train_set.csv and test_set.csv.
	Mplus LPA (BSMAS_LPA.inp)
		Run in Mplus 8.3 with MAS.dat as input.