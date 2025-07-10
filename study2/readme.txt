Study 2: Data Analysis Pipeline

File Structure
Code Files
	study2.R
		Main analysis script containing all data processing and analysis code for:
			T-tests
			Inverse Probability Weighting (IPW) Propensity Score Matching
			Raincloud plots visualization
			Correlation analysis
			Regression analysis
	geom_flat_violin.R
		Custom ggplot2 function for creating flat violin plots (used in visualization)


Data Files
The data/ folder contains:
	study2_usedata.xlsx
		Contains:
			Index IDs
			Socio-demographic information
			Scores for:
				Rest intolerance
				Stress
				Depression
				Objective and subjective measures of social media use



Usage Instructions
	Main Analysis (study2.R)
		Ensure all required R packages are installed (see dependencies below)
		Source the geom_flat_violin.R function when needed for visualization
		Run the script sequentially to reproduce all analyses
	Visualization Function (geom_flat_violin.R)
		This is a helper file that should be sourced by the main analysis script
		Used specifically for creating enhanced violin plots