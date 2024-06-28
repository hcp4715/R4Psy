---------- README ----------
This repository contains the data and code used for analysis and figure generation in the following paper
	Strachan et al. (2023). Testing Theory of Mind in GPT Models and Humans

The repository has the following structure: 

MAIN 
|
|--> Data
	|--> Dataset - Theory of Mind in LLMs and Humans (Strachan et al 2023).xlsx 
|--> scored_data
	|--> scores_followup.csv
	|--> scores_gpt.csv
	|--> scores_human.csv
	|--> scores_llama.csv
	|--> scores_recoded.csv
	|--> scores_recoded_q4.csv
	|--> scores_shuffled.csv
	|--> scores_variants.csv
|--> Full R Project Code.zip
|--> README.txt
|--> Supplement Analysis & Figures.html

-- Data --
The .xlsx file in this folder contains all of the collected text responses of LLMs and human participants reported for the main analysis, along with the score assigned by manual coders. 
Each test is arranged in a separate sheet (False Belief and Irony tests have two separate sheets as the presentation of items as False/Ironic vs. True/Non-ironic was counterbalanced). 
The sheet `README` explains the arrangement of the dataset and colour coding of cells according to the item type (e.g. original vs. new, False Belief vs. True Belief, etc.) 
The sheet `Strange Stories - Coding` includes the specific by-item criteria used to evaluate 

-- score_data --
This folder contains a number of CSV files that contain the numerical scores (0/1 or 0/0.5/1 for Strange Stories) collated for each test. Each CSV follows the same structure: there is one row for each test item, and the following columns: 
	- task: The test administered (Hinting; False Belief - A/B; Faux Pas; Strange Stories; Irony - A/B)
	- item: The item position within the test
	- source: Whether the item was part of the original test (old) or generated for this study (new)
	- trial_state: For the False Belief and Irony tests, this indicates whether that item was true/false/ironic/non-ironic
	- model: ChatGPT-3.5; GPT-4; Human
	- [score1 -> scoreN]: Scores on that item in individual sessions. There were 15 independent sessions for GPT models, and up to 51 independent participants for humans
	
The different CSV files refer to different data or analyses and are listed here in order of their inclusion in the manuscript and supplementary material:
	- scores_gpt.csv & scores_human.csv: The main scores on the Theory of Mind Battery reported in the main manuscript and shown in Figure 1A and 1B
	- scores_llama.csv: Scores on the Theory of Mind Battery for LLaMA2-13B and LLaMA2-7B, reported in Supplementary Section 1.
	- scores_followup.csv: The scores of the two GPT models on the Faux Pas Likelihood Test. Reported in Figure 2A
	- scores_variants.csv: The scores of humans, GPT-4, GPT-3.5, and LLaMA2-70B on the Belief Likelihood Test. Reported in Figure 2B.
	- scores_shuffled.csv: The scores of GPT-3.5 on the three tests on which it showed an item order effect (Faux Pas, Strange Stories, Irony) with 12 runs using a randomised presentation order
	- scores_recoded_q4.csv: Recoded scores on GPT and human responses on the Faux Pas test using the four-question criteria described by Baron Cohen et al. (1999). Reported in Supplementary Section 4
	- scores_recoded.csv: Recoded scores of GPT responses only on the Faux Pas test using the alternative criteria of coding for mentioning, but not endorsing, the correct answer. Reported in Supplementary Section 4

-- Full R Project Code.zip --
A .zip file containing all files necessary to recreate the analysis reported in the main text and supplementary material, including code to generate all figures. The .zip file has the following structure:

Full R Project Code.zip
|
|--> Figures/
	|--> Figure1.png & Figure2.png: The figures included in the main manuscript. This folder needs to be included in the directory in order for the Markdown file to compile 
|--> renv/
	|--> All files relating to the R environment used in the project. It is especially important that the file `renv/activate.R` is included in the download
|--> scored_data/
	|--> The same folder and contents as included in the main repository. It is included here so that all required files can be downloaded as a single .zip file 
|--> .Rprofile: A file necessary for the project to run
|--> Markdown.Rproj: The R Project file 
|--> renv.lock: Lockfile including package metadata that allow the project to be reinstalled on a new machine. Used by `renv::restore()` to reproduce the working R environment in which the project was built
|--> Supplement Analysis & Figures.Rmd: R Markdown file including all code for figures and analysis reported in the main manuscript and supplementary material. Outputs to HTML file

-- README.txt --
That's this file that you're reading right now.

-- Supplement Analysis & Figures.html --
Output of the Markdown file included in `Full R Project Code.zip`. This file includes all code required to reproduce the results and generate the figures

---- General Notes ----
This project has been distributed using a self-contained `renv` environment. In order to run the Markdown file and reproduce the analysis, it is important to ensure that the generated auto-loaders `.Rprofile` and `renv/activate.R` are included in the download and are stored in the same project folder. Downloading and unzipping the .zip file directly should include all of the relevant files and directories necessary
When first launching this project, `renv` should automatically bootstrap itself, downloading and installing all of the necessary files and versions into the project library. After this has been completed, make sure to run the following command to restore the project locally on your machine
	renv::restore()
Press Y when prompted to activate the project to ensure that `renv::restore()` restores packages into the project library as expected
