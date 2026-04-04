- This project is about folder structure [short description of the study]

- A description of the folder structure
|-root_dir
|---1_General_Protocol
|-------About_Protocol.txt
|-------Research_Design.docx
|-------Literature_Review.docx [optional]
|-------General_IRB_Approval.pdf
|
|---2_Study1_keyword1
|---2_1_Study_specific_protocol [optional]
|-------Study_design.docx
|-------2_1_1_Prereg [optional]
|---2_2_MaterialProc
|-------About_MaterialProc.txt
|-------2_2_1_Procedure
|-------2_2_2_Other_docs
|-------2_2_3_IRB_Approval
|---2_3_RawData
|-------About_RawData.txt
|-------Experiment_Records.txt
|-------RawData
|---2_4_Analayses
|-------About_Analyses.txt
|-------2_4_1_Data_bids [Standardized_Data]
|-------------2_4_1_1_RawData_for_preprocessing[convert raw data to standardized data]
|-------------2_4_1_1_data[converted data]
|--------------------proecprocessing_code.r
|-------2_4_2_Script
|-------------2_4_2_1_figures
|-------------Analyses_code.r
|-------2_4_3_tmp_data [optional]
|
|---3_Study2[optional]
|------- ...
|
|---4_Reports
|-------About_Reports.txt
|-------4_1_Project_reports [slides]
|-------4_2_Preprint
|-------4_3_Conference1 [optional]
|-------4_4_Conference2 [optional]
|-------4_5_Manuscript
|
|---ReadMe.txt
|-.gitignore [optional]

- Software required to open or run any of the shared files
This project used Psychopy 3.0.0 to present stimuli and collect, R for behavioral data analyses, python 3.7.3 for HDDM analyses, MNE for EEG data analyses, fMRIprep for fMRI data preprecessing, Nipy for .....

- Under which license(s) the files are shared
---- CC-BY 4.0 [default]

- Regularity of file/folder names
All projects, files, and folders should be named in a systematic way.
---- Project names: related to research topic. For example, "sv02" mean the 2nd project related to the validation of self-tagging paradigm.
---- Study names should name after projects names, for example, "sv02_s1nm" study 1 of sv02 targetted at non-matching trials
---- folder names should be consistent with its corresponding level (project/study/sub-folder of study)
---- file names should include the following parts: content of the doc, project name, study name, author/edictor, and date. For example, Exp_Design_sv02_s1nm_ChengDong_20210915.docx

- Online information related to this project
---- Project storage: osf.io/...
---- Code storage: github.com/Chuan-Peng-Lab/....
---- Registration: osf.io/.... [optional]
---- Preprint: psyrxiv.org/.... [optional]
---- Journal: ...

- Contact infomration for the authors
---- XXX, email:
---- Hu Chuan-Peng (Prof., Dr.), email: hcp4715@hotmail.com