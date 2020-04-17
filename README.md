### R code for evaluating ClinicalTrials.gov database looking at how well controlled clinical trials have been over the past 10 years

This is the R code to parse ClinicalTrials.gov data compiled by AACT. Running the file **"placeboTrials_handCuratedFile.R"** will reproduce the figures and generate the compiled data

The source directory/working directory should be set to wherever this R file is saved, and the csv file **"placebo.csv"** should be within the same directory.

Before running this file, an individual must make an account with the
AACT website via the following link.  
*https://aact.ctti-clinicaltrials.org/users/sign_up*

Within this R file are several variables that should be set before running the script.   
**savePlot** - this variable is boolean (TRUE/FALSE), and determines whether the plots generated will be saved in the working directory.    
**saveData** - this variable is boolean (TRUE/FALSE), and determines whether the data generated will be saved in the working directory.  
**userAACT** - this variable needs to be set as a string to whatever user name the user has setup with the above link. e.g. "user_name".  
**passwordAACT** - this variable needs to be set as a string to whatever password the user has setup with the above link. e.g. "user_name".

---
R Packages required for running this analysis.
tidyr  
RPostgreSQL  
dplyr  
plyr  
stringr  
lubridate  
ggplot2  
ggsci  
gridExtra  
cowplot  
here  

Install with install.packges('packageName')

---

BSD-3 License  
David Caldwell, on behalf of the coauthors
