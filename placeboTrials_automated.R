#########################################
# load libraries
library(tidyr)
library(RPostgreSQL)
library(plyr)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggsci)
library(gridExtra)
library(cowplot)
library(here)
library(sjPlot)
library(coin)

#########################################
# boolean values for saving, username and password for accessing AACT database

savePlot = TRUE  
saveData = TRUE
userAACT="djcald"
passwordAACT="DD968radford"

#########################################
# create search parameters
startDate = as.Date("2009-01-01")
endDate = as.Date("2019-12-31")
countriesList = c("United States")
`%nin%` = Negate(`%in%`)
placeboString = c('placebo','standard of care','usual care')
placeboStringOnly = c('placebo')
standardCareString = c('standard of care','usual care')

# terms to search within conditions field
diseaseTerms = c('cardiac', 'heart', 'cardiomyopathy', 'cardiopulmonary resuscitation',
'mechanical circulatory support', 'ejection fraction', 'myocardial', 'arrhythmia', 'atrial fibrillation',
'ventricular fibrillation', 'tachycardia', 'bradycardia', 'accessory pathway', 'left atrial appendage',
'anticoagulation', 'antiplatelet', 'antithrombotic', 'chest pain', 'acute coronary syndrome', 'ST-elevation MI',
'non-ST-elevation MI', 'artery disease', 'cholesterol', 'lipid', 'triglyceride', 'statin', 'hypertension',
'blood pressure', 'cerebrovascular', 'transient ischemic event', 'ischemic stroke')

# terms to exclude, since often glaucoma trials are included with hypertension studies
termsSearchCondTitleExclude = c('glaucoma','ocular hypertension','opthalm')


#########################################

# connect to database
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org",user=userAACT,password=passwordAACT,port=5432)

# begin loading, filtering, selecting tables

study_tbl_conditions = tbl(src=con, 'conditions')

condsCond <- study_tbl_conditions %>% select(nct_id,downcase_name) %>% filter(downcase_name %in% diseaseTerms) %>% collect
condsCond = condsCond %>% group_by(nct_id) %>% summarize(condsPaste = paste(downcase_name,collapse=", ")) %>% collect()

study_tbl = tbl(src=con,'studies')
#filter_dates <- study_tbl %>% select(official_title,study_first_posted_date,verification_date,start_date,start_month_year,nct_id,phase,last_known_status,study_type,enrollment,overall_status) %>% filter(study_type == 'Interventional')  %>% collect()
filter_dates <- study_tbl %>% select(official_title,study_first_posted_date,verification_date,start_date,start_month_year,nct_id,phase,last_known_status,study_type,enrollment,overall_status) %>% filter((start_date >= startDate) & (study_first_posted_date >= startDate) & (study_first_posted_date <= endDate)  & (study_type == 'Interventional'))  %>% collect()
filter_dates <- filter_dates %>%filter(nct_id %in% condsCond$nct_id) %>% mutate(phase = replace(phase, phase == "N/A", "Not Applicable"))

# update overall status with last known status 
filter_dates <- filter_dates %>% mutate(overall_status = case_when(!is.na(last_known_status) ~ last_known_status,
                                                                   TRUE ~ overall_status))
# search within title
trials_excluded_title <- filter_dates %>% filter(str_detect(tolower(official_title),pattern = paste(termsSearchCondTitleExclude,collapse="|"))) 

filter_dates <- filter_dates %>% filter(!str_detect(tolower(official_title),pattern = paste(termsSearchCondTitleExclude,collapse="|"))) 

# search within brief summary 
brief_summaries_tbl = tbl(src=con,"brief_summaries")
brief_summaries = brief_summaries_tbl %>% select(nct_id,description) %>% collect()
brief_summaries <- brief_summaries %>% filter(nct_id %in% filter_dates$nct_id)

trials_excluded_summary <- brief_summaries %>% filter(str_detect(tolower(description),pattern = paste(termsSearchCondTitleExclude,collapse="|"))) 

filter_dates <- filter_dates %>% filter(nct_id %nin% trials_excluded_summary$nct_id)

interventions_tbl = tbl(src=con,'interventions')
interventions = interventions_tbl %>% select(nct_id,name,description) %>% collect()
interventions <- interventions %>% group_by(nct_id) %>% summarize(name_comb =paste(name,collapse=", "),descrip_comb=paste(description,collapse=", "))

design_groups_tbl = tbl(src=con,'design_groups')
design_groups <- design_groups_tbl %>% select(nct_id,group_type,title,description) %>% collect()
design_groups_counted <- design_groups %>% group_by(nct_id) %>% tally()
design_groups_counted <- rename(design_groups_counted,number_of_arms = n)

design_groups <- design_groups %>% group_by(nct_id) %>% summarize(group_type_comb = paste(group_type,collapse=", "))

designTrial = design_groups %>% mutate(designGroup = case_when(str_detect(tolower(group_type_comb), pattern = paste('placebo comparator')) ~ 'Placebo Comparator',
                                                                          str_detect(tolower(group_type_comb), pattern = paste('active comparator')) ~ 'Active Comparator',
                                                                          str_detect(tolower(group_type_comb), pattern = paste('sham')) ~ 'Sham Comparator',
                                                                          str_detect(tolower(group_type_comb), pattern = paste('treatment comparison')) ~ 'Treatment Comparison',
                                                                          str_detect(tolower(group_type_comb), pattern = paste('active')) ~ 'Active Comparator',
                                                                          str_detect(tolower(group_type_comb), pattern = paste('case')) ~ 'Case',
                                                                          str_detect(tolower(group_type_comb), pattern = paste('control')) ~ 'Control',
                                                                          str_detect(tolower(group_type_comb), pattern = paste('other')) ~ 'Other',
                                                                          str_detect(tolower(group_type_comb), pattern = paste('no intervention')) ~ 'No intervention',
                                                                          str_detect(tolower(group_type_comb), pattern = paste('null')) ~ 'Null',
                                                                          str_detect(tolower(group_type_comb), pattern = paste('experimental')) ~ 'Experimental Only'))

designTrialSummaryCheck <- designTrial %>% group_by(designGroup) %>% tally()

designTrialCollapsed = design_groups %>% mutate(designGroup = case_when(str_detect(tolower(group_type_comb), pattern = paste('placebo comparator')) ~ 'Control Arm Present',
                                                               str_detect(tolower(group_type_comb), pattern = paste('active comparator')) ~ 'Control Arm Present',
                                                               str_detect(tolower(group_type_comb), pattern = paste('sham')) ~ 'Control Arm Present',
                                                               str_detect(tolower(group_type_comb), pattern = paste('treatment comparison')) ~ 'No Control Arm Present',
                                                               str_detect(tolower(group_type_comb), pattern = paste('active')) ~ 'Control Arm Present',
                                                               str_detect(tolower(group_type_comb), pattern = paste('case')) ~ 'No Control Arm Present',
                                                               str_detect(tolower(group_type_comb), pattern = paste('control')) ~ 'Control Arm Present',
                                                               str_detect(tolower(group_type_comb), pattern = paste('other')) ~ 'No Control Arm Present',
                                                               str_detect(tolower(group_type_comb), pattern = paste('no intervention')) ~ 'No Control Arm Present',
                                                               str_detect(tolower(group_type_comb), pattern = paste('null')) ~ 'No Control Arm Present',
                                                               str_detect(tolower(group_type_comb), pattern = paste('experimental')) ~ 'Experimental Only'))

designTrialCollapsedSummaryCheck <- designTrialCollapsed %>% group_by(designGroup) %>% tally()


designTrialCollapsed$design_groups_counted = design_groups_counted$number_of_arms


# will fix any mislabeled experimental arm only trials later with searh
designTrialCollapsed = designTrialCollapsed %>% mutate(multi_arm = case_when(design_groups_counted==1 ~ 'Single-Arm Trial',
                                                                            (design_groups_counted>1 & designGroup == 'Control Arm Present') ~ 'Control Arm Present',
                                                                             (design_groups_counted>1 & designGroup =='No Control Arm Present') ~ 'No Control Arm Present',
                                                                            (design_groups_counted>1 & designGroup =='Experimental Only') ~ 'No Control Arm Present'))

designTrialCollapsedArmSummaryCheck <- designTrialCollapsed %>% group_by(multi_arm) %>% tally()


designTrialExamineExperimentalOnly <- designTrialCollapsed %>% filter(design_groups_counted>1 & designGroup == 'Experimental Only')

baseline_counts_tbl = tbl(src=con,'baseline_counts')
baseline_counts <- baseline_counts_tbl %>% select(nct_id,count) %>% collect()

design_tbl = tbl(src=con,'designs')
design = design_tbl %>% select(nct_id,intervention_model) %>% collect()

location_tbl = tbl(src=con,'countries')

# check if country is the only one in a list 
locations = location_tbl %>% select(nct_id,name)  %>% collect()
locations <- locations %>%  group_by(nct_id) %>% summarize(countriesPaste = paste(name,collapse=", ")) %>% mutate(usaLoc = case_when(countriesPaste == countriesList ~ 'USA only',
                                                                                                                                     str_detect(countriesPaste, pattern = paste(countriesList)) ~ 'USA site present',
                                                                                                                                     TRUE ~ 'no site in USA'))
                                                                                                                                    

sponsor_tbl = tbl(src=con,'sponsors')
sponsor <- sponsor_tbl %>%  select(nct_id,agency_class,lead_or_collaborator,name)%>% collect()


sponsor = sponsor %>% group_by(nct_id) %>% mutate(funding = case_when(any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Industry',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='nih')) ~ 'NIH',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='u.s. fed')) ~ 'U.S. Fed',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Industry',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='nih')) ~ 'NIH',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='u.s. fed')) ~ 'U.S. Fed',
                                                                      TRUE ~ 'Other'))

sponsor = distinct(sponsor,nct_id,.keep_all=TRUE) %>% select(nct_id,funding)

sponsorCombined <- sponsor_tbl %>%  select(nct_id,agency_class,lead_or_collaborator,name)%>% collect()
sponsorCombined = sponsorCombined %>% group_by(nct_id) %>% mutate(fundingComb = case_when(any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Industry',
                                                                                                                 any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='nih')) ~ 'Public',
                                                                                                                 any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='u.s. fed')) ~ 'Public',
                                                                                                                 any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Industry',
                                                                                                                 any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='nih')) ~ 'Public',
                                                                                                                 any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='u.s. fed')) ~ 'Public',
                                                                                                                 TRUE ~ 'Other'))

sponsorCombined = sponsorCombined %>% group_by(nct_id) %>% mutate(univHosp = case_when(any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Other',
                                                                                       any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Other',
                                                                                          any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(name),pattern='university')) ~ 'University',
                                                                                          any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(name),pattern='hospital')) ~ 'Hospital',
                                                                                          TRUE ~ 'Other'))



sponsorCombined = distinct(sponsorCombined,nct_id,.keep_all=TRUE) %>% select(nct_id,fundingComb,univHosp)

calculatedValues_tbl = tbl(src=con,'calculated_values')
calculatedValues <- calculatedValues_tbl  %>% select(nct_id,were_results_reported,minimum_age_num,minimum_age_unit) %>% collect()
calculatedValues <- calculatedValues %>% filter((minimum_age_num >= 18) & (minimum_age_unit == 'Years'))

facilities_tbl = tbl(src=con,'facilities')
facilities <- facilities_tbl  %>% select(nct_id,status,name) %>%collect()
facilities_tabulated <- facilities %>% group_by(nct_id) %>% tally()
facilities_tabulated <- rename(facilities_tabulated,facilitiesCount = n)
facilities_tabulated <- facilities_tabulated %>% mutate(multisite = ifelse(facilitiesCount>1,TRUE,FALSE))

study_ref_tbl = tbl(src=con,'study_references')
study_ref <- study_ref_tbl %>% select(nct_id,pmid,reference_type,citation) %>% collect()
study_ref_tabulated <- study_ref %>% group_by(nct_id) %>% tally()
study_ref_tabulated <- rename(study_ref_tabulated,pubCount = n)

# this is a join that includes all categories, but only ones that match the description 
joinedTable <- join_all(list(locations,interventions,design_groups_counted,design,designTrialCollapsed,filter_dates,facilities_tabulated,sponsor,sponsorCombined,calculatedValues),by='nct_id',type="full")
joinedTable <- joinedTable %>% filter((nct_id %in% filter_dates$nct_id) & (nct_id %in% calculatedValues$nct_id))

# get rid of any NA start dates
#joinedTable <- joinedTable[complete.cases(joinedTable$start_date),]

# this adds pub counts, and NAs for those that dont have pubs
joinedTable <- left_join(joinedTable,study_ref_tabulated,by='nct_id')

joinedTable <- joinedTable %>% mutate(pubCountBool = case_when(!is.na(pubCount) ~ 'TRUE',
                                                               TRUE ~ 'FALSE'))

joinedTable <- joinedTable %>% mutate(yearStart=year(joinedTable$study_first_posted_date))

# first pass of quality control, if a trial doesnt have a number of arms, and no placebo listed, then no control arm present/
# trials that have other matching criteria are added below 
joinedTable$multi_arm[is.na(joinedTable$number_of_arms) & (joinedTable$intervention_model == 'Parallel Assignment' | joinedTable$intervention_model == 'Crossover Assignment' | joinedTable$intervention_model == 'Factorial Group Assignment' | joinedTable$intervention_model == 'Sequential Assignment') & str_detect(tolower(joinedTable$name_comb), pattern = paste(placeboString,collapse="|"),negate=TRUE)] = 'No Control Arm Present'
noArmsListed = joinedTable[is.na(joinedTable$number_of_arms) & (joinedTable$intervention_model == 'Parallel Assignment' | joinedTable$intervention_model == 'Crossover Assignment' | joinedTable$intervention_model == 'Factorial Group Assignment' | joinedTable$intervention_model == 'Sequential Assignment') & str_detect(tolower(joinedTable$name_comb), pattern = paste(placeboString,collapse="|"),negate=TRUE),]

# fix NAs for single gorup assignment, by definition now no control arm present 
joinedTable$multi_arm[is.na(joinedTable$number_of_arms) & joinedTable$intervention_model == 'Single Group Assignment'] = 'Single-Arm Trial' 

# fix multiple experimental arms that have placebo listed under interventions 
joinedTable$multi_arm[joinedTable$number_of_arms > 1 & joinedTable$designGroup == 'Experimental Only' & str_detect(tolower(joinedTable$name_comb), pattern = paste(placeboString,collapse="|"))] = 'Control Arm Present'

joinedTable$multi_arm[joinedTable$active_placebo == 'Placebo Comparator' | joinedTable$active_placebo == 'Active & Placebo Present' | joinedTable$active_placebo == 'Active Comparator']= 'Control Arm Present'


joinedTable$multi_arm[is.na(joinedTable$number_of_arms) & (joinedTable$intervention_model == 'Parallel Assignment' | joinedTable$intervention_model == 'Crossover Assignment' | joinedTable$intervention_model == 'Factorial Group Assignment' | joinedTable$intervention_model == 'Sequential Assignment') & str_detect(tolower(joinedTable$name_comb), pattern = paste(placeboString,collapse="|"))] = 'Control Arm Present'


# count number of missing columns
joinedTable<- joinedTable %>% mutate(numMissing = rowSums(is.na(.)))

# double check that no trials are double counted
doubleCounts <- joinedTable %>% group_by(nct_id) %>% summarise(count=n())
unique(doubleCounts$count)

# add in industry vs. non industry
joinedTable <- joinedTable %>% mutate(industryNonIndustry = case_when(str_detect(tolower(funding), pattern = paste('industry')) ~ 'Industry Sponsor',
                                                                          TRUE ~ 'Non-Industry Sponsor'))

# add in information about placebo, active comparator, both 
joinedTable <- joinedTable %>% mutate(active_placebo = case_when((str_detect(tolower(group_type_comb), pattern = paste('placebo comparator'))) & (str_detect(tolower(group_type_comb), pattern = paste('active comparator')))~ 'Active & Placebo Present',
                                                                 (str_detect(tolower(group_type_comb), pattern = paste('sham comparator'))) & (str_detect(tolower(group_type_comb), pattern = paste('active comparator')))~ 'Active & Placebo Present',
                                                                        str_detect(tolower(name_comb),  pattern = paste(placeboStringOnly,collapse="|")) ~ 'Placebo Comparator',
                                                                        str_detect(tolower(name_comb),  pattern = paste(standardCareString,collapse="|")) ~ 'Placebo Comparator',
                                                                        str_detect(tolower(group_type_comb), pattern = paste('active comparator')) ~ 'Active Comparator',
                                                                        str_detect(tolower(group_type_comb), pattern = paste('placebo comparator')) ~ 'Placebo Comparator',
                                                                        str_detect(tolower(group_type_comb),pattern=paste('sham comparator'))~ 'Placebo Comparator'))

joinedTableFix <- joinedTable %>% filter((multi_arm != 'Control Arm Present') & (!is.na(active_placebo))) %>% mutate(multi_arm = case_when((str_detect(tolower(name_comb),pattern = paste(placeboStringOnly,collapse="|"))) |(str_detect(tolower(descrip_comb),pattern = paste(placeboStringOnly,collapse="|"))) ~'Control Arm Present',
                                                                                                                                           str_detect(tolower(designGroup),pattern='control arm present') ~ 'Control Arm Present'))


joinedTable$multi_arm[(joinedTable$multi_arm != 'Control Arm Present') & (!is.na(joinedTable$active_placebo))] = joinedTableFix$multi_arm


joinedTable$multi_arm[(joinedTable$multi_arm != 'Control Arm Present') & (!is.na(joinedTable$active_placebo))] = joinedTableFixAP$active_placebo


# get rid of sham AV trial
joinedTable <- joinedTable %>% filter(nct_id != 'NCT03483051')

# do all the manual curation

joinedTable$active_placebo[joinedTable$nct_id == 'NCT00846469'] = 'Active Comparator'
joinedTable$multi_arm[joinedTable$nct_id == 'NCT00846469'] = 'Control Arm Present'

joinedTable$active_placebo[joinedTable$nct_id == 'NCT00989339'] = 'Placebo Comparator'
joinedTable$multi_arm[joinedTable$nct_id == 'NCT00989339'] = 'Control Arm Present'

joinedTable$active_placebo[joinedTable$nct_id == 'NCT01113294'] = NA
joinedTable$multi_arm[joinedTable$nct_id == 'NCT01113294'] = 'Single-Arm Trial'

joinedTable$active_placebo[joinedTable$nct_id == 'NCT01178034'] = 'Active Comparator'
joinedTable$multi_arm[joinedTable$nct_id == 'NCT01178034'] = 'Control Arm Present'

joinedTable$active_placebo[joinedTable$nct_id == 'NCT01198496'] = 'Active Comparator'
joinedTable$multi_arm[joinedTable$nct_id == 'NCT01198496'] = 'Control Arm Present'

joinedTable$active_placebo[joinedTable$nct_id == 'NCT01285193'] = 'Active Comparator'
joinedTable$multi_arm[joinedTable$nct_id == 'NCT01285193'] = 'Control Arm Present'

joinedTable$active_placebo[joinedTable$nct_id == 'NCT01310491'] = 'Active Comparator'
joinedTable$multi_arm[joinedTable$nct_id == 'NCT01310491'] = 'Control Arm Present'

joinedTable$active_placebo[joinedTable$nct_id == 'NCT02150252'] = 'Placebo Comparator'
joinedTable$multi_arm[joinedTable$nct_id == 'NCT02150252'] = 'Control Arm Present'

joinedTable$number_of_arms[joinedTable$nct_id == 'NCT00839891'] = 3 
joinedTable$active_placebo[joinedTable$nct_id == 'NCT00839891'] = 'Active & Placebo Present'
joinedTable$intervention_model[joinedTable$nct_id == 'NCT00839891'] = 'Parallel Assignment'

joinedTable$number_of_arms[joinedTable$nct_id == 'NCT00996021'] = 3 
joinedTable$active_placebo[joinedTable$nct_id == 'NCT00996021'] = 'Active & Placebo Present'
joinedTable$intervention_model[joinedTable$nct_id == 'NCT00996021'] = 'Crossover Assignment'

joinedTable$number_of_arms[joinedTable$nct_id == 'NCT01328054'] = 2
joinedTable$active_placebo[joinedTable$nct_id == 'NCT01328054'] = 'Placebo Comparator'
joinedTable$intervention_model[joinedTable$nct_id == 'NCT01328054'] = 'Crossover Assignment'

joinedTable$number_of_arms[joinedTable$nct_id == 'NCT01667744'] = 2
joinedTable$active_placebo[joinedTable$nct_id == 'NCT01667744'] = 'Placebo Comparator'
joinedTable$intervention_model[joinedTable$nct_id == 'NCT01667744'] = 'Parallel Assignment'

joinedTable$number_of_arms[joinedTable$nct_id == 'NCT01959971'] = 2
joinedTable$active_placebo[joinedTable$nct_id == 'NCT01959971'] = 'Placebo Comparator'
joinedTable$intervention_model[joinedTable$nct_id == 'NCT01959971'] = 'Crossover Assignment'

joinedTable$number_of_arms[joinedTable$nct_id == 'NCT01968720'] = 2
joinedTable$active_placebo[joinedTable$nct_id == 'NCT01968720'] = 'Placebo Comparator'
joinedTable$intervention_model[joinedTable$nct_id == 'NCT01968720'] = 'Crossover Assignment'

#joinedTable$number_of_arms[joinedTable$nct_id == 'NCT01968720'] = 1
#joinedTable$active_placebo[joinedTable$nct_id == 'NCT01968720'] = NA
#joinedTable$intervention_model[joinedTable$nct_id == 'NCT01968720'] = 'Single-Arm Assignment'

joinedTable$number_of_arms[joinedTable$nct_id == 'NCT02153983'] = 3
joinedTable$active_placebo[joinedTable$nct_id == 'NCT02153983'] = 'Placebo Comparator'
joinedTable$intervention_model[joinedTable$nct_id == 'NCT02153983'] = 'Parallel Assignment'

joinedTable$number_of_arms[joinedTable$nct_id == 'NCT02424695'] = 2
joinedTable$active_placebo[joinedTable$nct_id == 'NCT02424695'] = 'Placebo Comparator'
joinedTable$intervention_model[joinedTable$nct_id == 'NCT02424695'] = 'Crossover Assignment'

joinedTable$number_of_arms[joinedTable$nct_id == 'NCT02496221'] = 2
joinedTable$active_placebo[joinedTable$nct_id == 'NCT02496221'] = 'Placebo Comparator'
joinedTable$intervention_model[joinedTable$nct_id == 'NCT02496221'] = 'Crossover Assignment'

joinedTable$number_of_arms[joinedTable$nct_id == 'NCT02497937'] = 2
joinedTable$active_placebo[joinedTable$nct_id == 'NCT02497937'] = 'Placebo Comparator'
joinedTable$intervention_model[joinedTable$nct_id == 'NCT02497937'] = 'Crossover Assignment'

joinedTable$number_of_arms[joinedTable$nct_id == 'NCT02593305'] = 2
joinedTable$active_placebo[joinedTable$nct_id == 'NCT02593305'] = 'Placebo Comparator'
joinedTable$intervention_model[joinedTable$nct_id == 'NCT02593305'] = 'Crossover Assignment'

joinedTable$number_of_arms[joinedTable$nct_id == 'NCT02906579'] = 2
joinedTable$active_placebo[joinedTable$nct_id == 'NCT02906579'] = 'Placebo Comparator'
joinedTable$intervention_model[joinedTable$nct_id == 'NCT02906579'] = 'Parallel Assignment'

joinedTable$number_of_arms[joinedTable$nct_id == 'NCT02906579'] = 2
joinedTable$active_placebo[joinedTable$nct_id == 'NCT02906579'] = 'Placebo Comparator'
joinedTable$intervention_model[joinedTable$nct_id == 'NCT02906579'] = 'Crossover Assignment'

# create new group for control group present vs absent (lumping single arm in)
joinedTable <- joinedTable %>% mutate(control_status = case_when(multi_arm=='Control Arm Present' ~ 'Control Arm Present',
                                                                 ((multi_arm=='Single-Arm Trial') | (multi_arm=='No Control Arm Present')) ~ 'No Control Arm Present'))

# create column for condensed phase
joinedTable <- joinedTable %>% mutate(phase_condensed = case_when(phase=='Phase 4' ~ 'Phase 4',
                                                                 ((phase == 'Phase 3') | (phase == 'Phase 2/Phase 3')) ~ 'Phase 3',
                                                                 ((phase == 'Phase 2') | (phase == 'Phase 1/Phase 2')) ~ 'Phase 2',
                                                                 ((phase == 'Phase 1') | (phase == 'Early Phase 1')) ~ 'Phase 1',
                                                                 phase == 'Not Applicable' ~ 'Not Applicable'))

# create column for condensed status
joinedTable <- joinedTable %>% mutate(status_condensed = case_when(((overall_status == 'Not yet recruiting') | (overall_status == 'Active, not recruiting') | (overall_status == 'Enrolling by invitation') | (overall_status == 'Recruiting')) ~ 'In process',
                                                                   ((overall_status == 'Withdrawn') | (overall_status == 'Terminated') | (overall_status == 'Suspended')) ~ 'Discontinued',
                                                                  overall_status == 'Completed' ~ 'Completed',
                                                                  TRUE ~ 'Unknown'))



# create column for phase 

# done processing, now do checks, totals, and calculations 

joinedTableCheck <- joinedTable %>% filter((multi_arm != 'Control Arm Present') & (!is.na(active_placebo)))

joinedTableActivePlacebo <- joinedTable %>% filter(control_status=='Control Arm Present') %>% group_by(active_placebo) %>% tally()
joinedTableWhichDesign <- joinedTable %>% filter(multi_arm=='Control Arm Present') %>% group_by(intervention_model) %>% tally()
joinedTableWhichDesignTrials <- joinedTable %>% filter(multi_arm=='Control Arm Present') %>% group_by(intervention_model)

joinedTableActivePlaceboCheck <- joinedTable %>% filter(multi_arm!='Control Arm Present') %>% group_by(active_placebo) %>% tally()
joinedTableWhichDesignCheck <- joinedTable %>% filter(multi_arm!='Control Arm Present') %>% group_by(intervention_model) %>% tally()

joinedTableActivePlaceboCheckNoCtrl <- joinedTable %>% filter(multi_arm=='No Control Arm Present') %>% group_by(active_placebo) %>% tally()
joinedTableWhichDesignCheckNoCtrl <- joinedTable %>% filter(multi_arm=='No Control Arm Present') %>% group_by(intervention_model) %>% tally()
joinedTableWhichDesignCheckNoCtrlTrials <- joinedTable %>% filter(multi_arm=='No Control Arm Present') %>% group_by(intervention_model)

joinedTableDoubleCheck <- joinedTable %>% filter((multi_arm != 'Control Arm Present') & ((active_placebo == 'Active & Placebo Present') | (active_placebo == 'Active Comparator') | (active_placebo == 'Placebo Comparator') ))
joinedTableTripleCheck <- joinedTable %>% filter((multi_arm == 'Control Arm Present') & (is.na(active_placebo)))

# check single arm
joinedTableSingleCheck <- joinedTable %>% filter((multi_arm == 'Single-Arm Trial') & ((intervention_model == "Parallel Assignment") | (intervention_model == 'Crossover Assignment')))

# group by year and multi-arm group 
joinedTableCount <- joinedTable %>% group_by(yearStart,control_status) %>%
  summarize(n=n()) %>%
  mutate(freq = n/sum(n))
joinedTableCount <- rename(joinedTableCount,yearlyCount = n)

# calculate statistics
joinedTableTotals <- joinedTable %>% group_by(control_status) %>% tally()

controlN =  joinedTableTotals$n[joinedTableTotals$control_status == 'Control Arm Present']
noControlN = joinedTableTotals$n[joinedTableTotals$control_status == 'No Control Arm Present']

joinedTableSummarize <- joinedTable %>% group_by(control_status) %>% summarize(median=median(number_of_arms,na.rm=TRUE),iqr = IQR(number_of_arms,na.rm=TRUE))

joinedTableSummarizeCountry <- joinedTable %>% group_by(control_status,usaLoc) %>% tally()
joinedTableSummarizeCountry <- joinedTableSummarizeCountry %>% mutate(totalN = case_when(control_status=='Control Arm Present' ~ controlN ,
                                                                      control_status=='No Control Arm Present' ~ noControlN))

joinedTableSummarizeType <- joinedTable %>% group_by(control_status,study_type) %>% tally()
joinedTableSummarizePhase <- joinedTable %>% group_by(control_status,phase_condensed) %>% tally()
joinedTableSummarizePhaseMore <- joinedTable %>% group_by(control_status,phase) %>% tally()
joinedTableSummarizeAgency <- joinedTable %>% group_by(control_status,fundingComb) %>% tally()
joinedTableSummarizeReported <- joinedTable %>% group_by(control_status,were_results_reported) %>% tally()
joinedTableSummarizeSite<- joinedTable %>% group_by(control_status,multisite) %>% tally()
joinedTableSummarizeStatus<- joinedTable %>% group_by(control_status,last_known_status) %>% tally()
joinedTableSummarizeOverallStatus <- joinedTable %>% group_by(control_status,status_condensed) %>% tally()
joinedTableSummarizePubCount <- joinedTable %>% group_by(control_status,pubCountBool) %>% tally()
joinedTableMedianNumbers <- joinedTable %>% filter(enrollment>0) %>% group_by(control_status) %>% summarize(median=median(enrollment,na.rm=TRUE),iqr = IQR(enrollment,na.rm=TRUE))
joinedTableUnivHosp <- joinedTable %>% filter((univHosp %in% c('University','Hospital')) & fundingComb == 'Other') %>% group_by(control_status,univHosp) %>% tally()

#########################################
# statistical testing


# group by year and multi-arm group 
joinedTableCountCat <- joinedTable %>% mutate(yearStart = as.factor(yearStart)) %>% group_by(yearStart,control_status) %>%
  summarize(n=n()) %>%
  mutate(freq = n/sum(n))
joinedTableCountCat <- rename(joinedTableCountCat,yearlyCount = n)

joinedTableCat <- joinedTable %>% mutate(yearStart = as.factor(yearStart)) %>% 
  mutate(control_status = recode(control_status,"No Control Arm Present" = 0,"Control Arm Present"=1))

stat_model_cat <- glm(control_status~yearStart,data=joinedTableCat,family=binomial(link="logit"))

summary(stat_model_cat)
confint(stat_model_cat)
emmeansModel <- emmeans(stat_model_cat,'yearStart',type='response')
pairs(emmeansModel,reverse=TRUE)
confint(emmeansModel)


joinedTableSampleSizeTest <- joinedTable %>% filter(enrollment>0) %>% select(control_status,yearStart,enrollment,multisite,status_condensed,usaLoc,fundingComb,phase,phase_condensed)
joinedTableSampleSizeTest$control_status <- as.factor(mapvalues(joinedTableSampleSizeTest$control_status,from=c('Control Arm Present','No Control Arm Present'),to=c(1,0)))
joinedTableSampleSizeTest$yearStart <- as.integer(mapvalues(joinedTableSampleSizeTest$yearStart,from=c(min(joinedTableSampleSizeTest$yearStart):max(joinedTableSampleSizeTest$yearStart)),to=c(0:(length(unique(joinedTableSampleSizeTest$yearStart))-1))))
which(! complete.cases(joinedTableSampleSizeTest))

medianSampleSize <- median_test(enrollment~control_status,data = joinedTableSampleSizeTest)

yearlyCount = joinedTableCount$yearlyCount
lengthYC= length(yearlyCount)

stat_model <- glm(control_status~yearStart,family=binomial(link="logit"),data=joinedTableSampleSizeTest)
summary(stat_model)
confint(stat_model)
tab_model(stat_model)

stat_model_group <- glm(control_status~yearStart+multisite+status_condensed+usaLoc+phase_condensed+fundingComb,family=binomial(link="logit"),data=joinedTableSampleSizeTest)
summary(stat_model_group)
tab_model(stat_model_group)
anova(stat_model_group,test="Chisq")
confint(stat_model_group)


tableCountry = table(joinedTable$usaLoc,joinedTable$control_status,useNA = 'ifany')
tableCountryStats <- sapply(1:nrow(tableCountry),function(z) prop.test(tableCountry[z,, drop = TRUE], n = colSums(tableCountry)))
chisq.test(tableCountry)

tableControlArm = table(joinedTable$active_placebo,joinedTable$control_status,useNA='ifany')
tableControlArmStats <- sapply(1:nrow(tableControlArm),function(z) prop.test(tableControlArm[z,, drop = TRUE], n = colSums(tableControlArm)))



tableStatus = table(joinedTable$status_condensed,joinedTable$control_status,useNA = 'ifany')
tableStatusStats <- sapply(1:nrow(tableStatus),function(z) prop.test(tableStatus[z,, drop = TRUE], n = colSums(tableStatus)))
chisq.test(tableStatus)

tableSite = table(joinedTable$multisite,joinedTable$control_status,useNA = 'ifany')
Table[is.na(data_1)] <- 0
tableSiteStats <- sapply(1:nrow(tableSite),function(z) prop.test(tableSite[z,, drop = TRUE], n = colSums(tableSite)))
chisq.test(tableSite)


tableFunder = table(joinedTable$fundingComb,joinedTable$control_status,useNA = 'ifany')
tableFunderStats <- sapply(1:nrow(tableFunder),function(z) prop.test(tableFunder[z,, drop = TRUE], n = colSums(tableFunder)))
chisq.test(tableFunder)


tablePhase = table(joinedTable$phase_condensed,joinedTable$control_status,useNA = 'ifany')
tablePhaseStats <- sapply(1:nrow(tablePhase),function(z) prop.test(tablePhase[z,, drop = TRUE], n = colSums(tablePhase)))
chisq.test(tablePhase)


tablePub = table(joinedTable$pubCountBool,joinedTable$control_status,useNA = 'ifany')
tablePubStats <- sapply(1:nrow(tablePub),function(z) prop.test(tablePub[z,, drop = TRUE], n = colSums(tablePub)))
chisq.test(tablePub)


tableResults = table(joinedTable$were_results_reported,joinedTable$control_status,useNA = 'ifany')
tableResultsStats <- sapply(1:nrow(tableResults),function(z) prop.test(tableResults[z,, drop = TRUE], n = colSums(tableResults)))
chisq.test(tableResults)


tableYearlyCount = table(joinedTable$yearStart,joinedTable$control_status,useNA='ifany')
tableYearlyCountStats <- sapply(1:nrow(tableYearlyCount),function(z) prop.test(tableYearlyCount[z,, drop = TRUE], n = colSums(tableYearlyCount)))
chisq.test(tableYearlyCount)

########################
if (saveData){
  saveRDS(joinedTable, file = "controlArmRdata_10_31_2020.rds")
  write.csv(designTrialExamineExperimentalOnly,'experimentalOnly_10_31_2020.csv')
  write.csv(joinedTable,'controlArmTableTotal_10_31_2020.csv')
  write.csv(joinedTableDiverseDiscontinued,'controlArmTableDiscDiverse_10_31_2020.csv')
  write.csv(joinedTableSummarizeInterv,'controlArmTableInterv_10_31_2020.csv')
  write.csv(joinedTableSummarizeType,'controlArmTableType_10_31_2020.csv')
  write.csv(joinedTableSummarizePhase,'controlArmTablePhase_10_31_2020.csv')
  write.csv(joinedTableSummarizeAgency,'controlArmTableAgency_10_31_2020.csv')
  write.csv(joinedTableSummarizeReported,'controlArmTableReported_10_31_2020.csv')
  write.csv(joinedTableSummarizeSite,'controlArmTableSite_10_31_2020.csv')
  write.csv(joinedTableSummarizeStatus,'controlArmTableStatus_10_31_2020.csv')
  write.csv(joinedTableSummarizeOverallStatus,'controlArmTableOverallStatus_10_31_2020.csv')
  write.csv(joinedTableSummarizePubCount,'controlArmTablePubCount_10_31_2020.csv')
}

#########################################

# make plots
pInd<-ggplot(joinedTableCount, aes(x=yearStart,y=yearlyCount, group=control_status, color=control_status)) +
  geom_line()+
  geom_point() +
  labs(title="Number of Cardiovascular Clinical Trials \nRegistered by Control Arm Status, by Year",x = "Year Registered",y="Number of Trials",color='Control Arm Status') +
  ylim(0,max(joinedTableCount$yearlyCount)+10) +
  scale_x_continuous(breaks=seq(2009,2019,1),limits=c(2009,2019)) + 
  scale_color_jama() 
print(pInd)
if (savePlot){
  ggsave("trialsByYearMultiArm_10_31_2020.png", units="in", width=6, height=4, dpi=600)
}

pHist<-ggplot(joinedTable, aes(x=number_of_arms,color=control_status,fill=control_status)) +
  geom_histogram(binwidth=1,alpha=0.5) +
  labs(x = "Number of Arms",y="Count",fill='Control Arm Status') +
  coord_cartesian(xlim=c(0,max(joinedTable$number_of_arms,na.rm = TRUE)))  + 
  guides(color=FALSE)
print(pHist)
if (savePlot){
  ggsave("trialsByYearHist_10_31_2020.png", units="in", width=5, height=4, dpi=600)
}

