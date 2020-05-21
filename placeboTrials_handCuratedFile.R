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

#########################################
# boolean values for saving, username and password for accessing AACT database

savePlot = TRUE
saveData = TRUE
userAACT="USER_NAME"
passwordAACT="PASSWORD"
path_to_data = here('data_files')

#########################################
# set up data directories, load in hand curated file
#dataFile = here("Combination_Cardiac_Search.csv")
#dataFile1 = here("CVA_Cerebro Search.csv")
#dataFile2 = here("Heart Failure Search.csv")
#dataFile3 = here("Arrhythmia Search.csv")
#dataFile4 = here("ACS related Search.csv")
#dataFile5 = here("Cholesterol and BP Search.csv")


handCurated <- list.files(path=path_to_data,full.names = TRUE) %>%
  lapply(read_csv) %>% bind_rows

#  read.csv(file=dataFile, header=TRUE, sep=",",na.strings=c(""))

handCurated <- handCurated %>% rename(nct_id = 'NCT Number')
handCurated = distinct(handCurated,nct_id,.keep_all=TRUE) 


#########################################
# create search parameters
startDate = as.Date("2009-01-01")
countriesList = c("United States")
`%nin%` = Negate(`%in%`)
placeboString = c('placebo','standard of care','usual care')
placeboStringOnly = c('placebo')
standardCareString = c('standard of care','usual care')

#########################################

# connect to database
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org",user=userAACT,password=passwordAACT,port=5432)

# begin loading, filtering, selecting tables

interventions_tbl = tbl(src=con,'interventions')
interventions = interventions_tbl %>% select(nct_id,name,description) %>% collect()
interventions <- interventions %>% filter(nct_id %in% handCurated$nct_id) %>% group_by(nct_id) %>% summarize(name_comb =paste(name,collapse=", "),descrip_comb=paste(description,collapse=", "))

design_groups_tbl = tbl(src=con,'design_groups')
design_groups <- design_groups_tbl %>% select(nct_id,group_type,title,description) %>% collect()
design_groups_counted <- design_groups %>% filter(nct_id %in% handCurated$nct_id) %>% group_by(nct_id) %>% tally()
design_groups_counted <- rename(design_groups_counted,number_of_arms = n)

design_groups <- design_groups %>% filter(nct_id %in% handCurated$nct_id) %>% group_by(nct_id) %>% summarize(group_type_comb = paste(group_type,collapse=", "))

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
                                                               str_detect(tolower(group_type_comb), pattern = paste('active')) ~ 'No Control Arm Present',
                                                               str_detect(tolower(group_type_comb), pattern = paste('case')) ~ 'No Control Arm Present',
                                                               str_detect(tolower(group_type_comb), pattern = paste('control')) ~ 'No Control Arm Present',
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
baseline_counts <- baseline_counts %>% filter(nct_id %in% handCurated$nct_id)

design_tbl = tbl(src=con,'designs')
design = design_tbl %>% select(nct_id,intervention_model) %>% collect()
design <- design %>% filter(nct_id %in% handCurated$nct_id)

study_tbl = tbl(src=con,'studies')
#filter_dates <- study_tbl %>% select(official_title,study_first_posted_date,verification_date,start_date,start_month_year,nct_id,phase,last_known_status,study_type,enrollment,overall_status) %>% filter(study_type == 'Interventional')  %>% collect()
filter_dates <- study_tbl %>% select(official_title,study_first_posted_date,verification_date,start_date,start_month_year,nct_id,phase,last_known_status,study_type,enrollment,overall_status) %>% filter(start_date >= startDate & study_type == 'Interventional')  %>% collect()
filter_dates <- filter_dates %>% filter(nct_id %in% handCurated$nct_id)
filter_dates <- filter_dates%>% mutate(phase = replace(phase, phase == "N/A", "Not Applicable"))

location_tbl = tbl(src=con,'countries')

# check if country is the only one in a list 
locations = location_tbl %>% select(nct_id,name)  %>% collect()
locations <- locations %>% filter(nct_id %in% handCurated$nct_id) %>%  group_by(nct_id) %>% summarize(countriesPaste = paste(name,collapse=", ")) %>% filter (countriesPaste == countriesList) %>% collect()

locationsTotal = location_tbl %>% select(nct_id,name) %>%  collect()
locationsTotal = locationsTotal %>% filter(nct_id %in% handCurated$nct_id) %>% group_by(nct_id) %>% summarize(countriesPaste = paste(name,collapse=", ")) %>% collect()

locationCheck <- full_join(locationsTotal,handCurated,by='nct_id')

sponsor_tbl = tbl(src=con,'sponsors')
sponsor <- sponsor_tbl %>%  select(nct_id,agency_class,lead_or_collaborator)%>% collect()


sponsor = sponsor %>% filter(nct_id %in% handCurated$nct_id) %>% group_by(nct_id) %>% mutate(funding = case_when(any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Industry',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='nih')) ~ 'NIH',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='u.s. fed')) ~ 'U.S. Fed',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Industry',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='nih')) ~ 'NIH',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='u.s. fed')) ~ 'U.S. Fed',
                                                                      TRUE ~ 'Other'))

sponsor = distinct(sponsor,nct_id,.keep_all=TRUE) %>% select(nct_id,funding)

sponsorCombined <- sponsor_tbl %>%  select(nct_id,agency_class,lead_or_collaborator)%>% collect()
sponsorCombined = sponsorCombined %>% filter(nct_id %in% handCurated$nct_id) %>% group_by(nct_id) %>% mutate(fundingComb = case_when(any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Industry',
                                                                                                                 any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='nih')) ~ 'Public',
                                                                                                                 any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='u.s. fed')) ~ 'Public',
                                                                                                                 any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Industry',
                                                                                                                 any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='nih')) ~ 'Public',
                                                                                                                 any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='u.s. fed')) ~ 'Public',
                                                                                                                 TRUE ~ 'Other'))



sponsorCombined = distinct(sponsorCombined,nct_id,.keep_all=TRUE) %>% select(nct_id,fundingComb)

calculatedValues_tbl = tbl(src=con,'calculated_values')
calculatedValues <- calculatedValues_tbl  %>% select(nct_id,were_results_reported,minimum_age_num,minimum_age_unit) %>% collect()
calculatedValues <- calculatedValues %>% filter(nct_id %in% handCurated$nct_id & minimum_age_num >= 18 & minimum_age_unit == 'Years')

facilities_tbl = tbl(src=con,'facilities')
facilities <- facilities_tbl  %>% select(nct_id,status,name) %>%collect()
facilities_tabulated <- facilities %>% filter(nct_id %in% handCurated$nct_id) %>% group_by(nct_id) %>% tally()
facilities_tabulated <- rename(facilities_tabulated,facilitiesCount = n)
facilities_tabulated <- facilities_tabulated %>% mutate(multisite = ifelse(facilitiesCount>1,TRUE,FALSE))

study_ref_tbl = tbl(src=con,'study_references')
study_ref <- study_ref_tbl %>% select(nct_id,pmid,reference_type,citation) %>% collect()
study_ref_tabulated <- study_ref %>% filter(nct_id %in% handCurated$nct_id) %>% group_by(nct_id) %>% tally()
study_ref_tabulated <- rename(study_ref_tabulated,pubCount = n)


# this is a join that includes all categories, but only ones that match the description 
joinedTable <- join_all(list(interventions,design_groups_counted,design,designTrialCollapsed,filter_dates,facilities_tabulated,sponsor,sponsorCombined,calculatedValues),by='nct_id',type="full")
joinedTable <- joinedTable %>% filter((nct_id %in% locations$nct_id) & (nct_id %in% filter_dates$nct_id) & (nct_id %in% calculatedValues$nct_id))

# get rid of any NA start dates
#joinedTable <- joinedTable[complete.cases(joinedTable$start_date),]

# this adds pub counts, and NAs for those that dont have pubs
joinedTable <- left_join(joinedTable,study_ref_tabulated,by='nct_id')

joinedTable <- joinedTable %>% mutate(pubCountBool = case_when(!is.na(pubCount) ~ 'TRUE',
                                                               TRUE ~ 'FALSE'))

handCuratedShrunk <- handCurated %>% filter(nct_id %in% joinedTable$nct_id) %>% collect()
joinedTable <- inner_join(joinedTable,handCuratedShrunk,by='nct_id')

# re-ensure that age selection takes place

joinedTable <- joinedTable %>% mutate(yearStart=year(joinedTable$study_first_posted))

# fix NAs for single gorup assignment, by definition now no control arm present 
joinedTable$multi_arm[is.na(joinedTable$number_of_arms) & joinedTable$intervention_model == 'Single Group Assignment'] = 'Single-Arm Trial' 

# fix multiple experimental arms that have placebo listed under interventions 
joinedTable$multi_arm[joinedTable$number_of_arms > 1 & joinedTable$designGroup == 'Experimental Only' & str_detect(tolower(joinedTable$name_comb), pattern = paste(placeboString,collapse="|"))] = 'Control Arm Present'

joinedTable$multi_arm[is.na(joinedTable$number_of_arms) & (joinedTable$intervention_model == 'Parallel Assignment' | joinedTable$intervention_model == 'Crossover Assignment' | joinedTable$intervention_model == 'Factorial Group Assignment' | joinedTable$intervention_model == 'Sequential Assignment') & str_detect(tolower(joinedTable$name_comb), pattern = paste(placeboString,collapse="|"))] = 'Control Arm Present'

# if it didnt have the above, no control arm present
joinedTable$multi_arm[is.na(joinedTable$number_of_arms) & (joinedTable$intervention_model == 'Parallel Assignment' | joinedTable$intervention_model == 'Crossover Assignment' | joinedTable$intervention_model == 'Factorial Group Assignment' | joinedTable$intervention_model == 'Sequential Assignment') & str_detect(tolower(joinedTable$name_comb), pattern = paste(placeboString,collapse="|"),negate=TRUE)] = 'No Control Arm Present'

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
                                                                        str_detect(tolower(name_comb),  pattern = paste(placeboStringOnly,collapse="|")) ~ 'Placebo Comparator',
                                                                        str_detect(tolower(name_comb),  pattern = paste(standardCareString,collapse="|")) ~ 'Placebo Comparator',
                                                                        str_detect(tolower(group_type_comb), pattern = paste('active comparator')) ~ 'Active Comparator',
                                                                        str_detect(tolower(group_type_comb), pattern = paste('placebo comparator')) ~ 'Placebo Comparator'))

joinedTableFix <- joinedTable %>% filter((multi_arm != 'Control Arm Present') & (!is.na(active_placebo))) %>% mutate(multi_arm = case_when((str_detect(tolower(name_comb),pattern = paste(placeboStringOnly,collapse="|"))) |(str_detect(tolower(descrip_comb),pattern = paste(placeboStringOnly,collapse="|"))) ~'Control Arm Present',
                                                                                                                                           str_detect(tolower(designGroup),pattern='control arm present') ~ 'Control Arm Present'))


joinedTable$multi_arm[(joinedTable$multi_arm != 'Control Arm Present') & (!is.na(joinedTable$active_placebo))] = joinedTableFix$multi_arm


# get rid of sham AV trial
joinedTable <- joinedTable %>% filter(nct_id != 'NCT03483051')

# do all the manual curation

joinedTable$number_of_arms[joinedTable$nct_id == 'NCT00839891'] = 3 
joinedTable$active_placebo[joinedTable$nct_id == 'NCT00839891'] = 'Active & Placebo Present'
joinedTable$intervention_model[joinedTable$nct_id == 'NCT00839891'] = 'Parallel Assignment'

joinedTable$number_of_arms[joinedTable$nct_id == 'NCT00996021'] = 3 
joinedTable$active_placebo[joinedTable$nct_id == 'NCT00996021'] = 'Active & Placebo Present'
joinedTable$intervention_model[joinedTable$nct_id == 'NCT00996021'] = 'Crossover Assignment'

joinedTable$number_of_arms[joinedTable$nct_id == 'NCT01328054'] = 2
joinedTable$active_placebo[joinedTable$nct_id == 'NCT01328054'] = 'Placebo Comparator'
joinedTable$intervention_model[joinedTable$nct_id == 'NCT01328054'] = 'Parallel Assignment'

joinedTable$number_of_arms[joinedTable$nct_id == 'NCT01667744'] = 2
joinedTable$active_placebo[joinedTable$nct_id == 'NCT01667744'] = 'Placebo Comparator'
joinedTable$intervention_model[joinedTable$nct_id == 'NCT01667744'] = 'Parallel Assignment'

joinedTable$number_of_arms[joinedTable$nct_id == 'NCT01959971'] = 2
joinedTable$active_placebo[joinedTable$nct_id == 'NCT01959971'] = 'Placebo Comparator'
joinedTable$intervention_model[joinedTable$nct_id == 'NCT01959971'] = 'Crossover Assignment'

joinedTable$number_of_arms[joinedTable$nct_id == 'NCT01968720'] = 2
joinedTable$active_placebo[joinedTable$nct_id == 'NCT01968720'] = 'Placebo Comparator'
joinedTable$intervention_model[joinedTable$nct_id == 'NCT01968720'] = 'Crossover Assignment'

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

# done processing, now do checks, totals, and calculations 

joinedTableCheck <- joinedTable %>% filter((multi_arm != 'Control Arm Present') & (!is.na(active_placebo)))

joinedTableActivePlacebo <- joinedTable %>% group_by(active_placebo) %>% tally()

joinedTableDoubleCheck <- joinedTable %>% filter((multi_arm != 'Control Arm Present') & ((active_placebo == 'Active & Placebo Present') | (active_placebo == 'Active Comparator') | (active_placebo == 'Placebo Comparator') ))

joinedTableTripleCheck <- joinedTable %>% filter((multi_arm == 'Control Arm Present') & (is.na(active_placebo)))

# group by year and multi-arm group 
joinedTableCount <- joinedTable %>% group_by(yearStart,multi_arm) %>%
  summarize(n=n()) %>%
  mutate(freq = n/sum(n))
joinedTableCount <- rename(joinedTableCount,yearlyCount = n)

# calculate statistics
joinedTableTotals <- joinedTable %>% group_by(multi_arm) %>% tally()

joinedTableSummarize <- joinedTable %>% group_by(multi_arm) %>% summarize(median=median(number_of_arms,na.rm=TRUE),iqr = IQR(number_of_arms,na.rm=TRUE))

joinedTableSummarizeType <- joinedTable %>% group_by(multi_arm,study_type) %>% tally()
joinedTableSummarizePhase <- joinedTable %>% group_by(multi_arm,phase) %>% tally()
joinedTableSummarizeAgency <- joinedTable %>% group_by(multi_arm,funding) %>% tally()
joinedTableSummarizeReported <- joinedTable %>% group_by(multi_arm,were_results_reported) %>% tally()
joinedTableSummarizeSite<- joinedTable %>% group_by(multi_arm,multisite) %>% tally()
joinedTableSummarizeStatus<- joinedTable %>% group_by(multi_arm,last_known_status) %>% tally()
joinedTableSummarizeOverallStatus <- joinedTable %>% group_by(multi_arm,overall_status) %>% tally()
joinedTableSummarizePubCount <- joinedTable %>% group_by(multi_arm,pubCountBool) %>% tally()
joinedTableMedianNumbers <- joinedTable %>% group_by(multi_arm) %>% summarize(median=median(enrollment,na.rm=TRUE),iqr = IQR(enrollment,na.rm=TRUE))

#########################################
# statistical testing

yearlyCount = joinedTableCount$yearlyCount
lengthYC= length(yearlyCount)


stat_model <- lm(freq~yearStart,joinedTableCount)
summary(stat_model)
tab_model(stat_model)

stat_model_group <- lm(freq~yearStart+multi_arm,joinedTableCount)
summary(stat_model_group)
tab_model(stat_model_group)

########################
if (saveData){
  saveRDS(joinedTable, file = "controlArmRdata_5_21_2020.rds")
  write.csv(designTrialExamineExperimentalOnly,'experimentalOnly_5_21_2020.csv')
  write.csv(joinedTable,'controlArmTableTotal_5_21_2020.csv')
  write.csv(joinedTableDiverseDiscontinued,'controlArmTableDiscDiverse_5_21_2020.csv')
  write.csv(joinedTableSummarizeInterv,'controlArmTableInterv_5_21_2020.csv')
  write.csv(joinedTableSummarizeType,'controlArmTableType_5_21_2020.csv')
  write.csv(joinedTableSummarizePhase,'controlArmTablePhase_5_21_2020.csv')
  write.csv(joinedTableSummarizeAgency,'controlArmTableAgency_5_21_2020.csv')
  write.csv(joinedTableSummarizeReported,'controlArmTableReported_5_21_2020.csv')
  write.csv(joinedTableSummarizeSite,'controlArmTableSite_5_21_2020.csv')
  write.csv(joinedTableSummarizeStatus,'controlArmTableStatus_5_21_2020.csv')
  write.csv(joinedTableSummarizeOverallStatus,'controlArmTableOverallStatus_5_21_2020.csv')
  write.csv(joinedTableSummarizePubCount,'controlArmTablePubCount_5_21_2020.csv')
}

#########################################

# make plots
pInd<-ggplot(joinedTableCount, aes(x=yearStart,y=yearlyCount, group=multi_arm, color=multi_arm)) +
  geom_line()+
  geom_point() +
  labs(title="Number of Cardiovascular Clinical Trials \nRegistered by Control Arm Status, by Year",x = "Year Registered",y="Number of Trials",color='Control Arm Status') +
  ylim(0,max(joinedTableCount$yearlyCount)+10) +
  scale_x_continuous(breaks=seq(2009,2019,1),limits=c(2009,2019)) + 
  scale_color_jama() 
print(pInd)
if (savePlot){
  ggsave("trialsByYearMultiArm_5_21_2020.png", units="in", width=6, height=4, dpi=600)
}

pHist<-ggplot(joinedTable, aes(x=number_of_arms,color=multi_arm,fill=multi_arm)) +
  geom_histogram(binwidth=1,alpha=0.5) +
  labs(x = "Number of Arms",y="Count",fill='Control Arm Status') +
  coord_cartesian(xlim=c(0,max(joinedTable$number_of_arms,na.rm = TRUE)))  + 
  guides(color=FALSE)
print(pHist)
if (savePlot){
  ggsave("trialsByYearHist_5_21_2020.png", units="in", width=5, height=4, dpi=600)
}

