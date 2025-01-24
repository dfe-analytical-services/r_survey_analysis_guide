
# 4.1 - load packages and custom functions --------------------------------------


source("00_packages_and_functions.R")


# 4.1 -  read in data for this script --------------------------------------------


TELSdata <- haven::read_spss("data/TELS_DUMMY_DATA.sav"
                             #ensures that user-defined missing values in the spss file
                             # are read into R as NA
                             # If you want to bring in these codes into analysis (e.g. you might be interested
                             # in the share who refused) change to TRUE.
                             , user_na=FALSE)


# 4.2 - survey_freq_table() - uses survey package ----------------------------------------------------------------



# 4.2.1 - Weighted frequency tables  ---------------------------------------------------------------


# single variable ---------------------------------------------------------

#save to environment - single var only

sex_freq_table <- survey_freq_table(data=TELSdata,
                                    variables = "FF_Sex",
                                    weight="TechEd_W3_cross")

#print
survey_freq_table(data=TELSdata,
                  variables = "FF_Sex",
                  weight="TechEd_W3_cross",
                  output = "print")

#download

survey_freq_table(data=TELSdata,
                  variables = "FF_Sex",
                  weight="TechEd_W3_cross",
                  output = "download")


# multiple variables -------------------------------------------------------
# List of variables
vars <- c("FF_Sex", "DV_Ethnicity", "SEN_DV", "WrkStud", "NextStepStudyGeneralField")

#print
survey_freq_table(data=TELSdata,
                  variables = vars,
                  weight="TechEd_W3_cross",
                  output = "print")



#download

survey_freq_table(data=TELSdata,
                  variables = vars,
                  weight="TechEd_W3_cross",
                  output = "download")


# 4.3 - Unweighted data --------------------------------------------------


# unweighted - survey_freq_table()-------------------------------------------------------------


#create an un-weighted frequency table for the variable 'WrkStud
survey_freq_table(data=TELSdata,
                  variables = "WrkStud",
                  output = "print")


# 4.4 - Combine the learning ---------------------------------------

#create a frequency table of "WrkStud" for males only with a moe

# create a new dataset of males

TELSdata_male <- TELSdata %>%
  filter(FF_Sex==2)

#freq table for the filtered dataset we just created.

survey_freq_table(TELSdata_male,
                 variables="WrkStud",
                 weight="TechEd_W3_cross",
                 output = "print")






