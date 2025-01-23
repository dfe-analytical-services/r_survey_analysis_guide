
# 3.1 - Load packages, functions and read in data -------------------------


# load packages and custom functions --------------------------------------


source("00_packages_and_functions.R")


# read in data for this script --------------------------------------------


TELSdata <- haven::read_spss("data/TELS_DUMMY_DATA.sav"
                             #ensures that user-defined missing values in the spss file
                             # are read into R as NA
                             # If you want to bring in these codes into analysis (e.g. you might be interested
                             # in the share who refused) change to TRUE.
                             , user_na=FALSE)

#3.2 -  Re-coding ---------------------------------------------------



#  Simple re-codes ----------------------------------------------------


#  run a frequency of the variable to see how results look and what
# the value code / labels are.

survey_freq_table(TELSdata,
                  variable="Grade",
                  weight="TechEd_W3_cross")

# In this example, I may want to recode the Grade variable so that I have a 'pass'
# and 'fail'. To do this I will need to recode 1-4 (pass) into 1, and 5 (fail) into 0.
# everything else will be NA


#example of recoding using case_when
TELSdata <- TELSdata %>%
  mutate(Grade_recode=case_when(
    # if 1 or 2 are in Grade, code as 1 in Grade_recode
    Grade %in% c(1,2) ~ 1,
    # if 3 or 4 are in Grade, code as 2 in Grade_recode
    Grade %in% c(3,4) ~ 2))



# Adds value labels

TELSdata <- TELSdata %>%
  dplyr::mutate(Grade_recode= haven::labelled(Grade_recode
                                              #label based on code
                                              , labels= c("Pass/merit"=1,
                                                          "Distinction/starred distinction"=2)))

# Check if the recode and labelling looks about right.

survey_freq_table(TELSdata,
                  variable="Grade_recode",
                  weight="TechEd_W3_cross")

# Cross tab with original variable to confirm.

survey_crosstab(TELSdata,
                   x="Grade",
                   y="Grade_recode",
                   weight="TechEd_W3_cross")



# 3.3 - Deriving variables -------------------------------------------------

#check the values in currentsit
unique(TELSdata$CurrentSit)

#check the values in recommend
unique(TELSdata$Recommend)

#create derived variable
TELSdata <- TELSdata %>%
  dplyr::mutate(Ful_recomm=case_when(
    CurrentSit %in% c(1,2) & Recommend %in% c(1,2) ~ 1,
    CurrentSit %in% c(1,2) & Recommend %in% c(4,5) ~ 2,
    CurrentSit %in% c(4,5) & Recommend %in% c(1,2) ~ 3,
    TRUE ~ 4))

#label the derived variable column
TELSdata <- TELSdata %>%
  dplyr::mutate(Ful_recomm= haven::labelled(Ful_recomm,
                                            labels=c("Fulfilled and recommend" = 1,
                                                     "Fulfilled but don't recommend" = 2,
                                                     "Not Fulfilled and don't recommend" = 3,
                                                     "Other" = 4
                                            ) ))

# Check the recode.

survey_freq_table(TELSdata,
                  variable="Ful_recomm",
                  weight="TechEd_W3_cross")



# 3.4 - Missing values ----------------------------------------------------------


#check the values in InstitutionAwareness
TELSdata %>%
  dplyr::distinct(InstitutionAwareness)

TELSdata <- TELSdata %>%
  dplyr::mutate(InstitutionAwareness_recode = dplyr::na_if(InstitutionAwareness, 5))

#check the values in InstitutionAwareness after the change
# you should not be able to see 5 anymore as it has been grouped with NA

TELSdata %>%
  dplyr::distinct(InstitutionAwareness_recode)


# 3.5 - Filtering ---------------------------------------------------------------

#check values for sex

TELSdata %>%
  dplyr::count(FF_Sex)

# create a new dataset of males - you should see that the count is the same as the
# the total for males in the output above

TELSdata_male <- TELSdata %>%
  filter(FF_Sex==2)

# This runs a frequency table on the new filtered dataset we just created.

survey_freq_table(TELSdata_male,
                variable="WrkStud",
                weight="TechEd_W3_cross")





