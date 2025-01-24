
# 5.1 - Load packages, functions and read in data -------------------------


# load packages and custom functions --------------------------------------


source("00_packages_and_functions.R")


# read in data for this script --------------------------------------------


TELSdata <- haven::read_spss("data/TELS_DUMMY_DATA.sav"
                             #ensures that user-defined missing values in the spss file
                             # are read into R as NA
                             # If you want to bring in these codes into analysis (e.g. you might be interested
                             # in the share who refused) change to TRUE.
                             , user_na=FALSE)


# 5.2 - survey_crosstab() - uses the survey package ----------------------------------------------------------------


# weighted  ---------------------------------------------------------------

# 5.2.1 - Single variables---------------------------------------------------------

#two way cross tabs - create crosstab of FF_Sex and WrkStud

# Notice that the value in the column n_FF_Sex is the same for each sex. This is
# because we need this to divide the count of that cell by to get the proportion

#save to environment

sex_work_stud_cross <- survey_crosstab(
  #outline data
  data = TELSdata,
  #outline independent variable of crosstab
  x = "FF_Sex",
  #outline dependent variable of crosstab
  y = "WrkStud",
  #outline weight variable
  weight = "TechEd_W3_cross"
)


#print
survey_crosstab(
  #outline data
  data = TELSdata,
  #outline independent variable of crosstab
  x = "FF_Sex",
  #outline dependent variable of crosstab
  y = "WrkStud",
  #outline weight variable
  weight = "TechEd_W3_cross",
  #outline the output desired
  output = "print"
)

#download
survey_crosstab(
  #outline data
  data = TELSdata,
  #outline independent variable of crosstab
  x = "FF_Sex",
  #outline dependent variable of crosstab
  y = "WrkStud",
  #outline weight variable
  weight = "TechEd_W3_cross",
  #outline the output desired
  output = "download"
)


# three way cross tabs - FF_Sex, WrkStud and DV_Ethnicity

#print
survey_crosstab(
  #outline data
  data = TELSdata,
  #outline independent variable of crosstab
  x = "FF_Sex",
  #outline control variable
  z= "DV_Ethnicity",
  #outline dependent variable of crosstab
  y = "WrkStud",
  #outline weight variable
  weight = "TechEd_W3_cross",
  #outline the output desired
  output = "print"
)

# 5.2.2 - Multiple variables  -----------------------------------------------------

# List of variables
vars <- c("WrkStud","DV_Ethnicity", "SEN_DV",  "NextStepStudyGeneralField")
#create two way cross tabulations of FF_sex and the listed vars above

#print
survey_crosstab(
  #outline data
  data = TELSdata,
  #outline independent variable of crosstab
  x = "FF_Sex",
  #outline dependent variable of crosstab
  y =vars,
  #outline weight variable
  weight = "TechEd_W3_cross",
  #outline the output desired
  output = "print"
)

#three way cross tabs
# List of variables
vars <- c("DV_Ethnicity", "SEN_DV",  "NextStepStudyGeneralField")

#create three way cross tabulations of FF_sex and the listed vars above
#print
survey_crosstab(
  #outline data
  data = TELSdata,
  #outline independent variable of crosstab
  x = "FF_Sex",
  #outline control variable of crosstab
  z= "WrkStud",
  #outline dependent variable of crosstab
  y = vars,
  #outline weight variable
  weight = "TechEd_W3_cross",
  #outline the output desired
  output = "print"
)

#download
survey_crosstab(
  #outline data
  data = TELSdata,
  #outline independent variable of crosstab
  x = "FF_Sex",
  #outline control variable of crosstab
  z="WrkStud",
  #outline dependent variable of crosstab
  y = vars,
  #outline weight variable
  weight = "TechEd_W3_cross",
  #outline the output desired
  output = "download"
)


# 5.3 - Unweighted data -------------------------------------------------------------
#do this by leaving the weight parameter undefined



# single variable ---------------------------------------------------------

#print
survey_crosstab(
  #outline data
  data = TELSdata,
  #outline independent variable of crosstab
  x = "FF_Sex",
  #outline control variable of crosstab
  z= "DV_Ethnicity",
  #outline dependent variable of crosstab
  y = "WrkStud",
  #outline the output desired
  output = "print"
)


# unweighted three way cross tabs - FF_Sex, WrkStud and multiple variables

survey_crosstab(
  #outline data
  data = TELSdata,
  #outline independent variable of crosstab
  x = "FF_Sex",
  #outline control variable of crosstab
  z= "WrkStud",
  #outline dependent variable of crosstab
  y = vars,
  #outline the output desired
  output = "print"
)

# 5.4 - Combine the learning ---------------------------------------
#create a crosstab of recoded grade values and sex

#example of recoding using case_when
TELSdata <- TELSdata %>%
  mutate(Grade_recode=case_when(
    Grade %in% c(1,2,3,4) ~ 1,
    Grade == 5 ~ 0))



# Adds value labels

TELSdata <- TELSdata %>%
  dplyr::mutate(Grade_recode= haven::labelled(Grade_recode
                                              , labels= c("Fail"=0, "Pass"=1)))
#create the crosstab
survey_crosstab(TELSdata,
                x="Grade_recode",
                y="FF_Sex",
                output="print")

