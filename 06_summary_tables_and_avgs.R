
# 6.1 - Load packages, functions and read in data -------------------------



# load packages and custom functions --------------------------------------


source("00_packages_and_functions.R")


# read in data for this script --------------------------------------------


TELSdata <- haven::read_spss("data/TELS_DUMMY_DATA.sav"
                             #ensures that user-defined missing values in the spss file
                             # are read into R as NA
                             # If you want to bring in these codes into analysis (e.g. you might be interested
                             # in the share who refused) change to TRUE.
                             , user_na=FALSE)
# 6.2 - Summary tables -----------------------------------------------------------


# 6.2.1 - Single variable -------------------------------------------------


# create a summary table that gives you the mean, max, min etc. for Age

st_age <- create_summary_table(data=TELSdata,
                               variables ="FF_Age2022"
                               , weight = "TechEd_W3_cross" )


# 6.2.2 - Multiple variables ----------------------------------------------


# List of variables to loop through
variables <- c("FF_Sex", "FF_Ethnic5", "SEN_DV", "FSM_DV")

create_summary_table(data=TELSdata,
                     variables =variables
                     , weight = "TechEd_W3_cross",
                     output = "print")


# 6.2.3 - Un-weighted data -------------------------------------------------------------

# List of variables to loop through
variables <- c("FF_Sex", "FF_Ethnic5", "SEN_DV", "FSM_DV")

create_summary_table(data=TELSdata,
                     variables =variables,
                     output = "print")


# 6.3 - Weighted averages -------------------------------------------------------


# 6.3.1 - Mean ------------------------------------------------------------


#calculate weighted mean for currentsit

weighted_avg(data = TELSdata,
             x="CurrentSit",
             type="mean",
             weight ="TechEd_W3_cross",
             na.rm=TRUE )

#  6.3.2 - Mean by group ----------------------------------------------
#calculate weighted mean for currentsit by sex

weighted_avg(data = TELSdata,
             x="CurrentSit",
             type="avg by grp",
             weight ="TechEd_W3_cross",
             by ="FF_Sex" ,
             na.rm=TRUE
)


# 6.3.3 - Median -------------------------------------------------

# calculate weighted median for currentsit


weighted_avg(data = TELSdata,
             x="CurrentSit",
             #outline type of average
             type="median",
             weight ="TechEd_W3_cross",
             #set it at 0.5 (50% quantile)
             quantile=0.5,
             ci=FALSE)

# adjust quantile figure to calculate other quantiles or deciles, e.g. 10%

weighted_avg(data = TELSdata,
             x="CurrentSit",
             #outline type of average
             type="median",
             weight ="TechEd_W3_cross",
             #set it for 10%
             quantile=0.1,
             ci=FALSE)

# 6.4 - Combine the learning ---------------------------------------
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

#create summary table for the derived variable
create_summary_table(data=TELSdata,
                     variables ="Ful_recomm",
                     output = "print")

#calculate weighted mean for the derived variable

weighted_avg(data = TELSdata,
             x="Ful_recomm",
             type="mean",
             weight ="TechEd_W3_cross" )
