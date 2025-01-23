# 8.1 - Load packages, functions and read in data -------------------------

# load packages and custom functions --------------------------------------


source("00_packages_and_functions.R")


# read in data for this script --------------------------------------------


TELSdata <- haven::read_spss("data/TELS_DUMMY_DATA.sav"
                             #ensures that user-defined missing values in the spss file
                             # are read into R as NA
                             # If you want to bring in these codes into analysis (e.g. you might be interested
                             # in the share who refused) change to TRUE.
                             , user_na=FALSE)


# 8.2 Suitability of data -------------------------------------------------


# Check value codes are appropriate for correlation analysis

unique(TELSdata$Progress)
unique(TELSdata$PrepareCareer)

# re-code responses. negative responses = negative no
# positive repose = positive no
# neutral as 0

TELSdata <- TELSdata %>%
  dplyr::mutate(
    #re-code progress
    Progress_recode= dplyr::case_when(
      Progress == 5 ~ -2,
      Progress == 4 ~ -1,
      Progress == 3 ~ 0,
      Progress == 2 ~ 1,
      Progress == 1 ~ 2),
    #re-code preparecareer
    PrepareCareer_recode=case_when(
      PrepareCareer == 5 ~ -2,
      PrepareCareer == 4 ~ -1,
      PrepareCareer == 3 ~ 0,
      PrepareCareer == 2 ~ 1,
      PrepareCareer == 1 ~ 2)
  )

#  8.3 Spearman’s Rho -------------------------------------------------

# most appropriate correlation method for most survey data

cor_test <- stats::cor.test(
  # specify the variables you want to test for correlation
  TELSdata$Progress_recode,
  TELSdata$PrepareCareer_recode,
  #indicate the alternative hypothesis
  alternative = "two.sided",
  # specify method
  method =  "spearman",
  #  specify confidence level
  conf.level = 0.95,
  exact=FALSE)

#view results in the console
cor_test

# 8.4 Kendall’s Tau -------------------------------------------------

# Kendall's is similar to Spearman's but is better suited to
#small sample sizes, 'tied ranks' and outliers

cor_test <- stats::cor.test(
  # specify the values you want to test for correlation
  TELSdata$Progress_recode,
  TELSdata$PrepareCareer_recode,
  #indicate the alternative hypothesis
  alternative = "two.sided",
  # specify method
  method =  "kendall",
  #  specify confidence level
  conf.level = 0.95)

#view results in the console
cor_test

# 8.5 Pearson’s ------------------------------------------------------

#check that the dependent variable (age) has a numeric data type
class(TELSdata$FF_Age2022)

#change the data type from character to numeric (to reflect continuous values)

TELSdata <- TELSdata %>%
  dplyr::mutate(FF_Age2022=as.numeric(FF_Age2022))


#create survey design object that outlines weights
weight <- survey::svydesign(ids = ~1,
                            weights = ~TechEd_W3_cross,
                            data = TELSdata)


#store the details in an object
corr <- jtools::svycor(~FF_Age2022 + income,
                       design = weight,
                       #removes NAs from the calculation
                       na.rm=TRUE,
                       #provide all details not just coefficient
                       sig.stats=TRUE)

#extract correlation coefficient
corr$cors
#extract p value
corr$p.values
#extract std error
corr$std.err


# 8.6 Combine the learning ----------------------------------------------------

# do a correlation test on a filtered data set and we will be filtering using
# a derived variable

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

#create a filtered data set for fulfilled and recommend
tels_ful_recomm <- TELSdata %>%
  dplyr::filter(Ful_recomm==1)

#carry out correlation test on the filtered data set to test correlation between
# Progress_recode and PrepareCareer_recode
cor_test <- stats::cor.test(
  # specify the variables you want to test for correlation
  tels_ful_recomm$Progress_recode,
  tels_ful_recomm$PrepareCareer_recode,
  #indicate the alternative hypothesis
  alternative = "two.sided",
  # specify method
  method =  "spearman",
  #  specify confidence level
  conf.level = 0.95,
  exact=FALSE)

#view results in the console
cor_test
