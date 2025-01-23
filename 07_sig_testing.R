# 7.1 - Load packages, functions and read in data -------------------------

# load packages and custom functions --------------------------------------


source("00_packages_and_functions.R")


# read in data for this script --------------------------------------------


TELSdata <- haven::read_spss("data/TELS_DUMMY_DATA.sav"
                             #ensures that user-defined missing values in the spss file
                             # are read into R as NA
                             # If you want to bring in these codes into analysis (e.g. you might be interested
                             # in the share who refused) change to TRUE.
                             , user_na=FALSE)




#7.2 Non-parametric tests - appropriate for most data --------------------------------------------



#  7.2.1 Chi-squared --------------------------------------------------

#re-coding currentsit so it's two categories instead of 4

#for original data
TELSdata <- TELSdata %>%
  dplyr::mutate(CurrentSit_recode=dplyr::case_when(
    CurrentSit %in% c(1,2) ~ 1,
    CurrentSit %in% c(4,5) ~ 2))



#do a chi-squared test on currentsit and fsm_dv
chisq <- sjstats::chi_squared_test(TELSdata,
                                   select = "CurrentSit_recode",
                                   by ="FSM_DV",
                                   weights= "TechEd_W3_cross")

#view results in the console
chisq



# If you have a longitudinal dataset do a paired test to see if
# views have changed over time.

chisq <- sjstats::chi_squared_test(TELSdata,
                                   select = "CurrentSit_recode",
                                   #define groups for paired test
                                   #note this column does not exist in the data
                                   by ="wave",
                                   weights= "TechEd_W3_cross",
                                   #do a paired test
                                   paired=TRUE)


#view results in the console
chisq

#  7.2.2 Mann-Whitney U / Kruskal-Wallis -------------------------------------

#check how many levels (categories) there are for FF_SEX in the data

unique(TELSdata$FF_Sex) # there are more than 2

# Use mutate to convert FF_Sex to a factor and drop unused levels (categories) for sex
TELSdata <- TELSdata %>%
  dplyr::mutate(FF_Sex = droplevels(as.factor(FF_Sex)))

# comparing ordinal data across 2 groups for two independent samples


ranktest1 <- sjstats::mann_whitney_test(TELSdata,
                                        select="CurrentSit",
                                        by="FF_Sex",
                                        weights = "TechEd_W3_cross")
#view results in the console
ranktest1


# If your group variable has more than two categories, use the Kruskall-Wallis

#create survey design object that outlines weights
weight <- survey::svydesign(
  #- ~1 means that there is a single constant term
  #i.e. each observation is treated as a separate
  # sampling unit with equal weight.

  ids = ~1,
  #outline the weight variable
  weights = ~TechEd_W3_cross,
  data = TELSdata)

kruskalwallis <- survey::svyranktest(
  # The first variable in the formula is the dependent variable
  # the second is the independent variable or groups you want to test.
  formula = FF_Age2022 ~ TLPathway_Str,
  #use survey design object to define weight
  design=weight,
  # specify test
  test="KruskalWallis")

#view results in the console
kruskalwallis

# 7.2.3 Wilcoxon --------------------------------------

# The Wilcoxon test is like the Mann-Whitney U test but for paired samples

#wilcoxon without weights
wilcoxon <- sjstats::wilcoxon_test(TELSdata,
                                   select = "CurrentSit",
                                   by = "wave"
)

#view results in the console
wilcoxon

#wilcoxon with weights

design <- survey::svydesign(ids = ~1,
                            data = TELSdata,
                            weights = ~TechEd_W3_cross)

wilcoxon_weighted <- survey::svyranktest(CurrentSit ~ wave,
                    design,
                    test = "wilcoxon")

#view results in the console
wilcoxon_weighted



# 7.3 Parametric tests - appropriate in the minority of cases ------------------------------------------------



# 7.3.1 T-test -------------------------------------------------------

#Independent Samples t-test

#check that the dependent variable (age) has a numeric data type
class(TELSdata$FF_Age2022)

#change the data type from character to numeric

TELSdata <- TELSdata %>%
  dplyr::mutate(FF_Age2022=as.numeric(FF_Age2022))

# test whether male and females are significantly different
# from each other in age

ttest <- sjstats::t_test(TELSdata,
                         #define test/dependent variable
                         #this should be continuous
                         select = "FF_Age2022",
                         #define the groups we want to test
                         by = "FF_Sex",
                         #give column name for the weight
                         weights = "TechEd_W3_cross")
#view the results
ttest

#Paired Samples t-test

# for longitudinal data sets when you want to see if there
#is a difference over two time periods


ttest2 <- sjstats::t_test(TELSdata,
                          select = "income",
                          #split by variable that has the groups
                          by = "wave",
                          weights = "TechEd_W3_cross",
                          #set paired to true
                          paired=TRUE)

#view results in the console
ttest2

#One-Sample t-test
# compare the independent variable to a known value or the population mean
#we're comparing the income against its mean

# Perform one-sample t-test
result <- sjstats::t_test(TELSdata,
                          select = "income",
                          mu = 50052.28)
#view results in the console
result

#  7.3.2 Z-test -------------------------------------------------------


# determine if two proportions in large samples are significantly different
# using dummy data

# outlining weighted count for first group
p1<-86
# outlining sample size for first group
n1<-185
# outlining weighted count for second group
p2<-528
# outlining sample size for second group
n2<-948

result <-stats::prop.test(
  #give weighted counts for each group
  c(p1,p2),
  #give the sample size for each group
  n=c(n1,n2))

#print result in the console
result
#extract z statistic
zstatistic <-result$statistic


# 7.4 Combine the learning ----------------------------------------------------

# carry out a chi test for results from respondents that
# have finished or are doing t-levels only

#filter the data
tels_t_level <- TELSdata %>%
  dplyr::filter(FinishTLevel %in% c(1,2))

#do a chi-squared test on currentsit and fsm_dv
chisq <- sjstats::chi_squared_test(tels_t_level,
                                   select = "CurrentSit_recode",
                                   by ="FSM_DV",
                                   weights= "TechEd_W3_cross")

#view results in the console
chisq
