# load packages and custom functions --------------------------------------


source("00_packages_and_functions.R")

#2.1.1 - Reading SPSS data ----------------------------------------------


# Loads in your SPSS file under a dataframe named 'TELSdata'
# You will need to replace the file name and file extension (i.e. .sav in the case of
# an SPSS file).


TELSdata <- haven::read_spss("data/TELS_DUMMY_DATA.sav"
                             #ensures that user-defined missing values in the spss file
                             # are read into R as NA
                             # If you want to bring in these codes into analysis (e.g. you might be interested
                             # in the share who refused) change to TRUE.
                             , user_na=FALSE)


# 2.2.2- Labelling SPSS data -----------------------------------

# This section is for information only.

#Label SPSS datasets so that you are using labels instead of codes.
# This dataset format is not used elsewhere in this script.
TELSdata_labelled <- haven::as_factor(TELSdata)



#  2.2.3 - Data dictionaries ---------------------------------------------------


# Display the data dictionary in the R studio viewer

# This will open the dictionary in your browser (which you could save to your folder as html)
# and also displays the no. of missings for each variable

sjPlot::view_df(TELSdata %>%
                  #make sure that the weight variable is numeric
                  dplyr::mutate(TechEd_W3_cross=as.numeric(TechEd_W3_cross)),
                #show the frequency of each variable
                show.frq = TRUE,
                #show the percentage of each variable
                show.prc = TRUE,
                #show the number of missings for each variable
                show.na=TRUE,
                #show the frequency of each variable weighted by the TechEd_W3_cross variable
                show.wtd.frq = TRUE,
                #show the percentage of each variable weighted by the TechEd_W3_cross variable
                show.wtd.prc = TRUE,
                #define weight
                weight.by = TechEd_W3_cross,
                #display output in browser instead of viewer pane in RStudio
                use.viewer=FALSE
)

# search for a variable based on the label
# Below, I'm searching for questions that have the word 'reason' in the label
#but you can change it to any term you're looking for

TELSdata %>%
  labelled::look_for("reas")




# 2.2.4 - Alternative data dictionary ---------------------------------------

# data dictionary in a wide format
# create a dataframe, which can be exported to Excel
TELSdata_dic <- TELSdata %>%
  #create a data dictionary
  labelled::look_for(details = TRUE) %>%
  #convert named list columns to character vectors so they're easier to read
  labelled::convert_list_columns_to_character()

# Saves it as a csv in your work folder

data.table::fwrite(TELSdata_dic, file="TELSdata_dic.csv")

# If you want to properly interrogate the value labels, it's better to use the long format below
# since each value label is assigned its own row in the table.
TELSdata_dic2 <- TELSdata %>%
  #create a data dictionary
  labelled::look_for(details = TRUE) %>%
  #convert data to long format
  labelled::lookfor_to_long_format() %>%
  #convert named list columns to character vectors so they're easier to read
  labelled::convert_list_columns_to_character()

# Saves it as a csv in your work folder
data.table::fwrite(TELSdata_dic2, file="TELSdata_dic2.csv")

#  2.3.1 - Reading CSV data ----------------------------------------------------


# If you have csv data, such as PPLV, you can read it in using the following:

#Read in the csv data

pplv_data <- data.table::fread("data/pplv_dummy_data.csv")


# 2.3.2 -   Labelling CSV data manually ----------------------------------------------------

#check labels in a column before assigning labels to codes- you should just see numbers and NAs
#you can do any col but we picked futurestudy_consider_bachelors
pplv_data$futurestudy_consider_bachelors

# Since CSV data does not contain metadata or labels, we need to add these manually.
# You can do this using the following:

#define the labels for each variable you want to label

future_study_labels <- c("Don't know" = -1,
                         "Very likely" = 1,
                         "Fairly likely" = 2,
                         "Not very likely" = 3,
                         "Not at all likely" = 4,
                         "I've never heard of this type of qualification" = 5
)

futurestudy_HTQ_labels <- c("Don't know" = -1,
                            "A lot" = 1,
                            "A little" = 2,
                            "Only heard of the name" = 3,
                            "Never heard of them" = 4
)

futurestudy_plan_labels <- c("Don't know" = -1,
                             "Yes - I'm definitely planning on doing this" = 1,
                             "Yes - I'm considering this along with other options" = 2,
                             "No - but I considered this" = 3,
                             "No - and I did not consider this" = 4,
                             "I've never heard of this type of qualification" = 5
)

#mutate the columns for the specific varibles to re-label the data
pplv_data <- pplv_data %>%
  dplyr::mutate(
    #relabelling futurestudy_consider_bachelors column with future_study_labels
    futurestudy_consider_bachelors =
      haven::labelled(futurestudy_consider_bachelors
                      , labels = future_study_labels
      ),
    #relabelling futurestudy_HTQaware_rebase column with futurestudy_HTQ_labels
    futurestudy_HTQaware_rebase =
      haven::labelled(futurestudy_HTQaware_rebase
                      , labels = futurestudy_HTQ_labels
      ),
    #relabelling futurestudy_plan_level45 column with futurestudy_plan_labels
    futurestudy_plan_level45 =
      haven::labelled(futurestudy_plan_level45
                      , labels = futurestudy_plan_labels
      ) )

#check labels after assigning labels to codes- you should see the labels under the numbers in the console

pplv_data$futurestudy_consider_bachelors

# If you have multiple variables with the same response scale, you can add labels
# using a combination of mutate() and across():

# First you define a list that contains the response list and codes
labels <- c("Don't know" = -1,
            "Very likely" = 1,
            "Fairly likely" = 2,
            "Not very likely" = 3,
            "Not at all likely" = 4,
            "I've never heard of this type of qualification" = 5)

# Then you define a list with the names of the variables you want to add labels for
variables <- c("futurestudy_consider_bachelors",
               "futurestudy_consider_traineeship",
               "futurestudy_consider_appren",
               "futurestudy_consider_degappren",
               "futurestudy_consider_level45")


# Then you use mutate across to label multiple variables at once,
# To adapt this, you need to change the labels and variables above.

pplv_data <- pplv_data %>%
  dplyr::mutate(dplyr::across(all_of(variables)
                                        , ~ haven::labelled(., labels=labels)))




# 2.3.3  Labelling the data automatically using a dictionary -----------------------


#read in data
pplv_data <- data.table::fread("data/pplv_dummy_data.csv")

#read in dictionary file
pplv_data_dic <- data.table::fread("data/2024-02_PPLV_PupilsLearners_NS_Dic.csv")


# clean the dictionary file

pplv_data_dic <- pplv_data_dic %>%
  #format NA values in Value_code and value_label
  dplyr::mutate(Value_code=if_else(Value_code == "n/a", NA_character_,Value_code) ) %>%
  dplyr::mutate(Value_label=if_else(Value_label == "n/a", NA_character_, Value_label)) %>%
  #ensure that the numeric codes are in the correct format
  dplyr::mutate(Value_coden=as.numeric(as.character(Value_code)) )


# Using labelling_data()
# This function will check your dictionary and data for matching
#variable names. if there is a match, data will be re-labelled.

# This function will only work if the variable names in the dictionary and the
# data frame match and  if your dictionary is formatted properly.
# If there is a code in the data frame without a label in the
# dictionary, that code will be returned as is in the result.

#View the un-labelled data set
View(pplv_data)

#check data before assigning labels to codes- you should just see numbers and NAs

pplv_data$futurestudy_consider_bachelors

#assign labels the codes

pplv_data<- labelling_data(
  #name of the data to be labelled
  data= pplv_data,
  #name of the dictionary
  dict =pplv_data_dic,
  #the column where the names of the variables are stored in the dictionary.
  dict_variable= "Variable",
  #the column where the labels are stored in the dictionary.
  dict_label= "Value_label",
  # the column where the codes are stored in the dictionary.
  dict_code= "Value_code",
  # the column where questions are stored in the dictionary.
  dict_question = "Question text")



# test that the assigning labels to codes worked - you should see the labels under the numbers in the console
pplv_data$futurestudy_consider_bachelors

 #create a labelled version of the data

pplv_data_labelled <- haven::as_factor(pplv_data)
 #View the labelled data set
 # you should see labels instead of codes and that the columns have been labelled with text
 View(pplv_data_labelled)

