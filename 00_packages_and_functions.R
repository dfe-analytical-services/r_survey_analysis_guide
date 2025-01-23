# Loading libraries -------------------------------------------------------

# 'Library' loads the installed packages into your console to be used in a specific session.
# You'd usually need to run the library commands every time you open R.

library ("dplyr")
library ("labelled")
library ("sjlabelled")
library ("haven")
library ("survey")
library("pollster")
library("sjstats")
library ("sjPlot")
library ("jtools")
library("rlang")
library("janitor")
library("coin")
library("weights")

# custom functions --------------------------------------------------------


# output checks -----------------------------------------------------------

#' Helper function that checks the arguments provided for the parameter "output"
#'
#' @param output the variable inputted for the "output" arguments in other functions

output_check <- function(output){

  if (!any(output %in% c("download", "print") || is.null(output))) {
    stop(
      "You provided an invalid argument for the output argument. Please use NULL, 'download' or 'print'"
    )

  }

}

#' Helper function that checks that only one variable is used when output is set to NULL
#' This is needed because we can't save more than one object to the environment from one function
#'
#' @param output the variable inputted for the "output" arguments in other functions
#' @param variables the name of the variable that you want to check the length for

output_enviro_check <- function(output, variables){
  if (is.null(output) & length(variables) > 1) {
    stop(
      "You provided more than one variable without specifying 'print' or 'download' for the output argument.
      Please choose one of 'print' or 'download' for the output argument as it is not possible to store multiple tables using one function."
    )
  }

}

# data frame format-er ----------------------------------------------------

#' Helper function that formats data frames that are generated when output=NULL
#' The column names are converted to snake_case so that the columns are easier to
#' deal with for further analysis
#'
#' @param table_name the table you wish to format

create_data_frame <- function(table_name){

  result <- as.data.frame(table_name)

  result <- result %>%
    janitor::clean_names()

  return(result)

}


# weight checker ----------------------------------------------------------

create_weight <- function(data,weight){

  if(!is.null(weight)){
    data <- data
  }else{

    data <- data %>%
      dplyr::mutate(dummy_wt=1)
  }
}

# quoted variable checker -------------------------------------------------

###NICE TO HAVE


# print helper function ---------------------------------------------------


#' Helper function that loops through printing lines that describe the tables
#' being printed in the console and the tables that fall under that description
#'
#' @param print_line the descriptive lines that the function needs
#' to loop through for printing
#' @param result the table that the function needs to loop through for printing


print_helper <- function(print_line, result) {

  max_length <- max(length(print_line), length(result))

  # loop through the lists and print elements alternately
  for (i in 1:max_length) {
    if (i <= length(print_line)) {
      print(print_line[[i]])
    }
    if (i <= length(result)) {
      print(result[[i]])
    }
  }


}


# table name function --------------------------------------------------------

#' Helper function that determines the name given to tables if the output option
#' is set to "download" in a function.
#'
#' @param table_name the custom name provided by the user
#' @param default a default name for the function to fall back on if the user
#' does not specify a name.


get_table_name <- function(table_name, default = NULL) {
  if (is.null(default)) {
    default <- "table"
  }

  # deciding the name of the table
  #if null, then assign a default name
  if (is.null(table_name)) {
    table_name <- paste(default, ".xlsx")
    return(table_name)

    #else use the name specified
  } else{
    table_name <- paste(table_name, ".xlsx")
    return(table_name)
  }

}


# function for labelling data using dictionaries --------------------------


#' Labels data
#'
#' @param data Data to be labelled
#' @param dict A dictionary data frame to use for labelling data
#' @param dict_variable  The name of the column that contains the names of label
#'  variables in the dictionary.
#' @param dict_label  The name of the column that contains the labels
#' in the dictionary.
#' @param dict_code  The name of the column that contains the code in
#' the dictionary.
#' @param dict_question  The name of the column in the dictionary that contains
#' the question that you can use to label variables in the data.
#' @returns A labelled data frame.
#' @examples


labelling_data <- function(data,
                           dict,
                           dict_variable,
                           dict_label,
                           dict_code,
                           dict_question=NULL) {

  #convert all variables to character

  dict <- dict %>%
    dplyr::mutate(across(all_of(c(dict_code, dict_label, dict_question)), ~as.character(.)))


  #check if the variables and codes matches in data and dictionary
  match_check <- data %>%
    #get rid of dupes to improve processing time
    dplyr::distinct() %>%
    #make sure data type is the same in both data sets for the join to work
    dplyr::mutate(across(everything(), ~as.character(.))) %>%
    #pivot longer for the join to work
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "variables",
                        values_to = "code") %>%
    #remove dupes
    dplyr::distinct() %>%
    #do anti join to find non matches
    dplyr::anti_join(dict %>%
                       #make sure data type is the same in both data sets for the join to work
                       dplyr::mutate(across(all_of(c(dict_variable, dict_code)), ~as.character(.))),
                     by = c("variables" = dict_variable, "code" = dict_code))

  # If there are any that do not match, give a warning with a list
  if (nrow(match_check) > 0) {
    message("Warning: The following variables and codes are found in the data but not in the dictionary")
    message(paste0(capture.output(match_check), collapse = "\n"))
  }

  # Loop through each variable in the data dictionary
  for (i in unique(dict[[dict_variable]])) {
    # Check if the variable exists in the data frame
    if (i %in% names(data)) {
      # Subset the data dictionary for the current variable
      dic_subset <- dict %>%
        filter(!!sym(dict_variable) == i)

      # Convert the variable to character for labelling to work
      data[[i]] <- as.character(data[[i]])

      # Create a named vector of value labels
      value_labels <- setNames(dic_subset[[dict_code]], dic_subset[[dict_label]])

      # Get the unique codes in the data that are not in the dictionary
      unmatched_codes <- setdiff(unique(data[[i]]), names(value_labels))

      # Assign the value labels to the variable in the data frame using haven::labelled
      data[[i]] <- labelled(
        data[[i]],
        labels = value_labels
      )
      # if question col is specified, assign the question label to the variable
     if(!is.null(dict_question)){
       #label variables with questions
      Hmisc::label(data[[i]]) <- as.character(unique(dic_subset[[dict_question]]))}
    }
  }

  return(data)
}



# survey freq function ----------------------------------------------------

#' Creates frquency tables
#'
#' @param data Data to create the frequency table from
#' @param variables The column name(s) for variable(s) you want the frequency table for. Must be in quotes.
#' @param weight  The name of the column that contains the weights. Must be in quotes.
#' @param output  The type of output you want from the function. The available options are NULL, "download" and "print".
#' NULL is the default and it allows you to save the table to the environment when you use the function for a single variable.
#' "print" prints the frequency tables in the console - can be used for single or multiple variables.
#' "download" downloads a xlsx with the frequency table of the specified variable and weights - can be used for single or multiple variables.
#' @param table_name  A string of the name you want for the table if you use the "download" output option.
#' If NULL, then the table downloaded will be called "frequency_table.xlsx"
#' @returns A frequency table..
#' @examples
#'

survey_freq_table <- function(data,
                              variables,
                              weight = NULL,
                              output = NULL,
                              table_name = NULL) {

  # if the output is not NULL, download or print, stop the function
  output_check(output)

  #if the option is NULL but there are multiple variables, stop the function
  output_enviro_check(output,variables)

  # if weight is not provided, create a col with the same number for all
  # vars and use it for weight

  data <- create_weight(data,
                        weight)
  if(is.null(weight)){

    weight <- "dummy_wt"
  }


  # convert variables to factors

  data <- data %>%
    dplyr::mutate(dplyr::across(all_of(variables), ~ haven::as_factor(.)))

  #create survey design object

  weight <- as.formula(paste("~", weight))



  sdo <- survey::svydesign(ids = ~ 1,
                           weights = weight,
                           data = data)


  result <- lapply(variables, function(i) {
    #table_formula <-

    survey::svytable(as.formula(paste("~", i)), design = sdo) %>%
      as.data.frame() %>%
      dplyr::mutate(Prop = Freq / sum(Freq)) %>%
      dplyr::arrange(desc(Prop)) %>%
      dplyr::filter(Prop>0)



  })
  #print(result)


  if (is.null(output)) {
    #return(as.data.frame(result))
    create_data_frame(result)
  } else if (output == "print") {
    print_lines <- lapply(variables, function(i) {
      paste0("Frequency table for ", i)
    })

    print_helper(print_line = print_lines, result = result)


  } else{
    # # deciding the name of the table

    table_name <- get_table_name(table_name, default = "frequency_table")
    #rename the dataframes in the list according to the variables.
    names(result) <- variables
    #write out the excel sheet
    writexl::write_xlsx(result, table_name)

  }



}


# crosstab -survey package ------------------------------------------------

#' Creates two way or three way cross tabs of data
#'
#' @param data Data to create the crosstab from
#' @param x The column name for the independent variable. Must be in quotes.
#' @param y The column name for the dependent variable. Must be in quotes.
#' @param z The column name for the second control variable. Must be in quotes.
#' @param weight  The name of the column that contains the weights. Must be in quotes.
#' @param output  The type of output you want from the function. The available options are NULL, "download" and "print".
#' NULL is the default and it allows you to save the table to the environment when you use the function for a single variable.
#' "print" prints the frequency tables in the console - can be used for single or multiple variables.
#' "download" downloads a xlsx with the frequency table of the specified variable and weights - can be used for single or multiple variables.
#' @param table_name  A string of the name you want for the table if you use the "download" output option.
#' If NULL, then the table downloaded will be called "crosstab_table.xlsx"
#' @returns A two or three way crosstab.
#' @examples
#'
survey_crosstab <- function(data ,
                            x ,
                            y ,
                            z = NULL,
                            weight= NULL,
                            output = NULL,
                            table_name = NULL) {

  # mutate x, y and z if it exists so that they are factors

  data <- data %>%
    dplyr::mutate(across(all_of(c(x,y,z)), ~ haven::as_factor(.)))

  # if weight is not provided, create a col with the same number for all
  # vars and use it for weight

  if(is.null(weight)){

    data <- create_weight(data,
                          weight)

    weight <- "dummy_wt"
  }

  weight <- as.formula(paste("~", weight))

  # if the output is not NULL, download or print, stop the function
  output_check(output)

  #if the option is NULL but there are multiple variables, stop the function
  output_enviro_check(output,y)
  output_enviro_check(output,z)


  #create a survey design object (weight)

  sdo <- survey::svydesign(ids = ~ 1,
                           weights = weight,
                           data = data)

# if no z is provided do a two way crosstab

  if (is.null(z)) {
    result <- lapply(y, function(i) {
      svytable(as.formula(paste("~", x, "+", i)), design =sdo) %>%
        as.data.frame() %>%
        group_by(!!sym(x)) %>%
        mutate(n = sum(Freq), Prop = Freq / n) %>%
        ungroup() %>%
        dplyr::filter(Freq>0) %>%
        dplyr::arrange(!!sym(x))
    })

    print_lines <- lapply(y, function(i) {
      paste0("Crostab for ", x, " and ", i)
    })

    #rename the dataframes in the list according to the variables.
    if (!is.null(output) && output == "download") {
      names(result) <- lapply(y, function(i) {
        paste(x, i, sep = "_")
      })
    }

  #if z is provided do a three way crosstab
  } else{

    #generate result
    result  <- lapply(y, function(i) {
      svytable(as.formula(paste("~", x, "+", i, "+", z)), design = sdo) %>%
        as.data.frame() %>%
        group_by(!!sym(x), !!sym(z)) %>%
        mutate(n = sum(Freq), Prop = Freq / n) %>%
        ungroup()%>%
        dplyr::filter(Freq>0)%>%
        #pivoting y wider for three way only to make it easier to read
        tidyr::pivot_wider(names_from = !!sym(i), values_from = c(Freq, Prop)) %>%
        dplyr::arrange(!!sym(x),!!sym(z))
    })
    print_lines <- lapply(z, function(i) {
      paste0("Crostab for ", x, ", ", i, " and ", z)
    })

    #rename the dataframes in the list according to the variables.
    if (!is.null(output) && output == "download") {
      base::names(result) <- lapply(y, function(i) {
        paste(x, i, z, sep = "_")
      })
    }
  }

  if (is.null(output)) {
    #return(as.data.frame(result))
    create_data_frame(result)
  } else if (output == "print") {
    print_helper(print_line = print_lines, result = result)
  } else{
    # deciding the name of the table

    table_name <- get_table_name(table_name, default = "survey_crosstab")

    #write out the excel sheet
    writexl::write_xlsx(result, table_name)
  }

}


# create summary tables ---------------------------------------------------

#' Creates weighted summary tables of data
#'
#' @param data Data to create the summary table from
#' @param variables The column name(s) for variable(s) you want the summary table for. Must be in quotes.
#'  These need to be numeric and not categorical data.
#' @param weight  The name of the column that contains the weighting variable. If NULL, table will be un-weighted.
#' @param output  The type of output you want from the function.
#' The available options are NULL, "download" and "print".
#' NULL is the default will allow you to save a single table in the environment. It cannot be used when you have more than one variable.
#' "print" prints the summary tables in the console.
#' "download" downloads a xlsx with the summary table of the specified variable and weights.
#' @param table_name  A string of the name you want for the table if you use the "download" output option.
#' If NULL, then the table downloaded will be called "summary_table.xlsx"
#' @returns A summary table.
#' @examples

create_summary_table <- function(data,
                                 variables,
                                 weight=NULL,
                                 output = NULL,
                                 table_name = NULL) {

  # if the output is not NULL, download or print, stop the function
  output_check(output)

  #if the option is NULL but there are multiple variables, stop the function
  output_enviro_check(output,variables)

  # if weight is not provided, create a col with the same number for all
  # vars and use it for weight

  data <- create_weight(data,
                        weight)
  if(is.null(weight)){

    weight <- "dummy_wt"
  }

  # ensure that variables are changed to numeric class type

  data <- data %>%
    dplyr::mutate(across(all_of(variables), ~ as.numeric(as.character(.))))

  #change column names based on having weight or not

  if(weight!="dummy_wt"){
    result <- lapply(variables, function(i) {
      pollster::summary_table(
        df = data,
        variable = !!sym(i),
        weight = !!sym(weight),
        name_style = "clean"
      )


    })}else {

      result <- lapply(variables, function(i) {
        pollster::summary_table(
          df = data,
          variable = !!sym(i),
          weight = !!sym(weight),
          name_style = "clean"
        ) %>%dplyr::select(variable_name,
                           unweighted_observations,
                           mean=weighted_mean,
                           min_value,
                           max_value,
                           missing_observations)


      })

      print(names(result))
    }

  print_lines <- lapply(variables, function(i) {
    paste0("Summary table for ", i)
  })

  if (is.null(output)) {
    #return(as.data.frame(result))
    create_data_frame(result)

  } else if (output == "print") {
    print_helper(print_line = print_lines, result = result)

  } else{
    table_name <- get_table_name(table_name, default = "summary_table")

    #rename the dataframes in the list according to the variables.
    names(result) <- variables
    #write out the excel sheet
    writexl::write_xlsx(result, table_name)

  }

}



# weighted averages function  ---------------------------------------------

#' Creates weighted average
#'
#' @param data Data to create the weighted average from
#' @param x The column you want to get the average for. Must be in quotes.
#' @param weight  The name of the column that contains the weights. Must be in quotes.
#' @param type The type of average. One of "mean","median" and avg by grp".
#' Default is "mean".
#' @param by the name of the column you want to group the average by. Must be in quotes.
#' @param ... extra arguments to pass on to the functions used in this custom function.
#' Those functions are survey::svymean() and survey::svyquantile().
#' @returns A weighted average.
#' @examples
#'

weighted_avg <- function(data,
                         x,
                         weight,
                         type = "mean",
                         by = NULL,
                         ...
) {
  #check for weight
  if (rlang::is_missing(weight)) {
    stop("Please provide the column name for the weight you want to use for your average.")
  }


  #create a survey design object (weight)

  sdo <- survey::svydesign(ids = ~ 1,
                           weights =  as.formula(paste("~", weight)),
                           data = data)


  if (type == "mean"&&is.null(by)) {
    result <-  survey::svymean(x = as.formula(paste("~", x))
                               , design = sdo
                               #, na.rm=na.rm
                               , ...)

    return(result)
  } else if (type == "median"&&is.null(by)) {
    result <-  survey::svyquantile(x = as.formula(paste("~", x))
                                   , design = sdo
                                   # , na.rm=na.rm
                                   # ,quantiles = quantile
                                   # , ci=ci
                                   , ...)

  } else if (type == "avg by grp") {


    result <- svyby(
      formula =  as.formula(paste("~", x)),
      by = as.formula(paste("~", by)),
      design = sdo,
      FUN = svymean,
      row.names = FALSE,
      ...
    )
  }
  print(result)

}
