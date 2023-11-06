library(dplyr)
library(duckdb)
library(dplyr)
library(DBI)
library(DT)
library(ggplot2)
library(tidyr)
library(purrr)
library(forcats)


# Create the connection to a database and "studies" and "sponsors" tables.
project_dir <- "/Users/adan_rivas/Library/CloudStorage/OneDrive-YaleUniversity/
Classes/Fall 2023/BIS 620 Data Science Software Systems/midterm"
db_dir <- file.path(project_dir, "ctrialsgovdb")
con = dbConnect(
  duckdb(
    file.path(db_dir, "ctgov.duckdb"), 
    read_only = TRUE
  )
)

# check duck db connection
if (length(dbListTables(con)) == 0) {
  stop("Problem reading from connection.")
}

# query tables from db
STUDIES = tbl(con, "studies") 
SPONSORS = tbl(con, "sponsors")
CONDITIONS = tbl(con, "conditions")

# unique sponsor "types"
sponsor_types_u <- SPONSORS |> 
  dplyr::select(lead_or_collaborator) |> 
  dplyr::distinct() |>
  collect()

# get unique phase levels from studies table
study_phases_u <- STUDIES |> 
  dplyr::select(phase) |> 
  dplyr::distinct() |>
  collect() |>
  # recategoize NA as 'Missing
  dplyr::mutate(phase = case_when(
    is.na(phase) ~ 'Missing',
    .default = phase
  ))


################################
##### Function Definitions #####
################################

#' @title Query keywords from a database table.
#' @description Process user input keywords to filter database table.
#' @param df the database table.
#' @param kwds the keywords to look for.
#' @param column the column to look for the keywords in.
#' @param ignore_case should the case be ignored when searching for a keyword?
#' (default TRUE)
#' @param match_all should we look for values that match all of the keywords 
#' (intersection) or any of the keywords (union)? (default FALSE; union).
#' @return Data frame with filtered results based on keywords provided. 
query_kwds <- function(df, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  kwds = kwds[kwds != ""]
  kwds = paste0("%", kwds, "%") |>
    gsub("'", "''", x = _)
  if (ignore_case) {
    like <- " ilike "
  } else{
    like <- " like "
  }
  query = paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )
  filter(df, sql(query)) 
}


#' @title Histogram of queried clincial trial phases
#'
#' @description This function creates a histogram of study phases. It takes a data 
#' frame of studies and an optional
#'  set of phase labels as input to be used along the x-axis. It processes the data 
#'  frame to count the 
#'  number of studies in each phase, and then creates a histogram of these counts. 
#'  The x-axis of the 
#'  histogram represents the study phases, and the y-axis represents the counts. 
#'  The function returns
#'  a list containing the processed data frame and the histogram plot.
#'
#' @param studies_df A data frame of queried clinical studies.
#' @param phase_labels A character vector specifying the set of phase labels to 
#' show in the plot for standardization. Default is `study_phases_u`.
#'
#' @return A list containing the processed data frame and the histogram plot.
#' @export
plot_phase_histogram <- function(studies_df, phase_labels=study_phases_u, drop_not_applicable=TRUE) {
  studies_df$phase[is.na(studies_df$phase)] = "NA"
  
  if (nrow(studies_df) == 0) {
    return (list("study_phase_df" = data.frame(), "phase_plot" = ggplot()))
  }
  
  study_phase_df <- studies_df |>
    dplyr::select(phase) |>
    dplyr::mutate(phase = ifelse(is.na(phase), 'Missing', phase)) |>
    dplyr::group_by(phase) |>
    dplyr::summarize(n = n()) |>
    # Problem 1 
    # Fix the phase histogram so that the x-axis values are uniform regardless of the query.
    right_join(y=study_phases_u, by='phase')
  
  caption_txt = ''
  if (drop_not_applicable) {
    study_phase_df <- study_phase_df |> filter(phase != 'Not Applicable')
    caption_txt = "'Not Applicable' values were removed."
  }
  
  col_phase_plt <- ggplot(data=study_phase_df, aes(x = phase, y = n)) +
    geom_col() +
    theme_bw() +
    labs(x='CT Phase', y='Count', caption=caption_txt)
  
  return (list("study_phase_df" = study_phase_df, "phase_plot" = col_phase_plt))
}



#' @Title Get the number of concurrent studies for each date in a set of studies.
#'
#' @description This function takes a data frame of studies and returns a tibble with a 
#' `date` column and a `count` of the number of concurrent trials at that date.
#' @param studies_df A data frame. The studies to get the number of concurrent 
#' trials for.
#' @return A tibble with a `date` column and a `count` of the number of concurrent 
#' trials at that date.
#' @examples
#' \dontrun{
#' studies_df <- data.frame(
#'     start_date = as.Date(c('2020-01-01', '2020-02-01', '2020-03-01')), 
#'     completion_date = as.Date(c('2020-12-31', '2020-11-30', '2020-10-31')))
#' get_concurrent_studies(studies_df)
#' }
#' @export
get_concurrent_studies <- function(studies_df) {
  # Get all of the unique dates.
  all_dates = studies_df |> 
    tidyr::pivot_longer(cols = everything()) |>
    dplyr::select(-name) |>
    dplyr::distinct() |> 
    dplyr::arrange(value) |>
    na.omit() |> 
    dplyr::rename(date = value)
  
  within_date = function(date, starts, ends) {
    date >= starts & date <= ends
  }
  
  # Get the number of concurrent trials at each of the unique dates.
  all_dates$count = 
    purrr::map_dbl(
      all_dates$date, 
      ~ .x |> 
        within_date(studies_df$start_date, studies_df$completion_date) |>
        sum(na.rm = TRUE)
    )
  return(all_dates)
}


#' @title Line plot of concurrent studies over time.
#'
#' @description This function takes a data frame of studies and returns a list 
#' containing a data frame of the number of concurrent studies for each date and 
#' a ggplot object of the line plot of the number of concurrent studies over time.
#' @param studies_df A data frame. The studies to plot the number of concurrent trials for.
#' @return A list with two elements: 
#'   - "concurrent_df": A tibble with a `date` column and a `count` of the number 
#'   of concurrent trials at that date.
#'   - "concurrent_line_plot": A ggplot object of the line plot of the number of 
#'   concurrent studies over time.
#' @export
plot_concurrent_studies <- function(studies_df) {
  concurrent_studies_df <- studies_df |>
    dplyr::select(start_date, completion_date) |>
    get_concurrent_studies()
  
  concurrent_line_plt <- concurrent_studies_df |>
    ggplot(aes(x = date, y = count)) +
    geom_line() +
    xlab("Date") +
    ylab("Count") +
    theme_bw()
  
  return (list(
    "concurrent_df" = concurrent_studies_df, 
    "concurrent_line_plot" = concurrent_line_plt))
}


# Problem 2 helper function
#' Join studies data with study conditions data.
#'
#' @description This function joins the studies data frame with the study conditions 
#' data frame on 'nct_id'. The resulting data frame is returned.
#'
#' @param studies_df A data frame containing the studies data.
#'
#' @return A data frame containing the joined studies and study conditions data.
#' @export
get_study_conditions <- function(studies_df, conditions_df=CONDITIONS){

  if (nrow(studies_df) == 0) {
    return (data.frame())
  }

  study_conditions_df <- conditions_df |>
    dplyr::select(-c(id, downcase_name)) |>
    dplyr::inner_join(y=studies_df, by='nct_id', copy=TRUE) |>
    dplyr::rename(condition_name = name)
  
  return (study_conditions_df)
}


# ====== New Feature ===== #
# separate data pre-processing from plotting
#' @title Summarize Study Conditions
#'
#' @description This function processes a study conditions data frame and summarizes 
#' the frequency of each condition. It keeps only the top n conditions specified. 
#' If factor lumping is set to TRUE, it applies factor lumping. This is not fully
#' implemented for the user to enable from the app.
#'
#' @param study_conditions_df A data frame containing the study conditions data.
#' @param top_n An integer specifying the number of top conditions to keep. 
#' @param fct_lump A logical value indicating whether to apply factor lumping. 
#' Default is FALSE.
#'
#' @return A data frame containing the summarized study conditions data.
#' @export
summarize_study_conditions <- function(study_conditions_df, top_n, lump_fct=FALSE) {
  
  fct_lump_ngroups <- ifelse(top_n <= 12, 24 ,round(top_n*2.5))

  if (lump_fct) {
    grouped_study_conditions_df <- study_conditions_df |> 
      select(condition_name) |>
      collect() |>
      # only keep top 24 (or top_n*2.5) conditions, otherwise 'Other' group becomes too large
      mutate(condition_name = fct_lump_n(fct_infreq(condition_name), n=fct_lump_ngroups)) |>
      group_by(condition_name) 
  } else {
    grouped_study_conditions_df <- study_conditions_df |> 
      select(condition_name) |>
      group_by(condition_name)  
  }
  
  summ_study_conditions_df <- grouped_study_conditions_df |>
    summarize(n = n()) |>
    arrange(desc(n)) |>
    # keep top in case keyword query does not filter data much
    head(top_n) |>
    collect() |>
    mutate(condition_name = fct_rev(fct_reorder(condition_name, n))) 
  
  return (summ_study_conditions_df)
  
}


# Problem 2 plotting function
#' @title Histogram of the study conditions based on key-word query results
#'
#' @description This function takes a data frame of studies and an integer specifying 
#' the number of top conditions to keep, and returns a list containing a data frame 
#' of the summarized study conditions and a ggplot object of the histogram of the 
#' study conditions.
#' @param studies_df A data frame. The studies to plot the histogram for.
#' @param top_n An integer specifying the number of top conditions to keep. 
#' Default is 8.
#' @return A list with two elements: 
#'   - "summ_df": A data frame of the summarized study conditions.
#'   - "study_cond_plot": A ggplot object of the histogram of the study conditions.
#' @export
plot_study_conditions_histogram <- function(studies_df, top_n = 8) {
  study_conditions_df <- get_study_conditions(studies_df=studies_df)
  
  if (is_empty(study_conditions_df)) {
    return (list(
      "summ_df" = study_conditions_df, 
      "study_cond_plot" = ggplot()))
  }
  
  summ_study_conditions_df <- summarize_study_condtions(
    study_conditions_df = study_conditions_df, top_n = top_n)
  
  study_cond_col_plt <- ggplot(
      data=summ_study_conditions_df, aes(x = condition_name, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Study Condition") +
    ylab("Count")
  
  return (list(
    "summ_df" = summ_study_conditions_df, 
    "study_cond_plot" = study_cond_col_plt))
}

