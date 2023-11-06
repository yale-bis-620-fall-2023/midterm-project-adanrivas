library(dplyr)
library(tidyr)
library(vdiffr)
source("ct-util.R")


# show schema tables
dbGetQuery(con, "SHOW TABLES;")

# get unique phase levels from studies table
study_phases_u <- studies |> select(phase) |> distinct()


plot_phase_histogram(studies)

# cumulative studies
d = studies |> 
  query_kwds("pembrolizumab", "brief_title") |>
  select(start_date, completion_date) |>
  collect() |> 
  get_concurrent_trials() |>
  ggplot(aes(x = date, y = count)) +
    geom_line() +
    xlab("Date") +
    ylab("Count")


############################
#### TestThat Functions ####
############################

test_that("get_concurrent_studies", {
  # Define a simple data frame of studies
  studies_df <- data.frame(
    start_date = as.Date(c('2020-01-01', '2020-02-01', '2020-03-01')), 
    completion_date = as.Date(c('2020-12-31', '2020-11-30', '2020-10-31')))
  
  # Call the function with the test data frame
  result <- get_concurrent_studies(studies_df)
  
  # Check that the result has the correct columns
  expect_identical(names(result), c("date", "count"))
  
  # Check that the result has the correct number of rows
  expect_equal(nrow(result), length(unique(c(studies_df$start_date, studies_df$completion_date))))
  
  # Check that the count column contains only non-negative numbers
  expect_true(all(result$count >= 0))
})


test_that("plot_concurrent_studies", {
  # Define a simple data frame of studies
  studies_df <- data.frame(
    start_date = as.Date(c('2020-01-01', '2020-02-01', '2020-03-01')), 
    completion_date = as.Date(c('2020-12-31', '2020-11-30', '2020-10-31')))
  
  # Call the function with the test data frame
  result <- plot_concurrent_studies(studies_df)
  
  # Check that the "concurrent_df" tibble has the correct columns
  expect_identical(names(result$concurrent_df), c("date", "count"))
  
  # Check that the "concurrent_line_plot" element is a ggplot object
  expect_true(is.ggplot(result$concurrent_line_plot))
  
  vdiffr::expect_doppelganger(
    title="plot-concurrent-studies-1",
    result$concurrent_line_plot
  )

})


test_that("get_study_conditions correctly joins studies and conditions data", {
  # Define a simple data frame of studies and conditions
  studies_df <- data.frame(nct_id = c("NCT001", "NCT002"),
                           study_name = c("Study 1", "Study 2"))
 
  conditions <- data.frame(nct_id = c("NCT001", "NCT002"),
                           id = c(1, 2),
                           downcase_name = c("study 1", "study 2"),
                           name = c("Condition 1", "Condition 2"))
  
  result <- get_study_conditions(studies_df=studies_df, conditions_df=conditions)
  
  expected <- data.frame(
    nct_id = c("NCT001", "NCT002"),
    condition_name = c("Condition 1", "Condition 2"),
    study_name = c("Study 1", "Study 2"))
  
  expect_identical(result, expected)
})


test_that("summarize_study_conditions", {
  # Define a simple data frame of studies and conditions
  studies_df <- data.frame(
    nct_id = c("NCT001", "NCT002", "NCT003", "NCT004", "NCT005"),
    study_name = c("Study 1", "Study 2", "Study 3", "Study 4", "Study 5"))
  
  conditions <- data.frame(
    nct_id = c("NCT001", "NCT002", "NCT003", "NCT004", "NCT005"),
    id = c(1, 2, 3, 4, 5),
    downcase_name = c("study 1", "study 2", "study 3", "study 4", "study 5"),
    name = c("Condition 1", "Condition 2", "Condition 1", "Condition 3", "Condition 2"))
  
  study_conditions_df <- get_study_conditions(studies_df=studies_df, conditions_df=conditions)
  
  top_n <- 2
  result <- summarize_study_conditions(study_conditions_df, top_n = top_n, lump_fct = FALSE)
  
  # Check that the result has the correct number of rows
  expect_equal(nrow(result), top_n)
  
  # Check that the "n" column contains only non-negatives
  expect_true(all(result$n >= 0))
  
  # Check that result output matches expected
  expected <- data.frame(
    condition_name = factor(
      c("Condition 1", "Condition 2"), 
      levels=c("Condition 2", "Condition 1")),
    n = as.integer(c(2, 2)))
  
  expect_identical(result, tibble(expected))
  
})


test_that("plot_study_conditions_histogram", {
  # Define a simple data frame of studies
  studies_df <- data.frame(nct_id = c("NCT01527552", "NCT01532375", "NCT01533649"),
                           study_name = c("Study 1", "Study 2", "Study 3"))
  
  result <- plot_study_conditions_histogram(studies_df=studies_df, top_n = 3)
  
  # Check that the list has the correct names
  expect_identical(names(result), c("summ_df", "study_cond_plot"))
  
  # Check that the "summ_df" data frame has the correct columns
  expect_identical(names(result$summ_df), c("condition_name", "n"))
  
  # Check that the "study_cond_plot" element is a ggplot object
  expect_true(is.ggplot(result$study_cond_plot))
  
  vdiffr::expect_doppelganger(
    title="plot-study-conditions-1",
    result$study_cond_plot
  )

})


