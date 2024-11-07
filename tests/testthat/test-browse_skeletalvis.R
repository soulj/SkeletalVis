# Load the required libraries
library(testthat)
library(shiny)
library(DT)

# Define paths for test files (you might need to modify these paths to point to your actual test data)
exptable_path <- test_path("data", "expTable.txt")
comparisons_path <- test_path("data", "comparisons.txt")
