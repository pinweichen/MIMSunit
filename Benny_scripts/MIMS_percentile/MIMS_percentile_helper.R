library(data.table)
library(gamlss.dist)
library(tidyverse)
rm(list=ls())

# 1. Detect the directory where THIS script is saved
# This works whether the script is sourced or run line-by-line in RStudio
current_script_path <- (function() {
  attr(body(sys.function()), "srcfile")$filename
})()

# Fallback for RStudio users running line-by-line
if (is.null(current_script_path)) {
  current_script_path <- rstudioapi::getActiveDocumentContext()$path
}

# 2. Extract the folder path
script_dir <- dirname(current_script_path)

# 3. Create the full path to your CSV
csv_path <- file.path(script_dir, "BELCHER_LMS_MIMS_Metrics.csv")

# 4. Load the data safely
if (file.exists(csv_path)) {
  rdp_lms <- data.table::fread(csv_path)
} else {
  stop("Reference CSV not found at: ", csv_path)
}

#' Calculate MIMS Percentile and Z-Score using Box-Cox Tan (BCT)
#'
#' @param mims_value The daily MIMS unit score
#' @param age_years Patient age in years (numeric)
#' @param sex Patient sex ('M' or 'F')
#' @param reference_data The loaded rdp_lms data table
#' @return A data.table with the original value, parameters used, z-score, and centile.
get_mims_percentile <- function(mims_value, age_years, sex, reference_data) {
  
  # 1. Clean Reference Data Colnames (just in case)
  colnames(reference_data) <- tolower(colnames(reference_data))
  
  # 2. Filter by Sex
  ref_subset <- reference_data[sex == sex]
  
  if (nrow(ref_subset) == 0) {
    stop("No reference data found for the specified sex.")
  }
  
  # 3. Find Closest Age Match
  # We find the index of the reference row where age is closest to input age
  closest_idx <- which.min(abs(ref_subset$age - age_years))
  params <- ref_subset[closest_idx, ]
  
  # 4. Extract Parameters
  # Note: Based on your script, tau is stored as 't' and needs exp(t)
  L <- params$l
  M <- params$m
  S <- params$s
  T_val <- if("t" %in% colnames(params)) exp(params$t) else stop("Tau (t) column missing!")
  
  # 5. Calculate using gamlss.dist
  # pBCT gives the cumulative probability (centile)
  # qBCT/z-score equivalent is usually done via the probability 
  # but gamlss.dist provides the direct d, p, q, r functions.
  
  prob <- gamlss.dist::pBCT(q = mims_value, mu = M, sigma = S, nu = L, tau = T_val)
  
  # Calculate Z-score based on the probability
  z_score <- qnorm(prob)
  
  # 6. Return Clean Results
  return(data.table(
    input_mims = mims_value,
    matched_age = params$age,
    l_param = L,
    m_param = M,
    s_param = S,
    tau_param = T_val,
    z_score = round(z_score, 3),
    percentile = round(prob * 100, 2)
  ))
}

# # --- Example Usage for a New User ---
# 
# # 1. Load your reference data
#  rdp_lms <- fread("BELCHER_LMS_MIMS_Metrics.csv")
# 
# # 2. Apply the function for a single patient
# result <- get_mims_percentile(mims_value = 1250.5, age_years = 12.5, sex = "M", reference_data = rdp_lms)
# print(result)