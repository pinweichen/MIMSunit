
# Helper Function script ===========
#' @param dob Date of birth (Date object)
#' @param dov Date of visit (Date object)
#' @return Age in months, capped at 18*12 = 216 months.
calculate_age_months <- function(dob, dov) {
  age_in_days <- as.numeric(dov - dob)
  age_in_months <- round(age_in_days / 30.4375, 2) # Average days per month
  # Cap age at 18 years (in months)
  capped_age <- pmin(age_in_months, 18 * 12)
  return(capped_age)
}

#' Calculate Z-score and Centile using LMS method
#'
#' @param value The measured BMD value.
#' @param l Lambda parameter from LMS reference.
#' @param m Mu parameter (median) from LMS reference.
#' @param s Sigma parameter (coefficient of variation) from LMS reference.
#' @return A list containing the calculated z-score and centile.
calculate_lms <- function(value, l, m, s, t = NA) {
  if (is.na(value) || value == 0 || is.na(l) || is.na(m) || is.na(s) || m <= 0) {
    # Handle cases where calculation is not possible or value is zero
    return(list(z = NA_real_, centile = NA_real_))
  }
  # LMS Calculation: z = (((value/M)^L) - 1) / (L*S)
  # Handle L=0 case (Box-Cox transformation limit)
  if (abs(l) < 1e-6) { # Check if L is effectively zero
    z <- log(value / m) / s
  } else {
    z <- (((value / m)^l) - 1) / (l * s)
  }
  centile <- round(pnorm(z) * 100, 2)
  if(!is.na(t)){
    require(gamlss.dist)
    centile <- round(pBCT(q = z, mu = m, sigma = s, nu = l, tau = t)*100,2)
  }
  
  z <- round(z, 3)
  return(list(z = z, centile = centile))
}

#' Core Calculation Logic for LDF Z-scores
#'
#' Processes inputs for all three regions and returns a combined data table.
#' @param input_sub_id Subject ID.
#' @param input_dob Date of birth (Date object).
#' @param input_dov Date of visit (Date object).
#' @param input_age Age (numeric).
#' @param input_sex Sex ('M'/'F').
#' @param input_mims MIMS value 
#' @param reference_data The rdp_lms data table.
#' @return A data.table 'finaldt' with results for all regions.
run_calculations <- function(input_sub_id, input_dob, input_dov, input_age = NA, input_sex,
                             input_mims, reference_data) {
  
  if(!is.na(input_age)){
    patient_age_years <-   input_age 
    patient_age_months <- patient_age_years*12
  } else {
    patient_age_months <- calculate_age_months(input_dob, input_dov)
    patient_age_years <- patient_age_months / 12 # For matching ref data age
  }
  # Find closest matching reference data row
  # Filter first by demographics and region
  ref_subset <- reference_data[sex == input_sex]
  
  # Find row with minimum absolute difference in age (years)
  closest_row_index <- which.min(abs(ref_subset$age - patient_age_years))
  
  
  tryCatch({
    lms_params <- ref_subset[closest_row_index, .(l, m, s,t, ref_age_years = age)]
  }, error = function(e) {
    warning("tau do not exist in the reference data. Used LMS model instead", 
            "\nError: ", conditionMessage(e))
    # Fallback to current working directory if creation fails
    lms_params <- ref_subset[closest_row_index, .(l, m, s, ref_age_years = age)]
  })
  
  # Calculate Z-score and Centile
  if(length(grep("t",colnames(lms_params))) > 0){
    lms_results <- calculate_lms(input_mims, lms_params$l, lms_params$m, lms_params$s ,lms_params$t)
  } else {
    stop("No Tau detected. Please check your reference file and/or age range.")
  }
  # else {
  #   lms_results <- calculate_lms(input_mims, lms_params$l, lms_params$m, lms_params$s)
  # }
  # 
  
  
  
  # Store results for this region
  finaldt <- data.table(
    Subject_ID = input_sub_id,
    sex = input_sex,
    value = round(input_mims, 3),
    age_at_visit_months = patient_age_months,
    ref_age_years = lms_params$ref_age_years, # Store the reference age used
    z = lms_results$z,
    centile = lms_results$centile
  )
  return(finaldt)
}
