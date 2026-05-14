
library(ggplot2)
library(data.table) 
library(tidyverse)
rm(list=ls())

setwd("/Users/chenp7/Library/CloudStorage/OneDrive-Children\'sHospitalofPhiladelphia/Github_Repo/MIMSunit/Benny_scripts/")
rdp_lms <- fread(paste0(getwd(), "/BELCHER_LMS_MIMS_Metrics.csv"))
output_folder <- file.path(getwd(), "Belcher_LMS_testing_output")
dir.create(output_folder)
colnames(rdp_lms) <- tolower(colnames(rdp_lms))

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
calculate_lms <- function(value, l, m, s, t) {
  if (is.na(value) || value == 0 || is.na(l) || is.na(m) || is.na(s) || m <= 0) {
    # Handle cases where calculation is not possible or value is zero
    return(list(z = NA_real_, centile = NA_real_))
  }

  if(!is.na(t)){
    require(gamlss.dist)
    z <- round(BCT(q = value, mu = m, sigma = s, nu = l, tau = exp(t)),2)
    centile <- round(pBCT(q = value, mu = m, sigma = s, nu = l, tau = exp(t)),2)*100

  }

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
run_calculations <- function(input_sub_id, input_dob, input_dov, input_age, input_sex,
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
    lms_results_with_tau <- calculate_lms(input_mims, lms_params$l, lms_params$m, lms_params$s ,lms_params$t)
  }
    lms_results <- calculate_lms(input_mims, lms_params$l, lms_params$m, lms_params$s)

  
  
  # Store results for this region
  finaldt <- data.table(
    Subject_ID = input_sub_id,
    sex = input_sex,
    value = round(input_mims, 3),
    age_at_visit_months = patient_age_months,
    ref_age_years = lms_params$ref_age_years, # Store the reference age used
    z = lms_results$z,
    centile = lms_results$centile,
    centile_with_t = lms_results_with_tau$centile
  )
  return(finaldt)
}

#' Core Calculation Logic for LDF Centile
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
run_calculations_centile <- function(input_sub_id, input_dob, input_dov, input_age = NA, input_sex,
                                     input_mims, reference_data) {
  colnames(reference_data) <- tolower(colnames(reference_data))
  if(!is.na(input_age)){
    patient_age_years <-   input_age 
    patient_age_months <- patient_age_years*12
  } else {
    patient_age_months <- calculate_age_months(input_dob, input_dov)
    patient_age_years <- patient_age_months / 12 # For matching ref data age
  }
  # Find closest matching reference data row
  # Filter first by demographics and region
  # browser()
  ref_subset <- reference_data[sex %in% input_sex,]
  
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
  
  return(lms_results)
}

# Main script ===========
# install_github("brightantwiboasiako/wwadapercentiles",force=T)

percentile_LMS <- 
  rdp_lms |> 
  dplyr::select(l,m,s,t,sex,age,contains("percentiles_")) |>
  pivot_longer(
    cols = starts_with("percentiles_"),
    names_to = "percentiles",
    names_prefix = "percentiles_",
    values_to = "value",
    values_drop_na = TRUE
  )
setDT(percentile_LMS)
percentile_LMS[, index_n := 1:.N]
percentile_LMS[, age := as.numeric(age)]
percentile_LMS[, percentiles := as.numeric(percentiles)]
percentile_LMS[, sex := as.factor(sex)]
percentile_LMS[, Calculated_z :=  run_calculations(input_sub_id = index_n,
                                       input_age = age, 
                                       input_sex = sex, 
                                       input_mims = value,
                                       reference_data = rdp_lms)$z, by = .I]
percentile_LMS[, Calculated_centile :=  run_calculations(input_sub_id = index_n,
                                        input_age = age, 
                                        input_sex = sex, 
                                        input_mims = value,
                                        reference_data = rdp_lms)$centile, by = .I]
# percentile_LMS[, Calculated_centile_with_tau := round(pBCPE(q = value,
#                                                        mu = m, sigma = s, nu = l, tau = t)*100,3)
#                                                          , by = .I]
percentile_LMS[, Calculated_centile_with_tau := round(gamlss.dist::pBCT(value,
                                                            mu = m, sigma = s, nu = l, tau = exp(t))*100,2)
               , by = .I]
percentile_LMS[, Calculated_centile_with_tau_BCPE := round(gamlss.dist::pBCPE(value,
                                                                        mu = m, sigma = s, nu = l, tau = t)*100,2)
               , by = .I]



percentile_LMS[, diff_centiles := Calculated_centile - as.numeric(percentiles)]
percentile_LMS[, diff_centiles_tau := Calculated_centile_with_tau - as.numeric(percentiles)]
percentile_LMS[, diff_centiles_tau_BCPE := Calculated_centile_with_tau_BCPE - as.numeric(percentiles)]

percentile_LMS$percentiles <- as.numeric(percentile_LMS$percentiles)

fwrite(percentile_LMS, file.path(output_folder,"LMS_calculation_results.csv"))




p <- ggplot(aes(x = age), data = rdp_lms) + facet_grid(.~sex) +
  geom_line(aes(y = percentiles_5, colour = 'percentiles_5'), linetype = 'dashed', linewidth = 1) +
  geom_line(aes(y = percentiles_25, colour = 'percentiles_25'), linetype = 'dashed', linewidth = 1) +
  geom_line(aes(y = percentiles_50, colour = 'percentiles_50'), linetype = 'dashed', linewidth = 1) +
  geom_line(aes(y = percentiles_75, colour = 'percentiles_75'), linetype = 'dashed', linewidth = 1) +
  geom_line(aes(y = percentiles_95, colour = 'percentiles_95'), linetype = 'dashed', linewidth = 1) +
  scale_color_manual(
    name = 'Legend',
    values = c('percentiles_95' = 'navyblue', 'percentiles_75' = 'skyblue', 'percentiles_50' = 'blue',
               'percentiles_25' = 'skyblue', 'percentiles_5' = 'navyblue'),
    breaks = c('percentiles_95', 'percentiles_75', 'percentiles_50', 'percentiles_25', 'percentiles_5', 'Patient')
  ) +
  theme_bw() +
  scale_x_continuous(name = "Age (Years)", breaks = seq(3, 80, 10), limits = c(min(rdp_lms$age, na.rm = TRUE),
                                                                              max(rdp_lms$age, na.rm = TRUE))) +
  scale_y_continuous(name = expression(paste("Daily MIMS unit"))) + 
  ggtitle(paste0("Daily MIMS unit Percentile from Reference")) +
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"),
    axis.text.x = element_text(face="bold", color="black",size=14),
    axis.text.y = element_text(face="bold", color="black",size=14),
    legend.position = c(.95, .95),
    legend.background = element_rect(fill=alpha(0.4)),
    legend.justification = c("right", "top"),
    legend.text = element_text(size=12, face="bold"))
ggsave(file.path(output_folder,"MIMS_daily_percentile_reference.svg"), p, width = 16, height = 8, units = "cm", device = "svg")


q <- ggplot(data = percentile_LMS, aes(x = age, y = value, colour = Calculated_centile)) + 
  facet_grid(.~sex) +
  geom_point() +
  scale_color_gradient() + # Continuous color scale 
  scale_x_continuous(name = "Age (Years)", breaks = seq(3, 80, 10), limits = c(min(rdp_lms$age, na.rm = TRUE),
                                                                               max(rdp_lms$age, na.rm = TRUE))) +
  theme_bw() + scale_y_continuous(name = expression(paste("Daily MIMS unit"))) + 
  ggtitle(paste0("Daily MIMS unit Percentile from LMS only calculation")) +
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"),
    axis.text.x = element_text(face="bold", color="black",size=14),
    axis.text.y = element_text(face="bold", color="black",size=14),
    legend.position = c(.95, .95),
    legend.background = element_rect(fill=alpha(0.4)),
    legend.justification = c("right", "top"),
    legend.text = element_text(size=12, face="bold"))
ggsave(file.path(output_folder,"MIMS_daily_percentile_LMSonly_calculation.svg"), q, width = 16, height = 8, units = "cm", device = "svg")

r <- ggplot(data = percentile_LMS, aes(x = age, y = value, colour = Calculated_centile_with_tau)) + 
  facet_grid(.~sex) +
  geom_point() +
  scale_color_gradient() + # Continuous color scale 
  scale_x_continuous(name = "Age (Years)", breaks = seq(3, 80, 10), limits = c(min(rdp_lms$age, na.rm = TRUE),
                                                                               max(rdp_lms$age, na.rm = TRUE))) +
  theme_bw() + scale_y_continuous(name = expression(paste("Daily MIMS unit"))) + 
  ggtitle(paste0("Daily MIMS unit Percentile from LMS only calculation")) +
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"),
    axis.text.x = element_text(face="bold", color="black",size=14),
    axis.text.y = element_text(face="bold", color="black",size=14),
    legend.position = c(.95, .95),
    legend.background = element_rect(fill=alpha(0.4)),
    legend.justification = c("right", "top"),
    legend.text = element_text(size=12, face="bold"))
ggsave(file.path(output_folder,"MIMS_daily_percentile_LMSonly_calculation.svg"), r, width = 16, height = 8, units = "cm", device = "svg")



# ggplot(data = percentile_LMS, aes(x = age, y = Calculated_centile, colour = value )) + 
#   facet_grid(.~sex) +
#   geom_point() +
#   geom_point(aes(y = as.numeric(percentiles)), color = "red") +
#   scale_x_continuous(name = "Age (Years)", breaks = seq(3, 80, 10), limits = c(min(rdp_lms$age, na.rm = TRUE),
#                                                                              max(rdp_lms$age, na.rm = TRUE))) +
#   scale_y_continuous(name = expression(paste("Calculated Percentiles")))



ggplot(data = percentile_LMS, aes(x = age, y = diff_centiles, colour = as.factor(percentiles) )) + 
  facet_grid(.~sex) +
  geom_point() + scale_color_discrete() + 
  scale_x_continuous(name = "Age (Years)", breaks = seq(3, 80, 10), limits = c(min(rdp_lms$age, na.rm = TRUE),
                                                                               max(rdp_lms$age, na.rm = TRUE))) +
  scale_y_continuous(name = expression(paste("Calculated - Reference Percentiles"))) + 
  guides(colour=guide_legend(title="Reference")) +
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"),
    axis.text.x = element_text(face="bold", color="black",size=14),
    axis.text.y = element_text(face="bold", color="black",size=14),
    legend.background = element_rect(fill=alpha(0.4)),
    legend.justification = c("right", "top"),
    legend.text = element_text(size=12, face="bold"))
ggsave(file.path(output_folder,"percentile_differences_by_age.svg"), last_plot(), width = 16, height = 8, units = "cm", device = "svg")

ggplot(data = percentile_LMS, aes(x = age, y = diff_centiles_tau, colour = as.factor(percentiles) )) + 
  facet_grid(.~sex) +
  geom_point() + scale_color_discrete() + 
  scale_x_continuous(name = "Age (Years)", breaks = seq(3, 80, 10), limits = c(min(rdp_lms$age, na.rm = TRUE),
                                                                               max(rdp_lms$age, na.rm = TRUE))) +
  scale_y_continuous(name = expression(paste("Calculated - Reference Percentiles"))) + 
  guides(colour=guide_legend(title="Reference")) +
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"),
    axis.text.x = element_text(face="bold", color="black",size=14),
    axis.text.y = element_text(face="bold", color="black",size=14),
    legend.background = element_rect(fill=alpha(0.4)),
    legend.justification = c("right", "top"),
    legend.text = element_text(size=12, face="bold"))
ggsave(file.path(output_folder,"percentile_differences_pBCT_by_age.svg"), last_plot(), width = 16, height = 8, units = "cm", device = "svg")

ggplot(data = percentile_LMS, aes(x = age, y = diff_centiles_tau_BCPE, colour = as.factor(percentiles) )) + 
  facet_grid(.~sex) +
  geom_point() + scale_color_discrete() + 
  scale_x_continuous(name = "Age (Years)", breaks = seq(3, 80, 10), limits = c(min(rdp_lms$age, na.rm = TRUE),
                                                                               max(rdp_lms$age, na.rm = TRUE))) +
  scale_y_continuous(name = expression(paste("Calculated - Reference Percentiles"))) + 
  guides(colour=guide_legend(title="Reference")) +
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"),
    axis.text.x = element_text(face="bold", color="black",size=14),
    axis.text.y = element_text(face="bold", color="black",size=14),
    legend.background = element_rect(fill=alpha(0.4)),
    legend.justification = c("right", "top"),
    legend.text = element_text(size=12, face="bold"))
ggsave(file.path(output_folder,"percentile_differences_pBCPE_by_age.svg"), last_plot(), width = 16, height = 8, units = "cm", device = "svg")


# Children only
percentile_LMS_kid <- percentile_LMS[age <= 20]

ggplot(data = percentile_LMS_kid, aes(x = age, y = diff_centiles, colour = as.factor(percentiles))) + 
  facet_grid(.~sex) + geom_line(group = 1) +
  geom_point() + scale_color_discrete() + 
  scale_x_continuous(name = "Age (Years)", breaks = seq(3, 20, 2), limits = c(3,20)) +
  scale_y_continuous(name = expression(paste("Calculated - Reference Percentiles"))) + 
  guides(colour=guide_legend(title="Reference")) +
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"),
    axis.text.x = element_text(face="bold", color="black",size=14),
    axis.text.y = element_text(face="bold", color="black",size=14),
    legend.background = element_rect(fill=alpha(0.4)),
    legend.justification = c("right", "top"),
    legend.text = element_text(size=12, face="bold"))
ggsave(file.path(output_folder,"percentile_differences_by_age_youth.svg"), last_plot(), width = 16, height = 8, units = "cm", device = "svg")

ggplot(data = percentile_LMS_kid, aes(x = age, y = diff_centiles_tau, colour = as.factor(percentiles) )) + 
  facet_grid(.~sex) +
  geom_point() + scale_color_discrete() + 
  scale_x_continuous(name = "Age (Years)", breaks = seq(3, 20, 2), limits = c(3,20)) +
  scale_y_continuous(name = expression(paste("Calculated - Reference Percentiles"))) + 
  guides(colour=guide_legend(title="Reference")) +
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"),
    axis.text.x = element_text(face="bold", color="black",size=14),
    axis.text.y = element_text(face="bold", color="black",size=14),
    legend.background = element_rect(fill=alpha(0.4)),
    legend.justification = c("right", "top"),
    legend.text = element_text(size=12, face="bold"))
ggsave(file.path(output_folder,"percentile_differences_pBCT_by_age.svg"), last_plot(), width = 16, height = 8, units = "cm", device = "svg")



ggplot(data = percentile_LMS, aes(x = value, y = Calculated_centile, colour = as.factor(percentiles) )) +
  facet_grid(.~sex) +
  geom_point() + scale_color_discrete() +
  scale_x_continuous(name = "MIMS unit") +
  scale_y_continuous(name = expression(paste("Calculated Percentiles")), 
                     breaks = seq(0, 100, 5)) +
  guides(colour=guide_legend(title="Reference")) +
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"),
    axis.text.x = element_text(face="bold", color="black",size=14),
    axis.text.y = element_text(face="bold", color="black",size=14),
    legend.background = element_rect(fill=alpha(0.4)),
    legend.justification = c("right", "top"),
    legend.text = element_text(size=12, face="bold"))
ggsave(file.path(output_folder,"calculated_percentile_by_mims.svg"), last_plot(), width = 16, height = 8, units = "cm", device = "svg")

ggplot(data = percentile_LMS, aes(x = value, y = Calculated_centile_with_tau, colour = as.factor(percentiles) )) +
  facet_grid(.~sex) +
  geom_point() + scale_color_discrete() +
  scale_x_continuous(name = "MIMS unit") +
  scale_y_continuous(name = expression(paste("Calculated Percentiles")), 
                     breaks = seq(0, 100, 5)) +
  guides(colour=guide_legend(title="Reference")) +
  theme(
    plot.title = element_text(color="blue", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"),
    axis.text.x = element_text(face="bold", color="black",size=14),
    axis.text.y = element_text(face="bold", color="black",size=14),
    legend.background = element_rect(fill=alpha(0.4)),
    legend.justification = c("right", "top"),
    legend.text = element_text(size=12, face="bold"))
ggsave(file.path(output_folder,"calculated_percentile_with_tau_by_mims.svg"), last_plot(), width = 16, height = 8, units = "cm", device = "svg")




diff_summary <- percentile_LMS[,.(avg_centile = mean(diff_centiles),
                                  sd_centile = sd(diff_centiles)), by = c("percentiles","sex")]
diff_summary_with_tau <- percentile_LMS[,.(avg_centile_with_tau = mean(diff_centiles_tau),
                                  sd_centile_with_tau = sd(diff_centiles_tau)), by = c("percentiles","sex")]

install.packages("gamlss.dist")


