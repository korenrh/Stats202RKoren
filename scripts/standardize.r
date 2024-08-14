install.packages("dplyr")
library(dplyr)

# scale all signals
standardize <- function(data, sigs) {
  data <- data %>%
    mutate(across(all_of(sigs), ~log1p(.) %>% as.vector()))
  data <- data %>%
    mutate(across(all_of(sigs), ~scale(.) %>% as.vector()))
  return(data)
}

# scale all signals and add as new features
standardize_add_features <- function(data) {
  data_log <- data %>%
    mutate(across(starts_with("sig"), ~ log1p(.), .names = "log_{col}"))
  data_scale <- data_log %>%
    mutate(across(starts_with("sig"), ~ scale(.), .names = "scaled_{col}"))
  return(data_scale)
}

# scale select signals
standardize_custom <- function(data, scale_sigs, log_sigs) {
  data <- data %>%
    mutate(across(all_of(log_sigs), ~log1p(.)))
  data <- data %>%
    mutate(across(all_of(scale_sigs), ~scale(.)))
  return(data)
}

# scale select signals and add as new features
standardize_custom_add <- function(data, scale_sigs, log_sigs) {
  data <- data %>%
    mutate(across(all_of(log_sigs), ~ log1p(.), .names = "log_{col}"))
  data <- data %>%
    mutate(across(all_of(scale_sigs), ~ scale(.), .names = "scaled_{col}"))
  return(data)
}