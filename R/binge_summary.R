mean_intox_params <- function(weights, intox, dose, wd, bec) {
  intox <- mean_intox(intox, Subject, Group, Sex, Study, Timepoint)
  dose <- dose_day(dose, Subject, Group, Sex, Study, Timepoint, day) %>%
    mean_dose(., Subject, Group, Sex, Study, Timepoint)
  wd <- withdrawal_severity(wd, Subject, Group, Sex, Study, Timepoint)

  mean_intox_params <- dplyr::left_join(intox, dose) %>%
    dplyr::left_join(., wd) %>%
    dplyr::left_join(., bec)

  saveRDS(mean_intox_params, 'data/binge/mean_intox_params.rds')

  weight <- weight_loss(weights)
  saveRDS(weight, 'data/binge/weight_loss.rds')

  return(c('mean_intox_params' = mean_intox_params,
           'weight_loss' = weight))
}

weight_loss <- function(df) {
  df %>%
    tidyr::pivot_wider() %>%
    dplyr::mutate(begin = `Day 1`) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with('Day ')) %>%
    dplyr::mutate(pecent = (((begin - value) / begin)) * 100)
}

mean_intox <- function(df, ...) {
  df %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(mean_intox = mean(value))
}

dose_day <- function(df, ...) {
  df %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(dose_day = sum(value))
}

mean_dose <- function(df, ...) {
  df %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(mean_dose = mean(dose_day))
}

withdrawal_severity <- function(df, ...) {
  df %>%
    dplyr:: group_by(...) %>%
    dplyr::summarise(mean_wd = mean(value),
              max_wd = max(value))
}
