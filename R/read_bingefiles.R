intox_wd_params <- function(binge_files) {
  make_files()

  subjects <- map(sheets, get_subjects) %>%
    dplyr::bind_rows()
  saveRDS(subjects, 'data/binge/subject_log.rds')

  weights <- map(sheets, weight_data) %>%
    dplyr::bind_rows() %>%
    dplyr::left_join(., subjects)
  saveRDS(weights, 'data/binge/long/weights.rds')

  intox <- map(sheets, intox_dose_data) %>%
    dplyr::bind_rows() %>%
    dplyr::left_join(., subjects)
  saveRDS(intox, 'data/binge/long/intoxication.rds')

  dose <- map(sheets, intox_dose_data, 'Dose') %>%
    dplyr::bind_rows() %>%
    dplyr::left_join(., subjects)
  saveRDS(dose, 'data/binge/long/dose.rds')

  wd <- map(sheets, wd_data) %>%
    dplyr::bind_rows() %>%
    dplyr::left_join(., subjects)
  saveRDS(wd, 'data/binge/long/withdrawal.rds')

  bec <- map(sheets, bec_data) %>%
    dplyr::bind_rows() %>%
    dplyr::left_join(., subjects)
  saveRDS(bec, 'data/binge/long/bec.rds')

  return(list('weights' = weights,
              'intox' = intox,
              'dose' = dose,
              'wd' = wd,
              'bec' = bec))
}

make_files <- function() {
  if(!dir.exists('data')){
    dir.create('data')
  }
  if(!dir.exists(file.path('data','binge'))) {
    dir.create(file.path('data','binge'))
  }
  if(!dir.exists(file.path('data','binge', 'raw'))) {
    dir.create(file.path('data','binge','raw'))
  }
  if(!dir.exists(file.path('data','binge', 'long'))) {
    dir.create(file.path('data','binge','long'))
  }
}

get_subjects <- function(file, sheet = 'Binge Weight') {
  subjects <- readxl::read_excel(file, sheet = sheet) %>%
    dplyr::select('Subject','Group','Sex','Study','Timepoint') %>%
    dplyr::mutate(subject = dplyr::case_when(Study == 'SRTX 1' ~ Subject + 100,
                                             Study == 'SRTX 2' ~ Subject + 200,
                                             Study == 'SRTX 3' ~ Subject + 300),
                  group = dplyr::if_else(Group == 'Ethanol', 'Ethanol', 'Control')
    )
}

weight_data <- function(file, sheet = 'Binge Weight') {
  weight <- readxl::read_excel(file, sheet = sheet) %>%
    tidyr::drop_na() %>%
    pivot_longer(cols = c(6:9))
}

intox_dose_data <- function(file, sheet = 'Behavior') {
    intox_dose <- readxl::read_excel(file, sheet = sheet) %>%
      tidyr::drop_na() %>%
      dplyr::filter(Group == "Ethanol")

    if(nrow(intox_dose)*ncol(intox_dose) != 0){
      intox_dose <- intox_dose %>%
        tidyr::pivot_longer(cols = starts_with("Day"),
                            names_to = 'day') %>%
        dplyr::group_by(Subject) %>%
        dplyr:: mutate(day =
                         stringr::str_extract(day, "Day \\d"),
                         timepoint = c(1:12))}
}

wd_data <- function(file, sheet = 'Withdrawal') {
  wd <- readxl::read_excel(file, sheet = sheet) %>%
    tidyr::drop_na()

  if(nrow(intox)*ncol(intox) != 0){
    wd <- wd %>%
      tidyr::pivot_longer(cols = starts_with("4")) %>%
      dplyr::group_by(Subject) %>%
      dplyr::mutate(name = c(1:17))}
}

bec_data <- function(file, sheet = 'BEC') {
  bec <- readxl::read_excel(file, sheet = sheet) %>%
    tidyr::drop_na()

  if(nrow(bec)*ncol(bec) != 0){
    bec <- bec %>%
      tidyr::pivot_longer(cols = starts_with("Trial"),
                   names_to = "trial") %>%
      dplyr::group_by(Subject, Group, Sex, Study) %>%
      dplyr::summarize(mean_bec = mean(value))}
}
