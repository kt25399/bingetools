
intox_dose_data <- function(file, sheet) {
    intox_dose <- readxl::read_excel(file, sheet = sheet) %>%
      tidyr::drop_na() %>%
      dplyr::filter(Group == "Ethanol")

    if(nrow(intox_dose)*ncol(intox_dose) != 0){
      intox_dose <- intox_dose %>%
        tidyr::pivot_longer(cols = starts_with("Day"),
                            names_to = "day") %>%
        dplyr::group_by(Subject) %>%
        dplyr:: mutate(day =
                         stringr::str_extract(day, "Day \\d"),
                         timepoint = c(1:12))}
}

wd_data <- function(file, sheet) {
  wd <- readxl::read_excel(file, sheet = sheet) %>%
    tidyr::drop_na()

  if(nrow(intox)*ncol(intox) != 0){
    wd <- wd %>%
      tidyr::pivot_longer(cols = starts_with("4"),
                   names_to = "timepoint") %>%
      dplyr::group_by(Subject) %>%
      dplyr::mutate(timepoint = c(1:17))}
}

bec_data <- function(file, sheet) {
  bec <- readxl::read_excel(file, sheet = sheet) %>%
    tidyr::drop_na()

  if(nrow(bec)*ncol(bec) != 0){
    bec <- bec %>%
      tidyr::pivot_longer(cols = starts_with("Trial"),
                   names_to = "trial") %>%
      dplyr::group_by(Subject, Group, Sex, Study) %>%
      dplyr::summarize(average = mean(value))}
}
