#' year.month
#' @param date Date
#' @export
year.month <- function(date) {
  date %>% {paste0(lubridate::year(.), '_', sprintf("%02d", lubridate::month(.)))}
}

#' year.week
#' @param date Date
#' @export
year.week <- function(date) {
  date %>% {paste0(lubridate::year(.), '_', sprintf("%02d", lubridate::epiweek(.)))}
}

#' time_diff
#' @param time2 Date
#' @param time1 Date
#' @param unit character
#' @export
time_diff <- function(time2, time1, unit='mins', round_n=2) {
  difftime(time2, time1, units=unit) %>% as.numeric() %>% round(round_n)
}

#' week_diff
#' @param week2 character
#' @param week1 character
#' @param numeric logical
#' @export
week_diff <- function(week2, week1, numeric = F) {
  lubridate::interval(as.Date(paste0(week1, '.1'), format='%Y_%U.%w'),
                      as.Date(paste0(week2, '.1'), format='%Y_%U.%w')) %/%
    lubridate::weeks(1) %>% as.numeric() %>%
    ifelse(numeric, ., sprintf("%02d", .))
}

#' month_diff
#' @param month2 character
#' @param month1 character
#' @param numeric logical
#' @export
month_diff <- function(month2, month1, numeric = F) {
  interval(as.Date(paste0(month1, '.1'), format='%Y_%m.%d'),
           as.Date(paste0(month2, '.1'), format='%Y_%m.%d')) %/%
    lubridate::months(1) %>% as.numeric() %>%
    ifelse(numeric, ., sprintf("%02d", .))
}

#' day_of_month
#' @param date Date
#' @export
day_of_month <- function(date) {
  date %>% format('%d') %>% as.numeric()
}

#' day_of_week
#' @param date Date
#' @export
day_of_week <- function(date) {
  date %>% format('%w') %>% as.numeric()
}

#' readable_week
#' @param year.week character
#' @export
readable_week <- function(year.week) {
  paste(as.Date(paste0(year.week,'.0'), format='%Y_%U.%w') %>% as_date() %>% format('%y.%m.%d'),
        as.Date(paste0(year.week,'.6'), format='%Y_%U.%w') %>% as_date() %>% format('%y.%m.%d'),
        sep = ' to ')
}

#' readable_week_range
#' @param year.week character
#' @export
readable_week_range <- function(year.week.range) {
  paste(as.Date(paste0(year.week.range[1],'.0'), format='%Y_%U.%w') %>% as_date() %>% format('%Y.%m.%d'),
        as.Date(paste0(year.week.range[2],'.6'), format='%Y_%U.%w') %>% as_date()%>% format('%Y.%m.%d'),
        sep = ' to ')
}

#' first_day_this_week
#' @param date Date
#' @export
first_day_this_week <- function(date = today()) {
  date - lubridate::days(day_of_week(date))
}

#' first_day_this_month
#' @param date Date
#' @export
first_day_this_month <- function(date = today()) {
  date - lubridate::days(day_of_month(date) - 1)
}


#' Last Week
#' @param date Date
#' @export
last_week <- function(date = today()) {
  year.week(first_day_this_week(date) - 1)
}


#' Last Month
#' @param date Date
#' @export
last_month <- function(date = today()) {
  year.month(first_day_this_month(date) - 1)
}
