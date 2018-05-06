'#  year.month
'#  @export
year.month <- function(date) {
  date %>% {paste0(year(.), '_', sprintf("%02d", month(.)))}
}

'#  year.week
'#  @export
year.week <- function(date) {
  date %>% {paste0(year(.), '_', sprintf("%02d", epiweek(.)))}
}

'#  time_diff
'#  @export
time_diff <- function(time2, time1, unit='mins') {
  difftime(time2, time1, units=unit) %>% as.numeric() %>% round(2)
}

'#  week_diff
'#  @export
week_diff <- function(week2, week1, numeric = F) {
  interval(as.Date(paste0(week1, '.1'), format='%Y_%U.%w'),
           as.Date(paste0(week2, '.1'), format='%Y_%U.%w')) %/%
    weeks(1) %>% as.numeric() %>%
    ifelse(numeric, ., sprintf("%02d", .))
}

'#  month_diff
'#  @export
month_diff <- function(month2, month1, numeric = F) {
  interval(as.Date(paste0(month1, '.1'), format='%Y_%m.%d'),
           as.Date(paste0(month2, '.1'), format='%Y_%m.%d')) %/%
    months(1) %>% as.numeric() %>%
    ifelse(numeric, ., sprintf("%02d", .))
}

'#  day_of_month
'#  @export
day_of_month <- function(date) {
  date %>% format('%d') %>% as.numeric()
}

'#  readable_week
'#  @export
readable_week <- function(year.week) {
  paste(as.Date(paste0(year.week,'.0'), format='%Y_%U.%w') %>% as_date() %>% format('%y.%m.%d'),
        as.Date(paste0(year.week,'.6'), format='%Y_%U.%w') %>% as_date()%>% format('%y.%m.%d'),
        sep = ' to ')
}

'#  readable_week_range
'#  @export
readable_week_range <- function(year.week.range) {
  paste(as.Date(paste0(year.week.range[1],'.0'), format='%Y_%U.%w') %>% as_date() %>% format('%Y.%m.%d'),
        as.Date(paste0(year.week.range[2],'.6'), format='%Y_%U.%w') %>% as_date()%>% format('%Y.%m.%d'),
        sep = ' to ')
}
