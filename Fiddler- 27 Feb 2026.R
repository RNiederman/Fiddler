# https://thefiddler.substack.com/p/can-every-day-be-first

library(lubridate)
library(magrittr)

YEARS <- 1:9999
wday_names <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")

register <- cbind(
  YEARS,
  matrix(NA, nrow = length(YEARS), ncol = 7)
)
colnames(register) <- c("Year", wday_names)

for (y in YEARS) {

  start_date <- paste(y, 1, 1, sep = "/") %>% as.Date
  end_date <- paste(y, 12, 1, sep = "/") %>% as.Date
  first_days <- seq(start_date, end_date, by = "month")

  weekdays <- wday(first_days)
  
  tab <- table(factor(weekdays, levels = 1:7))
  rw <- c(y, tab)
  register[y,] <- rw
  
  special <- all(tab > 0)
  print(y)
  
  if (!special) {break}
}
