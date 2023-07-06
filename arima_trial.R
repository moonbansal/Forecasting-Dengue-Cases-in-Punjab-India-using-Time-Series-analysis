library('ggplot2')
library('forecast')
library('tseries')

fateh <- read.csv("Fatehgarh Sahib.csv", header = TRUE)
fateh$date_R <- as.Date(fateh$Date, format = "%d/%m/%y")
ggplot(fateh, aes(date_R, Cases)) + geom_line() + scale_x_date('month')  + ylab("Monthly cases") +
  xlab("")

cases_ts = ts(fateh[, c('Cases')])
fateh$clean_cases = tsclean(cases_ts)
ggplot() + 
  geom_line(data = fateh, aes(x = date_R, y = clean_cases)) + ylab('Cleaned Cases') 