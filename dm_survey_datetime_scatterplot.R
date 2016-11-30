# This script generates a huge scatterplot based on the dates and times of events.
# The test case is tweets plotted by date and time. It needs no other data other than
# date and times in a long list, like emails or log entries or whatever.

library(ggplot2)
library(scales)
d <- read.csv("~/Documents/dm_survey_data/dm_survey.csv")
d <- data.frame(d["Timestamp"])
d$date <- as.Date(d$Timestamp, "%Y/%m/%d")
d$timestring <- strftime(as.POSIXct(d$Timestamp, format="%Y/%m/%d %l:%M:%S %p"), format="%H:%M:%S")

d$time <- as.POSIXct(d$timestring, format="%H:%M:%S")
myplot <- ggplot(d, aes(x = date, y=time)) + 
  #geom_point(color="#000000", alpha=.2, size=5, shape=45, width=.4)  + 
  geom_jitter(color="#000000", alpha=.5, size=.2, shape=16, width=.3)  + 
  theme(axis.line=element_blank(),
        #axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_line(colour = "#eeeeee"),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = .5), 
        plot.title = element_text(hjust = 0.5),
        strip.background =element_blank()) + 
  scale_y_datetime(labels=date_format("%I:%M %p EST", tz = "EST"), date_breaks = "2 hours")  + 
  scale_x_date(date_breaks = "1 day", labels=date_format("%d %b")) + 
  ggtitle("Survey Responses by Date and Time")
ggsave("~/Documents/dm_survey_data/2016_survey_datetime_scatterplot.png", 
       plot = myplot, 
       width = 8, height = 4, 
       dpi = 300)