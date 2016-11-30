library(reshape2)
library(ggplot2)
library(scales)
library(corrplot)
d <- read.csv("~/Documents/dm_survey_data/dm_survey_clean.csv", check.names=FALSE)
d[] <- lapply(d, as.character) # Convert factors to strings.

keeps <- c("Frequency of Games","Length of Games",
           "Primary Locations","Campaign Worlds","Adventures",
           "Preferred Combat Type","Preparation Time")

d <- d[keeps]

columns_to_compare <- keeps

keeps_combos <- data.frame(combn(1:length(keeps),2))

for (combo_num in 1:length(keeps_combos)) {
  i <- keeps_combos[1,combo_num]
  x <- keeps_combos[2,combo_num]
  
  t <- as.data.frame(table(c(d[columns_to_compare[i]],d[columns_to_compare[x]])))
  
  var1 <- gsub(" ", ".", columns_to_compare[i])
  var2 <- gsub(" ", ".", columns_to_compare[x])
  m <- dcast(t, get(var1) ~ get(var2), value.var="Freq")
  rownames(m) <- m[,1]
  m <- m[ -c(1) ]
  print(columns_to_compare[i])
  print(columns_to_compare[x])
  ch <- chisq.test(m)
  
  print(chisq.test(m))
  output_filename = paste("~/Documents/dm_survey_data/corrplots/",columns_to_compare[i], "-", columns_to_compare[x], "_corrplot.png", sep = "")
  png(filename = output_filename, height=1200, width=1200, pointsize = 24)
  corrplot(ch$residuals, is.cor = FALSE,tl.col = "black",
           title=paste("Correlations of", columns_to_compare[i],"and",columns_to_compare[x]),
           mar=c(0,0,1.5,0), cl.pos="n")
  dev.off()
}