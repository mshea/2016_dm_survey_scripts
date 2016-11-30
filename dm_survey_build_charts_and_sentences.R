library(ggplot2)
library(scales)
d <- read.csv("~/Documents/dm_survey_data/dm_survey_clean.csv", check.names=FALSE)
d[] <- lapply(d, as.character) # Convert factors to strings.

build_barplot <- function(factor_labels, column_name){
  d[,column_name] <- factor(d[,column_name], levels = factor_labels)
  c <- ggplot(d, aes(factor(d[,column_name])))
  c + 
    geom_bar() + 
    coord_flip() + 
    scale_y_continuous(expand=c(.15, 0)) +
    theme_minimal() +
    labs(x = "",
         title = column_name, 
         y=paste("Number of Respondants out of",nrow(d))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..))), 
              stat= "count", hjust=-.1)
}

# Frequency of Sessions
column_name <- "Frequency of Games"
factor_labels <- c("Less than monthly","Monthly","Twice monthly","Weekly","Twice a week","More than twice weekly")
svg(filename="~/Documents/dm_survey_data/2016_survey_frequency_of_sessions.svg", height=3, width=6)
#png(filename="~/Documents/dm_survey_data/frequency_of_sessions.png", height=450, width=1200, pointsize = 24)
build_barplot(factor_labels, column_name)
dev.off()

# Frequency of Sessions
column_name <- "Length of Games"
factor_labels <- c("Longer than eight hours","About eight hours","About six hours","About four hours","About three hours","About two hours","About an hour")
svg(filename="~/Documents/dm_survey_data/2016_survey_length_of_sessions.svg", height=3, width=6)
build_barplot(factor_labels, column_name)
dev.off()

column_name <- "Preparation Time"
factor_labels <- c("More than four hours","About four hours","About three hours","About two hours","About an hour","About 30 minutes","About 15 minutes","I don't prepare at all")
svg(filename="~/Documents/dm_survey_data/2016_survey_preparation_time.svg", height=3, width=6)
build_barplot(factor_labels, column_name)
dev.off()

# Set up a bunch of facets to show bar plots
l <- reshape(d, 
             varying = c("Campaign and Worldbuilding","Story and Adventures","Combat Encounters","NPC Development","Exploration and Roleplay","Treasure and Magic Items","Prop and Handouts"),
             v.names = "Times",
             timevar = "Activities", 
             times = c("Campaign and Worldbuilding","Story and Adventures","Combat Encounters","NPC Development","Exploration and Roleplay","Treasure and Magic Items","Prop and Handouts"), 
             direction = "long")

keeps <- c("Activities", "Times")
l <- l[keeps]
l[l=="None"] <- "None"
l[l=="About 5 minutes"] <- "5 min"
l[l=="About 15 minutes"] <- "15 min"
l[l=="About 30 minutes"] <- "30 min"
l[l=="About an hour"] <- "1 hr"
l[l=="About two hours"] <- "2 hrs"
l[l=="More than two hours"] <- "> 2 hrs"
factor_labels <- c("None","5 min","15 min","30 min","1 hr","2 hrs","> 2 hrs")
factor_charts <- c("Story and Adventures","Campaign and Worldbuilding","Combat Encounters","NPC Development","Exploration and Roleplay","Treasure and Magic Items","Prop and Handouts")

l[,"Times"] <- factor(l[,"Times"], levels = factor_labels)
l[,"Activities"] <- factor(l[,"Activities"], levels = factor_charts)
#png(filename = "~/Documents/dm_survey_data/activities.png", height=1000,width=1800)
svg(filename="~/Documents/dm_survey_data/2016_survey_preparation_activities.svg", height=6, width=6)
row_count <- nrow(d)
ggplot(l, aes(x=Times)) + geom_bar() + facet_wrap(~Activities, nrow = 3, scales="free_x") + 
  xlab(paste("Preparation Time for Specific Activities out of",nrow(d),"Respondants")) +
  ylab("Number of respodants") + 
  scale_y_continuous(expand=c(.15, 0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x=element_text())  +
  geom_text(aes(row_count=row_count, label = paste(round((..count../row_count)*100,0),"%", sep="")), 
            stat= "count", vjust=-.1, size=3)
dev.off()

table_columns <- c("Frequency of Games","Length of Games",
                   "Primary Locations","Campaign Worlds",
                   "Adventures","Preferred Combat Type","Preparation Time",
                   "Campaign and Worldbuilding","Story and Adventures",
                   "Combat Encounters","NPC Development","Exploration and Roleplay",
                   "Treasure and Magic Items","Prop and Handouts")

keeps <- c("Frequency of Games","Length of Games",
           "Locations Played","Primary Locations","Campaign Worlds",
           "Adventures","Preferred Combat Type","Preparation Time",
           "Campaign and Worldbuilding","Story and Adventures",
           "Combat Encounters","NPC Development","Exploration and Roleplay",
           "Treasure and Magic Items","Prop and Handouts")

#table_columns <- keeps

total_sentences <- c()
for (table_column in table_columns) {
  num_respondents <- nrow(d[table_column])
  question_title <- tolower(table_column)
  sentence <- paste("Of", num_respondents, "respondents on", question_title, collapse = "")
  tbl <- data.frame(sort(table(d[table_column]),decreasing = TRUE))
  if (nrow(tbl) == 1) {
    tbl <- data.frame("Activity" = table_column, "Count" = nrow(d),"Freq" = 100)
    print(tbl)
  } else {
    tbl["Percentage"] <- round(tbl["Freq"] / colSums(tbl["Freq"]) * 100, 0)
  }
  for(i in 1:nrow(tbl)) {
    answer_title <- tolower(as.character(tbl[i,1]))
    answer_percentage <- tbl[i,3]
    sentence <- paste(sentence, ", ", answer_percentage, "% answered ", answer_title, collapse="", sep = '')
  }
  sentence <- paste(sentence, ".\n", collapse="", sep = '')
  total_sentences <- c(total_sentences, sentence)
}

write(total_sentences, "~/Documents/dm_survey_data/survey_results.txt")

