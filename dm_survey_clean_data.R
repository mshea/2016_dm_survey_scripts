library(ggplot2)
library(scales)
d <- read.csv("~/Documents/dm_survey_data/dm_survey.csv")
d <- d[d[7] != "",]
# rename columns
names(d) <- c("Timestamp","Frequency of Games","Length of Games","Locations Played",
              "Primary Locations","Campaign Worlds","Adventures",
              "Preferred Combat Type","Preparation Time","Campaign and Worldbuilding",
              "Story and Adventures","Combat Encounters","NPC Development",
              "Exploration and Roleplay","Treasure and Magic Items","Prop and Handouts",
              "Top Three Tools","Favorite Trick")

# Convert factors to characters
d[] <- lapply(d, as.character)

# Rename values for brevity
d[d=="Once a week"] <- "Weekly"
d[d=="Once a month"] <- "Monthly"
d[d=="Twice a month"] <- "Twice monthly"
d[d=="More than twice a week"] <- "More than twice weekly"
d[d=="I primarily run D&D games at home"] <- "Home"
d[d=="I primarily run D&D games online using Roll20"] <- "Roll20"
d[d=="I run a relatively equal mix of multiple methods"] <- "Equal mix"
d[d=="I primarily run D&D games at another private location"] <- "Another private location"
d[d=="I primarily run D&D games at a local game shop"] <- "Local game shop"
d[d=="I primarily run D&D games at another public location"] <- "Another public Location"
d[d=="I primarily run D&D games online using another tool"] <- "Another online tool"
d[d=="I primarily run D&D games online using Fantasy Grounds"] <- "Fantasy Grounds"
d[d=="Another D&D campaign world such as Eberron, Dark Sun, or Greyhawk"] <- "Another D&D Campaign World"
d[d=="Another Published Campaign World that isn't a traditional D&D campaign setting"] <- "Non-D&D Campaign World"
d[d=="My own personal setting"] <- "Personal setting"
d[d=="I primarily run published adventures"] <- "Published adventures"
d[d=="I primarily run my own adventures"] <- "Personal adventures"
d[d=="I prefer to use abstract maps and general positioning but without a grid."] <- "Abstract maps"
d[d=="I prefer to run narrative \"theater of the mind\" combat."] <- "Theater of the Mind"
d[d=="I prefer to run combat with maps, miniatures, and five foot per square grids."] <- "5' gridded combat"
d["Adventures"][d["Adventures"] == ""] <- "No answer"

keeps <- c("Frequency of Games","Length of Games","Locations Played",
           "Primary Locations","Campaign Worlds","Adventures",
           "Preferred Combat Type","Preparation Time","Campaign and Worldbuilding",
           "Story and Adventures","Combat Encounters","NPC Development",
           "Exploration and Roleplay","Treasure and Magic Items","Prop and Handouts",
           "Top Three Tools","Favorite Trick")

d <- d[keeps]

total_rows <- nrow(d)
total_potential_dupes <- total_rows - nrow(unique(d))


# Remove duplicate rows except those where text fields were blank.
c <- d[d["Top Three Tools"] == "" & d["Favorite Trick"] == "",]
d <- d[!d["Top Three Tools"] == "" | !d["Favorite Trick"] == "",]
d <- rbind(unique(d),c)

potential_dupes <- total_rows - nrow(d)

write.csv(d, file = "~/Documents/dm_survey_data/dm_survey_clean.csv")