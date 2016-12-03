library(ggplot2)

d <- read.csv("dm_survey_clean.csv", check.names=FALSE)
d[] <- lapply(d, as.character) # Convert factors to strings.


server <- function(input, output) {
  output$surveyPlot1 <- renderPlot({
    d <- d[d$"Frequency of Games" %in% c(input$frequency),]
    d <- d[d$"Length of Games" %in% c(input$length),]
    d <- d[d$"Primary Locations" %in% c(input$locations),]
    d <- d[d$"Campaign Worlds" %in% c(input$worlds),]
    d <- d[d$"Adventures" %in% c(input$adventures),]
    d <- d[d$"Preferred Combat Type" %in% c(input$combat),]
    d <- d[d$"Preparation Time" %in% c(input$preptime),]
    if (input$eliminateFirst == T) {
      d <- d[3446:nrow(d),]
    }
    if (input$randomSample == T) {
      d <- d[sample(nrow(d), nrow(d) / 10), ]
    }
    keeps <- c("Frequency of Games","Length of Games","Preparation Time")
    ld <- d[, (names(d) %in% keeps)]
    ld <- reshape(ld,varying = keeps,v.names = "answer", 
                  timevar = "activity", times = keeps, direction = "long")
    max_answers <- as.numeric(max(data.frame(table(ld[,2]))[,2]))
    
    output$textmain <- renderUI(HTML("<h1>2016 D&D Dungeon Master Survey</h1>"))
    output$textDescription <- renderUI(HTML("<p>Filter respondents by unchecking question responses on the left sidebar. Every question must have at least one checked box. All of the plots and result descriptions will now represent only responses that match the filters.<p>For example, if you want to see all of the results for just users of Roll 20, unselect everything but \"Roll 20\" on the \"Primary Locations\" question and hit the submit button. All of the descriptions and plots will now represent only respodents who selected Roll 20 as their primary location."))
    output$currentcount <- renderUI(HTML("<h3>",paste(prettyNum(nrow(d), big.mark = ","), "respondents currently selected.</h3>")))
    
    table_columns <- c("Campaign Worlds", "Primary Locations", 
                       "Adventures", 
                       "Preferred Combat Type")
    total_sentences <- c()
    for (table_column in table_columns) {
      num_respondents <- nrow(d[table_column])
      question_title <- tolower(table_column)
      sentence <- paste("Of", prettyNum(num_respondents, big.mark = ","), "respondents on", question_title, collapse = "")
      tbl <- data.frame(sort(table(d[table_column]),decreasing = TRUE))
      if (nrow(tbl) == 1) {
        tbl <- data.frame("Activity" = d[table_column][1,], "Count" = nrow(d),"Freq" = 100)
      } else {
        tbl["Percentage"] <- round(tbl["Freq"] / colSums(tbl["Freq"]) * 100, 0)
      }
      for(i in 1:nrow(tbl)) {
        answer_title <- tolower(as.character(tbl[i,1]))
        answer_percentage <- tbl[i,3]
        sentence <- paste(sentence, ", ", answer_percentage, "% answered ", answer_title, collapse="", sep = '')
      }
      sentence <- paste(sentence, ".", collapse="", sep = '')
      total_sentences <- c(total_sentences, sentence)
    }
    output$text1 <- renderUI(HTML(paste(total_sentences[1],"<br/><br/>")))
    output$text2 <- renderUI(HTML(paste(total_sentences[2],"<br/><br/>")))
    output$text3 <- renderUI(HTML(paste(total_sentences[3],"<br/><br/>")))
    output$text4 <- renderUI(HTML(paste(total_sentences[4],"<br/><br/>")))
    
    column_name <- "Frequency of Games"
    factor_labels <- c("Less than monthly","Monthly","Twice monthly","Weekly","Twice a week","More than twice weekly")
    d[,column_name] <- factor(d[,column_name], levels = factor_labels)
    p1 <- ggplot(d, aes(factor(d[,column_name])))
    p1 + geom_bar() + coord_flip() + theme_minimal(base_size = 18) +
      scale_y_continuous(expand=c(0, 0), limits = c(0,max_answers * 1.15)) +
      labs(x = "",
           title = column_name, y="") +
      #y=paste("Number of Respondants out of",nrow(d))) +
      
      geom_text(aes(label = scales::percent((..count..)/sum(..count..))), 
                stat= "count", hjust=-.1, size=5)
})
  output$surveyPlot2 <- renderPlot({
    d <- d[d$"Frequency of Games" %in% c(input$frequency),]
    d <- d[d$"Length of Games" %in% c(input$length),]
    d <- d[d$"Primary Locations" %in% c(input$locations),]
    d <- d[d$"Campaign Worlds" %in% c(input$worlds),]
    d <- d[d$"Adventures" %in% c(input$adventures),]
    d <- d[d$"Preferred Combat Type" %in% c(input$combat),]
    d <- d[d$"Preparation Time" %in% c(input$preptime),]
    keeps <- c("Frequency of Games","Length of Games","Preparation Time")
    ld <- d[, (names(d) %in% keeps)]
    ld <- reshape(ld,varying = keeps,v.names = "answer", 
                  timevar = "activity", times = keeps, direction = "long")
    max_answers <- as.numeric(max(data.frame(table(ld[,2]))[,2]))
    
    column_name <- "Length of Games"
    factor_labels <- c("Longer than eight hours","About eight hours","About six hours","About four hours","About three hours","About two hours","About an hour")
    d[,column_name] <- factor(d[,column_name], levels = factor_labels)
    p1 <- ggplot(d, aes(factor(d[,column_name])))
    p1 + geom_bar() + coord_flip() + theme_minimal(base_size = 18) +
      scale_y_continuous(expand=c(0, 0), limits = c(0,max_answers * 1.15)) +
      labs(x = "",
           title = column_name, y="") +
      #y=paste("Number of Respondants out of",nrow(d))) +
      
      geom_text(aes(label = scales::percent((..count..)/sum(..count..))), 
                stat= "count", hjust=-.1, size=5)
    
  })
    output$surveyPlot3 <- renderPlot({
      d <- d[d$"Frequency of Games" %in% c(input$frequency),]
      d <- d[d$"Length of Games" %in% c(input$length),]
      d <- d[d$"Primary Locations" %in% c(input$locations),]
      d <- d[d$"Campaign Worlds" %in% c(input$worlds),]
      d <- d[d$"Adventures" %in% c(input$adventures),]
      d <- d[d$"Preferred Combat Type" %in% c(input$combat),]
      d <- d[d$"Preparation Time" %in% c(input$preptime),] 
      keeps <- c("Frequency of Games","Length of Games","Preparation Time")
      ld <- d[, (names(d) %in% keeps)]
      ld <- reshape(ld,varying = keeps,v.names = "answer", 
                    timevar = "activity", times = keeps, direction = "long")
      max_answers <- as.numeric(max(data.frame(table(ld[,2]))[,2]))
      
    column_name <- "Preparation Time"
    factor_labels <- c("More than four hours","About four hours","About three hours","About two hours","About an hour","About 30 minutes","About 15 minutes","I don't prepare at all")
    d[,column_name] <- factor(d[,column_name], levels = factor_labels)
    p1 <- ggplot(d, aes(factor(d[,column_name])))
    p1 + geom_bar() + coord_flip() + theme_minimal(base_size = 18) +
      scale_y_continuous(expand=c(0, 0), limits = c(0,max_answers * 1.15)) +
      labs(x = "",
           title = column_name, y="") +
      #y=paste("Number of Respondants out of",nrow(d))) +
      
      geom_text(aes(label = scales::percent((..count..)/sum(..count..))), 
                stat= "count", hjust=-.1, size=5)
    
  })
    output$surveyPlot4 <- renderPlot({
      d <- d[d$"Frequency of Games" %in% c(input$frequency),]
      d <- d[d$"Length of Games" %in% c(input$length),]
      d <- d[d$"Primary Locations" %in% c(input$locations),]
      d <- d[d$"Campaign Worlds" %in% c(input$worlds),]
      d <- d[d$"Adventures" %in% c(input$adventures),]
      d <- d[d$"Preferred Combat Type" %in% c(input$combat),]
      d <- d[d$"Preparation Time" %in% c(input$preptime),]
      keeps <- c("Frequency of Games","Length of Games","Preparation Time")
      ld <- d[, (names(d) %in% keeps)]
      ld <- reshape(ld,varying = keeps,v.names = "answer", 
                    timevar = "activity", times = keeps, direction = "long")
      max_answers <- as.numeric(max(data.frame(table(ld[,2]))[,2]))
      
      
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
      factor_charts <- c("Campaign and Worldbuilding","Story and Adventures","Combat Encounters","NPC Development","Exploration and Roleplay","Treasure and Magic Items","Prop and Handouts")
      l[,"Times"] <- factor(l[,"Times"], levels = factor_labels)
      l[,"Activities"] <- factor(l[,"Activities"], levels = factor_charts)
      row_count <- nrow(d)
      ggplot(l, aes(x=Times)) + geom_bar() + facet_wrap(~Activities, nrow = 2, scales="free_x") + 
        xlab(paste("Preparation Time for Specific Activities out of",nrow(d),"Respondants")) +
        ylab("Number of respodants") +
        scale_y_continuous(expand=c(0, 0), limits = c(0,max_answers * 1.15)) +
        theme_minimal(base_size = 18) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              axis.title.x=element_text())  +
        geom_text(aes(row_count=row_count, label = paste(round((..count../row_count)*100,0),"%", sep="")), 
                  stat= "count", vjust=-.2, size=3)

    })
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      submitButton("Submit"),
      br(),
      checkboxInput("randomSample", "10% Random Sample"),
      checkboxInput("eliminateFirst", "Eliminate First Two days (3446 records)"),
      checkboxGroupInput("frequency", "Frequency of Games",
                         unique(d[,2]), selected = unique(d[,2])),
      checkboxGroupInput("length", "Length of Games",
                         unique(d[,3]), selected = unique(d[,3])),
      checkboxGroupInput("locations", "Primary Locations",
                         unique(d[,5]), selected = unique(d[,5])),
      checkboxGroupInput("worlds", "Campaign Worlds",
                         unique(d[,6]), selected = unique(d[,6])),
      checkboxGroupInput("adventures", "Adventures",
                         unique(d[,7]), selected = unique(d[,7])),
      checkboxGroupInput("combat", "Combat Type",
                         unique(d[,8]), selected = unique(d[,8])),
      checkboxGroupInput("preptime", "Preparation Time",
                         unique(d[,9]), selected = unique(d[,9])),
      ##<br/><br/>
      submitButton("Submit")
    ),
    mainPanel(htmlOutput("textmain"),
              htmlOutput("textDescription"),
              htmlOutput("currentcount"),
              htmlOutput("text1"),
              htmlOutput("text2"),
              htmlOutput("text3"),
              htmlOutput("text4"),
              plotOutput("surveyPlot1"),
              plotOutput("surveyPlot2"),
              plotOutput("surveyPlot3"),
              plotOutput("surveyPlot4"))
  )
)

shinyApp(ui = ui, server = server)