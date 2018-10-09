
library(shinythemes)
library(readxl)
SpeakerScores <- read_excel("SpeakerScores 1.xlsx")
SpeakerScores <- ifelse(SpeakerScores == "Outstanding" | SpeakerScores == "Wonderful", 1, ifelse(SpeakerScores == "Good", 0, -1))
SS1 <- na.omit(SpeakerScores) 
SS1 <- colMeans(SS1)
SS1 <- as.data.frame(SS1)
SS1 <- t(SS1)
SpeakerScores1 <- read_excel("SpeakerScores.xlsx")
Sessions <- SpeakerScores1[1,]
SS2 <- rbind(SS1, Sessions)
SS3 <- as.data.frame(SS2)
SS3 <- t(SS3)
SS3 <- as.data.frame(SS3)
library(data.table)
setDT(SS3, keep.rownames = TRUE)[]
colnames(SS3) <- c("Speaker","Score", "Theme")
library(ggplot2)
library(plotly)
SS4 <- SS3
SS4$Score = as.numeric(as.character(SS4$Score))

library(flexdashboard)
library(readxl)
library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(dplyr)
library(tidyr)
library(topicmodels)
library(data.table)
library(plotly)
Sys.setlocale("LC_ALL", "C")
GB <- read_excel("Gift Bag data.xlsx")
G1 <- na.omit(GB)
G1 <- G1[-1,]
seed <- 75
colnames(G1) <- "x"
G1$x <- removePunctuation(G1$x)
G1$x <-tolower(G1$x)
G1$x <-removeNumbers(G1$x)
G1$x <-removeWords(G1$x, stopwords("en"))
new_stops <- c("<U+00A0>", stopwords("en"))
G1$x <-removeWords(G1$x, new_stops)
G1 <- as.data.frame(lapply(G1, function(x) {
  gsub("\u00A0", "", G1$x)
})) 
corpus <- Corpus(VectorSource(G1$x))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
tdm.corpus <- TermDocumentMatrix(corpus)
tdm.corpus <- removeSparseTerms(tdm.corpus, 0.99)
positive <- readLines("positive-words.txt")
negative <- readLines("negative-words.txt")
G1$positive <- tm_term_score(tdm.corpus, positive)
G1$negative <- tm_term_score(tdm.corpus, negative)
G1$score <- G1$positive - G1$negative
mean(G1$positive)
mean(G1$negative)
setDT(G1, keep.rownames = TRUE)[]
GS <- select(G1, 1,5)
colnames(GS) <- c("Respondents","Score")
T <- ggplot(SS4, aes(Theme, Score)) +   
  geom_bar(aes(fill = Theme, color=Speaker), position = "dodge", stat="identity")+scale_color_manual(values=c(rep("white", 45)))+theme(legend.position="none")
T=ggplotly(T + ggtitle("Survey Respondent's Rating of Themes"))

SS15 <- select(SS4, 2, 3)
SS16 <- aggregate(SS15[,1], list(SS15$Theme), mean)
colnames(SS16) <- c("Theme","Score")
Q <- plot_ly(
  x = SS16$Theme,
  y = SS16$Score,
  name = "Survey Respondent's Rating of Themes",
  type = "bar",  color = SS16$Theme
)

d <- plot_ly(GS, x = ~Respondents, y = ~Score, color = ~Score, colors = c('#BF382A', '#0C4B8E'),
             marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 150),
             text = ~paste('Score:', Score)) %>% layout(title = 'Sentiment Scores for Gift Bags',
                                                        scene = list(xaxis = list(title = 'Scores',
                                                                                  gridcolor = 'rgb(255, 255, 255)',
                                                                                  range = c(2.003297660701705, 5.191505530708712),
                                                                                  type = 'log',
                                                                                  zerolinewidth = 1,
                                                                                  ticklen = 5,
                                                                                  gridwidth = 2),
                                                                     yaxis = list(title = 'Respondents',
                                                                                  gridcolor = 'rgb(255, 255, 255)',
                                                                                  range = c(36.12621671352166, 91.72921793264332),
                                                                                  zerolinewidth = 1,
                                                                                  ticklen = 5,
                                                                                  gridwith = 2),
                                                                     
                                                                     
                                                                     paper_bgcolor = 'rgb(243, 243, 243)',
                                                                     plot_bgcolor = 'rgb(243, 243, 243)'))

Sessions <- c("Session 1", "Session 2", "Session 3", "Session 4", "Session 5", "Session 6")
DNA <- c(6, 7, 4, 6, 3, 5)
Attended <- c(81, 74, 72, 55, 80, 87)
A2 <- data.frame(Sessions, DNA, Attended)

z <- plot_ly(A, x = ~Attended, y = Sessions, type = 'bar', orientation = 'h', name = 'Attended',
             marker = list(color = 'rgba(246, 78, 139, 0.6)',
                           line = list(color = 'rgba(246, 78, 139, 1.0)',
                                       width = 3))) %>%
  add_trace(x = ~DNA, name = 'Did Not Attend',
            marker = list(color = 'rgba(58, 71, 80, 0.6)',
                          line = list(color = 'rgba(58, 71, 80, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack',
         xaxis = list(title = "Attendance"),
         yaxis = list(title =""))
GR <- read_excel("GB.xlsx")
GR <- ifelse(GR == "Outstanding" | GR == "Wonderful", 1, ifelse(GR == "Good", 0, -1))
GR <- na.omit(GR)
GR <- as.data.frame(GR)
setDT(GR, keep.rownames = TRUE)[]
GR <- read_excel("GB.xlsx")
GR <- ifelse(GR == "Outstanding" | GR == "Wonderful", 1, ifelse(GR == "Good", 0, -1))
GR <- na.omit(GR)
GR <- as.data.frame(GR)
setDT(GR, keep.rownames = TRUE)[]
GR1 <- GR
names(GR1) <- c("Respondents", "Score")
i <- plot_ly(x = GR1$Score, y = GR1$Respondents) %>%
  add_trace(y = rev(GR1$Respondents)) %>%
  layout(
    xaxis = list(range = c(-1, 0, 1)),
    yaxis = list(range = c(100, 150, 200))) 
GR1 <- c(77, 71, 98)
GR2 <- c(-1,0,1)
GR3 <- data.frame(GR1, GR2)
v <- plot_ly(
  x = GR3$GR2,
  y = GR3$GR1,
  name = "Score",
  type = "bar",
  color ="slateblue"
)
df1 <- data.frame(GS, label=rep('Sentiment Score', 146))
df2 <- data.frame(GR, label=rep('Recoded Score', 246))
names(df2) <- c("Respondents", "Score", "label")
df=rbind(df1, df2)
i  <- ggplot(df, aes(Score, colour=label, fill=label)) 
i  <- i + geom_density(alpha=0.55)


library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)



ui <-fluidPage(theme = shinytheme("sandstone"),  list(tags$head(HTML('<link rel="icon", href="logo.png", 
                                                                     type="image/png" />'))),
               div(style="padding: 1px 0px; width: '100%'",
                   titlePanel(
                     title="", windowTitle="TED Women"), 
                   navbarPage(title=div(img(src="logo.png"), "Women 2017 Survey Analysis"),
                              tabPanel("Intro",
                                       includeMarkdown("./intro1.Rmd"),
                                       hr()),
                              (navbarMenu("Speaker Scores", 
                                          tabPanel("Speaker Scores Recoded",
                                                   fluidPage(h1("Speaker Scores Recoded"),
                                                             p("Originally on a scale of 'Awful < Poor < Okay < Good < Wonderful < Outstanding' which I recoded such that Okay and below = -1, Good = 0, and Wonderful and up = 1. Hover over the bars to see the names of the speakers."),
                                                             plotlyOutput("plot"),  mainPanel(
                                                               plotOutput("plot3")))),
                                          # Sidebar with a slider input
                                          tabPanel("Theme Scores",
                                                   fluidPage(h1("Theme Scores Based on Speakers Scores"),
                                                             p("The scores of speakers were aggregated to visualize a 'score' for individual sessions."),plotlyOutput("plot6"),
                                                             # Show a plot of the generated distribution
                                                             mainPanel(
                                                               plotOutput("plot5")
                                                             )
                                                   )
                                          ))),
                              (navbarMenu("Gift Bag",
                                          tabPanel("Gift Bag Recoded Values",
                                                   fluidPage( h1("Gift Bag Recoded Values"),  p("Originally on a scale of 'Awful < Poor < Okay < Good < Wonderful < Outstanding' which I recoded such that Okay and below = -1, Good = 0, and Wonderful and up = 1. This bar chart shows the number of respondents who responded with each of these recoded values."),
                                                              
                                                              plotlyOutput("plot2"), 
                                                              
                                                              # Sidebar with a slider input
                                                              
                                                              # Show a plot of the generated distribution
                                                              mainPanel(
                                                                plotOutput("plot4"), width = "155%", height = "800px"))),
                                          tabPanel("Gift Bag Box Plot", fluidPage(h1("Gift Bag Sentiment Score Box Plot"), p("Open ended responses about the gift bags were pared down and then compared to comprehensive lists of positive and negative words to give each response a 'sentiment' score. This is a box plot visualization for the scores. This box plot displays the minimum, first quartile, median, third quartile, and maximum for the scores."), 
                                                                                  plotlyOutput("plot14"),  mainPanel(plotlyOutput("plot10")))),
                                          
                                          tabPanel("Gift Bag Density Plot", fluidPage(h1("Gift Bag Density Plot"), p("This density plot compares the sentiment scores versus the recoded values to showcase the distribution of scores. Notice the values with the highest density."), 
                                                                                      plotlyOutput("plot16"),  mainPanel(plotlyOutput("plot17")))),
                                          tabPanel("Gift Bag Response Examples", 
                                                   includeMarkdown("./GB1.Rmd"),
                                                   hr()))),
                              
                              (navbarMenu("CTA", (tabPanel("CTA Visual",
                                                           fluidPage(h1("Calls to Action"),
                                                                     p("Calls to action were found in the responses to questions individual sessions, to 'How well do you feel TEDWomen did, at carrying out TED's mission of spreading ideas? What could we do better?', and 'Any other comments about TEDWomen 2017?'"),
                                                                     column(width = 12, imageOutput("image1"),
                                                                            mainPanel(imageOutput("image3")))))),
                                          tabPanel("CTA Data", 
                                                   includeMarkdown("./CTADATA.Rmd"),
                                                   hr()))),
                              
                              tabPanel("Comments About FreeQuency",
                                       includeMarkdown("./Freequency.Rmd"),
                                       hr()),
                              
                              (navbarMenu("Did Not Attend Sessions", 
                                          tabPanel("Stacked Bar Chart",
                                                   fluidPage( h1("Did Not Attend Sessions"),
                                                              p("Did not attend sessions counted from comments about each individual session. Note that this isn't a full count of attendees, but the people who mentioned not attending, whereas the did attend number comes from the counts of all of the other commenters for the specified session."),
                                                              plotlyOutput("plot11"),  mainPanel(
                                                                plotOutput("plot12")))),   tabPanel("Did Not Attend Data", includeMarkdown("./DNADATA.Rmd"),
                                                                                                    hr()))),
                              tabPanel("Lessons Learned",
                                       includeMarkdown("./LL1.Rmd"),
                                       hr()))))



server <- function(input, output, session) {
  
  # renderPlotly() also understands ggplot2 objects!
  output$plot <- renderPlotly({
    T <- ggplot(SS4, aes(Theme, Score)) +   
      geom_bar(aes(fill = Theme, color=Speaker), position = "dodge", stat="identity")+scale_color_manual(values=c(rep("white", 45)))+theme(legend.position="none")
    t=ggplotly(T + ggtitle("Survey Respondent's Rating of Speakers"))
    
  })
  
  output$plot6 <- renderPlotly({
    Q
    
  })
  
  
  output$t <- renderPrint({
    
    g <- t("plotly_hover")
  }) 
  output$plot2 <- renderPlotly({
    v
    
  })
  
  output$plot16 <- renderPlotly({
    i
    
  })
  
  output$plot14 <- renderPlotly({
    p <- plot_ly(y = GS$Score, type = "box",  name = ("Sentiment Scores Gift Bags"), boxpoints = "all", jitter = 0.3,
                 pointpos = -1.8)
    
  })
  
  output$p <- renderPrint({
    
    g <- p("plotly_hover")
  })
  output$image1 <- renderImage({list(src="CTA2.png")},deleteFile=FALSE)
  
  output$plot11 <- renderPlotly({
    z
    
  })
}

shinyApp(ui, server)