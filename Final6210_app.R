#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Libraries

library(shiny)
library(ggplot2)
library(readr)
library(plotly)
library(tidytext)
library(wordcloud)
library(dplyr)

#Data Section

df <- read_csv("~/Desktop/rshiny_data_1.csv")
df2 <- read_csv("~/Desktop/rshiny_data_2.csv")

states<- group_by(df,state)
states2<- summarize(states, death = mean(death))


states <- (df%>%
               select(state)%>%
               distinct())$state
states<-c("ALL",states)

min_date<-min(df$date)
max_date<-max(df$date)


# User Interface Section
ui <- fluidPage(
    titlePanel("Social Media Sentiment Around Covid-19"),
    fluidRow(
        column(12,
               dateRangeInput("date", label='Select Dates',start='2020-03-22',end="2020-12-06"),
               
               #Output
               fluidRow(
                   column(6,
                          plotlyOutput("p1s4")
                   ),
                   column(6,
                          plotlyOutput("p1s2")
                   )
               ),
               fluidRow(
                   column(6,
                          plotOutput("p1s3")
                   ),
                   column(6,
                          plotlyOutput("p1s1")
                   )
               ),
               fluidRow(
                   column(12,
                          plotlyOutput("p1s5")      
                   )
               ))))


# Graphic Layer
server <- function(input, output) {
    
    state<- states2%>%
        mutate(state = fct_reorder(state, desc(death)))
    
    output$p1s4<-renderPlotly(   
        ggplot(state, aes(x=state, y=death, fill=state))+
            geom_bar(stat="identity", color="black", show.legend = FALSE)+
            scale_fill_brewer(palette = "Blues")+
            labs(title="Covid Death Rate By State",
                 x="State", y="Death Rate"))
    
    
    output$p1s2<-renderPlotly({
        plot_dates <- df[ df$date >= input$date[1] & df$date <= input$date[2], ]
        
        ggplot(plot_dates, aes(date))+ geom_line(aes(y=positive, color=state))+
            labs(title = "Covid Positive Cases", x="Date", y="Positive Cases" )+
            scale_x_date(date_breaks = "months", date_labels = "%m-%y")})
    
    output$p1s1<-renderPlotly(
        ggplot(df,aes(x=positive,y=death,color=state))+
            coord_cartesian(ylim=c(0,8000), xlim =c(0,150000))+
            geom_point(alpha=0.4) +
            labs(y="Death",
                 x="Positive Cases",
                 title="Positive Cases VS Death Rate by State"))
    
    output$p1s3<-renderPlot(
        wordcloud(words = df2$keyword, freq = df2$keyword_count, min.freq = 1,
                  max.words=200, random.order=FALSE, rot.per=0.35, rotate=180,
                  colors=brewer.pal(8, "Dark2"))) 
    
    
    state_sentiment <- df%>%
        mutate(state = fct_reorder(state, desc(ave_sentiment)))
    
    output$p1s5<-renderPlotly(
        ggplot(state_sentiment,aes(x=state,y=ave_sentiment, fill=state, group= state))+
            geom_bar( stat = "summary", fun= "mean", color= "black", position="dodge") +
            scale_fill_brewer(palette = "Blues") +
            labs(title= "Average Sentiment By State",
                 x= "State",
                 y= "Average Sentiment",
                 fill= "State")
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)