library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)

df <-  read.csv("~/Desktop/R/Final6210.csv")

#1) tokenize data
tokenized_data<-df%>%
  unnest_tokens(word,text)

#2) sentiment 
library(sentimentr)
library(magrittr)

tsentiments<-df %>%
  get_sentences(text)%>%
  sentiment_by(by=c("state"))

tsentiments2<-df %>%
  get_sentences(text)%>%
  sentiment_by(by=c(""))

df %<>%
  inner_join(tsentiments,by=c("state"))

#3)
library(udpipe)
pos_model<-udpipe_download_model(language="english")
pos_udmodel<- udpipe_load_model(file = "english-ewt-ud-2.5-191206.udpipe")

data_pos<- df%$%
  udpipe_annotate(pos_udmodel,text)%>%
  data.frame()%>%
  data_frame()

data_pos%<>%
  select(doc_id,upos)

pos_counts <- data_pos %>%
  group_by(doc_id,upos)%>%
  summarize(tcount = n())

df <- df %>%
  mutate(doc_id = paste("doc",1:2331,sep=""))
View(df)

#Convert the data from long form to wide form
data_pos %<>%
  inner_join(pos_counts,by=c("doc_id","upos"))%>%
  distinct()%>%
  spread(upos,tcount)%>%
  replace(.,is.na(.),0)

#Joining this to the df dataset
df%<>%
  inner_join(data_pos,by=c("doc_id"))
remove_words <- c("https", "t.co", "ä", "äôs", "äôm", "äù","äúwhat", "byywdilasc", "dvksezgglt",
                  "htxdbhibka", "oxazzyvpzw", "rupzfp8vsx", "s7yiazmfzx", "üëç", "üíø", "üôç", 
                  "ur1hvwsdlx", "uu9oqaifkb")
remove_words <- data_frame(word=remove_words)

#4 Save the table that has each keyword and each keyword count into its own variable.
df2 <- tokenized_data %>%
  select(state,word)%>%
  anti_join(get_stopwords())%>%
  anti_join(remove_words)%>%
  count(word,sort=TRUE)
View(df2)

df2 <- df2 %>% 
  rename(
    keyword = word,
    keyword_count = n
  )
pos_data_long <- df %>%
  gather(key="measure",value = "value",names(df)[17:32])

#visuals


ggplot(subset(pos_data_long,measure %in% c("ADJ","ADV", "VERB", "NOUN")),
       aes(x=value, fill=measure, group= measure))+
  geom_bar(color= "black", position = "dodge") +
  scale_fill_brewer(palette = "Blues") +
  labs(title= "POS Per State",
       x= "State",
       y= "Value",
       fill= "POS")


ggplot(subset(pos_data_long,measure %in% c("ADJ","ADV", "VERB", "NOUN")),
       aes(x=measure,y=value, fill=state, group= state))+
  geom_bar( stat = "summary", fun= "mean", color= "black", position="dodge") +
  scale_fill_brewer(palette = "Blues") +
  labs(title= "POS Count By State",
       x= "POS",
       y= "Value",
       fill= "State")

pos_data_long %<>% mutate(state = fct_reorder(state, desc(ave_sentiment)))

ggplot(pos_data_long,
       aes(x=state,y=ave_sentiment, fill=state, group= state))+
  geom_bar( stat = "summary", fun= "mean", color= "black", position="dodge") +
  scale_fill_brewer(palette = "Blues") +
  labs(title= "Average Sentiment By State",
       x= "State",
       y= "Average Sentiment",
       fill= "State") 

##############
#user input/filter
input <- list(
  date_r = c("2020-03-22","2020-12-06"),
  state = "ALL"
)

#1
#covid death by state

library(ggplot2)

states<- group_by(df,state)
states2<- summarize(states, death = mean(death))

death_data <- states2%>%
  select(death, state)

if(input$state !="ALL"){
  death_data <- death_data%>%
    filter(state == input$state)
}

graph1 <- death_data %>%
  mutate(state = fct_reorder(state, desc(death))) %>%
  ggplot(aes(x=  state, y=death, fill=state))+
  geom_bar(stat="identity",color="black", show.legend = FALSE)+
  scale_fill_brewer(palette = "Blues")+
  labs(title= paste("Covid Death Rate in ",
                    ifelse(input$state =="ALL",
                           "All States",
                           input$state), sep =""), x="State", y="Death Rate")

#2
#word cloud

graph2 <- wordcloud(words = df2$keyword, freq = df2$keyword_count, min.freq = 1,
                    max.words=200, random.order=FALSE, rot.per=0.35, rotate=90,
                    colors=brewer.pal(8, "Dark2"))
#3
#positive cases VS death rate by state

death_and_pos_data <- df%>%
  select(death, state, positive)

if(input$state !="ALL"){
  death_and_pos_data  <- death_and_pos_data %>%
    filter(state == input$state)
}


graph3 <- death_and_pos_data %>%
  
  ggplot(aes(x=positive,y=death,color=state))+
  coord_cartesian(ylim=c(0,8000), xlim =c(0,150000))+
  geom_point(alpha=0.4) +
  labs(y="Death",
       x="Positive Cases",
       title=paste("Covid Death Rate and Positive Cases in ",
                   ifelse(input$state =="ALL",
                          "All States",
                          input$state), sep =""))


#4
#Covid Positive Cases

pos_case_data <- df%>%
  select(date, state, positive)

if(input$state !="ALL"){
  pos_case_data  <- pos_case_data %>%
    filter(state == input$state)
}


graph4 <-  pos_case_data %>%
  
  ggplot(aes(date))+ geom_line(aes(y=positive, color=state))+
  labs(title = paste("Covid Positive Cases in ",
                     ifelse(input$state =="ALL",
                            "All States",
                            input$state), sep =""), 
       scale_x_date(date_breaks = "months", date_labels = "%m-%y"))




#5
#Covid Death Rate

death_rate_data_date<- df%>%
  select(death, state, date)

if(input$state !="ALL"){
  death_rate_data_date <- death_rate_data_date%>%
    filter(state == input$state)
}


graph5 <-  death_rate_data_date%>%
  
  ggplot(aes(date))+ geom_line(aes(y=death, color=state))+
  labs(title = paste("Covid Positive Cases in ",
                     ifelse(input$state =="ALL",
                            "All States",
                            input$state), sep =""))+
  scale_x_date(date_breaks = "months", date_labels = "%m-%y")


#6
#sentiment plot

state_sentiment <- df%>%
  select(ave_sentiment, state, date)

if(input$state !="ALL"){
  state_sentiment <- state_sentiment%>%
    filter(state == input$state)
}

graph6 <- state_sentiment%>%
  
  ggplot(aes(x=state,y=ave_sentiment, fill=state, group= state))+
  geom_bar( stat = "summary", fun= "mean", color= "black", position="dodge") +
  scale_fill_brewer(palette = "Blues") +
  labs(title=paste("Covid Positive Cases in ",
                   ifelse(input$state =="ALL",
                          "All States",
                          input$state), sep =""),
       x= "State",
       y= "Average Sentiment",
       fill= "State")



mutate(state = fct_reorder(state, desc(ave_sentiment)))
+
  
  
  
  
  library(plotly)

ggplotly(graph1)
ggplotly(graph2)
ggplotly(graph3)
ggplotly(graph4)
ggplotly(graph5)
