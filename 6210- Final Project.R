library(dplyr)

#Dataset 1
vac <- read.csv("~/Downloads/vaccination_all_tweets.csv", comment.char="#")
vac <- vac[,!(names(vac) %in% c("id", "user_name", "user_description", "user_created"))]

#Dataset 2
covid_daily <- read.csv("~/Downloads/archive (3)/us_states_covid19_daily.csv")

covid_daily = covid_daily[,!(names(covid_daily) %in% c("pending", "inIcuCurrently", "inIcuCumulative"))]

#Date Formatting
vac$date <- as.Date(vac$date, format = "%Y-%m-%d")
class(vac$date)

covid_daily$date <- as.Date(covid_daily$date, format = "%m/%d/%y")
class(covid_daily$date)

#vac cleaning
vac1 <- vac %>%
  select(date, text, hashtags, retweets, favorites, is_retweet, source, user_verified)

#covid_daily cleaning
covid_daily1 <- covid_daily %>%
  select(date, state, positive, negative, death, fips, total)


#Join Datasets
new_data <- vac1 %>%
  inner_join(covid_daily1, by= "date") %>%
  group_by(state)




