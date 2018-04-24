setwd("/Users/Kianamon/R")
library(httr)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
#I have downloaded the csv files directly since the links on the assignments 
#description were not working directly from rstudio, because expenditure data frame
#was too big! :)
df_outcome <- read_csv("df_outcome.csv")
df_expend <- read_csv("df_expend.csv")
names(df_expend)[names(df_expend) == 'Financing scheme'] <- 'Finance'
dim(df_expend)
#filtering expenditure:
df1 <- df_expend %>%
  filter(Year==2015 & Measure=="Share of gross domestic product" & 
           Function=="Current expenditure on health (all functions)" & 
           Finance == "All financing schemes")
df2 <- df1 %>%
  filter(Provider == "All providers")
#selecting spnding column:
df3 <- df2 %>%
  select(Country, Value, Year)
names(df3)[names(df3) == 'Value'] <- 'Spending'


#calculating life expectancy for males
df_outcome2 <- df_outcome %>%
  filter(Year==2015, Variable == 'Males at birth', Measure == 'Years') %>%
  select(Country, Variable, Measure, Year, Value)
#calculating life expectancy for females
df_outcome3 <- df_outcome %>%
  filter(Year==2015, Variable == 'Females at birth', Measure == 'Years') %>%
  select(Country, Variable, Measure, Year, Value)
df_life <- full_join(df_outcome2, df_outcome3, by=c("Country"="Country"))

df_life <- df_life %>%
  mutate(AvgLifeExpect=(Value.x+Value.y)/2) %>%
  select(Country, AvgLifeExpect)

df_health <- inner_join(df3, df_life, by=c("Country"="Country"))
df_health <- df_health %>%
  mutate(Life=AvgLifeExpect/Spending) %>%
  arrange(desc(Life))

dim(df_health)
head(df_health, 35)
tail(df_health, 20)
ggplot(data=df_health, aes(x=Country, y=Life)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))