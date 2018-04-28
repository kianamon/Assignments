#libraries in need:
setwd("/Users/Kianamon/R/Assignment2")
library(rvest)
library(knitr)
library(httr)
library(readr)
library(dplyr)
library(tidyr)
library(XML)
library(ggplot2)
library(stringr)
library(lubridate)
library(RCurl)
#########################################################################################
#getting the URL for the website:
fixedURL <-GET("https://www.phillymag.com", 
               set_cookies(`10up-prestitial` = "TRUE"))
xmlpage <- htmlParse(fixedURL)
length(capture.output(xmlpage))
#########################################################################################
#getting the post title:
searchfor <- "(//a|//p|//div)[@class='list-post-card']"
stories <- xpathSApply(xmlpage, searchfor, xmlValue)
PostTitle1 <- gsub("\\n(\\t){2,}(\\b[a-zA-Z])\\b{2,}(\\t){2,}", "", stories)
PostTitle2 <- gsub("\\n(\\t){2,}([a-z A-Z]){2,}(\\t){2,}", "", PostTitle1)
PostTitle3 <- gsub("((\\n){1,}(\\t){1,}){1,}([a-z A-Z]){2,}", "", PostTitle2)
PostTitle4 <- gsub("((\\n){1,}(\\t){1,})", "", PostTitle3)
df1 <- data_frame(PostTitle4)
PostTitle <- df1[2:7,]
#########################################################################################
#getting the author's name for the post:
searchfor <- "(//a|//span)[@class='byline']"
PostAuthor1 <- xpathSApply(xmlpage, searchfor, xmlValue)
df2 <- data_frame(PostAuthor1)
PostAuthor <- df2[2:7,]
#########################################################################################
#getting the post slug(red tag):
searchfor <- "(//a | //div)[@class='post-slug']"
PostSlug1 <- xpathSApply(xmlpage, searchfor, xmlValue)
df3 <- data_frame(PostSlug1)
PostSlug <- df3[6:11,]
#########################################################################################
#getting the link for the story:
searchfor <- "(//a)[@class='post-image-link']"
PostLink1 <- xpathSApply(xmlpage, searchfor, xmlGetAttr, "href")
df4 <- data_frame(PostLink1)
PostLink <- df4[6:11,]
#########################################################################################
#getting the Publication date for the post:
PublishedDate1 <- str_extract(PostLink1,"[0-9]{4}/[0-9]{2}/[0-9]{2}")
df5 <- data_frame(PublishedDate1)
Date <- df5[6:11,]
#########################################################################################
#creating the data frame:
df_LatestStories <- data.frame(Date , PostTitle, PostSlug, PostAuthor, 
                               PostLink, stringsAsFactors=FALSE)
#examining the data frame with 5 columns:
class(df_LatestStories)
head(df_LatestStories)
dim(df_LatestStories)
names(df_LatestStories)
#saving the data frame in a csv file
write.table(df_LatestStories, paste0("/Users/Kianamon/R/Assignment2/LateStories.csv"), row.names=F)
#########################################################################################
#creating the links for the stories:
df_urls <- df_LatestStories %>%
  select(PostLink1)
head(df_urls)
for (i in 1:dim(df_urls)[1]) { 
  urlforsave <- df_urls[i,1]
  page <- htmlParse(rawToChar(GET(urlforsave, set_cookies(`10up-prestitial` = "TRUE"))$content))
  searchfor <- "(//p)"
  story <- xpathSApply(page, searchfor, xmlValue)
  Articledummy <- data_frame(story)
  Article <- Articledummy[-1,]
#saving the articles in txt files
  write.table(Article, paste0("/Users/Kianamon/R/Assignment2/Article_", i,".txt"), row.names=F)
}

