#set the working directory
setwd("/Users/Kianamon/R/assignment3")
#Here is the code for visulization of data for Health Care Expenditure worldwide
#At the output of this program you will get 4 PDF files containing the relavant figures:
#######################################################################################
#libraries in use:
library(httr)
library(readr)
library(dplyr)
library(tidyr)
library(XML)
library(ggplot2)
library(stringr)
library(rworldmap)
library(grid)
library(gridExtra)
#######################################################################################
#check for missing packages and install them:
list.of.packages <- c("gridExtra", "grid", "rworldmap")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
#######################################################################################
#I have downloaded the csv files directly since the links on the assignments 
#description were not working directly from rstudio, because expenditure data frame
#was too big! :)
df_outcome <- read_csv("df_outcome.csv")
df_expend <- read_csv("df_expend.csv")
names(df_expend)[names(df_expend) == 'Financing scheme'] <- 'Finance'
#######################################################################################
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
head(df3, 20)
dim(df3)
#mapping the total expenditure as a percentage of the GDP in 2015:
#######################################################################################
#Europe:
pdf("Europe.pdf", width = 16, height = 16)
n1 <- joinCountryData2Map(df3, joinCode="NAME", nameJoinColumn="Country")
country_coord<-data.frame(coordinates(n1),stringsAsFactors=F)
text(x=country_coord$X1,y=country_coord$X2,labels=row.names(country_coord))
mapCountryData(n1, nameColumnToPlot="Spending", 
               mapTitle=
                 "Europe's Total Expenditure on Health Care as percentage of the GDP in 2015",
               mapRegion="Europe",
               colourPalette="rainbow",
               addLegend=TRUE,
               oceanCol="lightblue", missingCountryCol="grey")
text(x=country_coord$X1,y=country_coord$X2,labels=row.names(country_coord), cex=0.4)
dev.off()
#######################################################################################
#calculating life expectancy for males
df_outcome2 <- df_outcome %>%
  filter(Year==2015, Variable == 'Males at birth', Measure == 'Years') %>%
  select(Country, Variable, Measure, Year, Value)
#calculating life expectancy for males over the years
df_expect1 <- df_outcome %>%
  filter(Variable == 'Males at birth', Measure == 'Years') %>%
  select(Country, Variable, Measure, Year, Value)
head(df_expect1)
#######################################################################################
#calculating life expectancy for females
df_outcome3 <- df_outcome %>%
  filter(Year==2015, Variable == 'Females at birth', Measure == 'Years') %>%
  select(Country, Variable, Measure, Year, Value)
#calculating life expectancy for females over the years
df_expect2 <- df_outcome %>%
  filter(Variable == 'Females at birth', Measure == 'Years') %>%
  select(Country, Variable, Measure, Year, Value)
head(df_expect2)
#######################################################################################
#ploting life expectancy of males and females in each country over the years:
expectM <- ggplot(data = df_expect1, 
                  aes(x = Year, y = Value, col = factor(Country)))+
  geom_point(size = 2, alpha = 0.6) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(y = "Life Expectancy in Males") 

expectF <- ggplot(data = df_expect2, 
                  aes(x = Year, y = Value, col = factor(Country)))+
  geom_point(size = 2, alpha = 0.6) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(y = "Life Expectancy in Females")

#extract the legend:
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend<-g_legend(expectM)
pdf("US_Costs.pdf", width = 16, height = 16)
grid.arrange(mylegend,                             
             arrangeGrob(expectM + theme(legend.position="none"), 
                         expectF + theme(legend.position="none"), 
                         ncol = 2), 
             nrow = 2)  
dev.off()
#######################################################################################
df_life <- full_join(df_outcome2, df_outcome3, by=c("Country"="Country"))
#######################################################################################
#average life expetancy for males and females:
df_life <- df_life %>%
  mutate(AvgLifeExpect=(Value.x+Value.y)/2) %>%
  select(Country, AvgLifeExpect)
#######################################################################################
#calculating the united states' expenditure on health care over the years:
df_expend2 <- df_expend %>%
  filter(Country == "United States", 
         Provider == "All providers", 
         Measure=="Share of gross domestic product", 
         #Function=="Current expenditure on health (all functions)", 
         Finance == "All financing schemes")
head(df_expend2, 17)
dim(df_expend2)
pdf("CategoriesExpenditure_US.pdf", width = 16, height = 16)
ggplot(data=df_expend2, aes(x=Year, y=Value, col = Function)) +
  geom_point() + 
  geom_line() +
  labs(y = "United States Health Care Costs in Different Categories") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
dev.off()
#######################################################################################
#joining the datasets:
df_health <- inner_join(df3, df_life, by=c("Country"="Country"))
df_health <- df_health %>%
  mutate(Life=AvgLifeExpect/Spending) %>%
  arrange(desc(Life))
tail(df_health)
#######################################################################################
pdf("World_Life.pdf", width = 16, height = 16)
n3 <- joinCountryData2Map(df_health, joinCode="NAME", nameJoinColumn="Country")
country_coord3<-data.frame(coordinates(n3),stringsAsFactors=F)
mapCountryData(n3, nameColumnToPlot="Life", mapTitle=
                 "World's Life Expectancy to Health Care Total Expenditure Ratio",
               colourPalette="rainbow",
               addLegend=TRUE,
               oceanCol="lightblue", missingCountryCol="grey")
text(x=country_coord3$X1,y=country_coord3$X2,labels=row.names(country_coord3), cex=0.1)
dev.off()
#######################################################################################