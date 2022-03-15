
rm(list=ls())

pkgs<-c('tidyverse','rJava','xlsx','lubridate','reshape2',
        'stringr','ggplot2','forecast','zoo','tseries','FinTS','quantmod',
        'tibbletime','httr',"shiny","TSstudio",'plotly') 

lapply(pkgs,library,character.only=T)

URL<-'https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/'
library(rvest)
library(magrittr)

pg <- read_html(URL)
# get all the Excel (xlsx) links on that page:
html_nodes(pg, xpath=".//a[contains(@href, '.xlsx')]") %>% 
  html_attr("href")  -> excel_links
excel_links

url_1 <- excel_links[10]
url_2 <- excel_links[11]
url_3 <- excel_links[12]
url_1;url_2;url_3
#the url_2 will have error in sunday because the data is not been updated in saturday.

GET(url_1, write_disk(tf1 <- tempfile(fileext = ".xlsx")))
GET(url_2, write_disk(tf2 <- tempfile(fileext = ".xlsx")))
GET(url_3, write_disk(tf3 <- tempfile(fileext = ".xlsx")))
head(tf1);head(tf2);head(tf3)
#get the period of data in excel file
x1 <- xlsx::read.xlsx(tf1,sheetIndex = 2,startRow = 4 , endRow = 4, colIndex = 2,as.data.frame = F)
x2 <- xlsx::read.xlsx(tf2,sheetIndex = 2,startRow = 4 , endRow = 4, colIndex = 2,as.data.frame = F)
x3 <- xlsx::read.xlsx(tf3,sheetIndex = 1,startRow = 4 , endRow = 4, colIndex = 2,as.data.frame = F)
x1$`4.2`;x2$`4.2`;x3$`4.2`

#strsplit(x1$`4.2`, " to ", fixed=FALSE)[[1]][1];strsplit(x1$`4.2`, " to ", fixed=FALSE)[[1]][2]
#strsplit(x2$`4.2`, " to ", fixed=FALSE)[[1]][1];strsplit(x2$`4.2`, " to ", fixed=FALSE)[[1]][2]

date1_1 <- gsub(" ", "/",gsub(" ", "/", paste("0",strsplit(x1$`4.2`, " to ", fixed = FALSE)[[1]][1],sep = "")))
date1_2 <- gsub(" ", "/",gsub(" ", "/", paste("",strsplit(x1$`4.2`, " to ", fixed = FALSE)[[1]][2],sep = "")))
date1_1;date1_2
date2_1 <- gsub(" ", "/",gsub(" ", "/", paste("0",strsplit(x2$`4.2`, " to ", fixed = FALSE)[[1]][1],sep = "")))
date2_2 <- gsub(" ", "/",gsub(" ", "/", paste("",strsplit(x2$`4.2`, " to ", fixed = FALSE)[[1]][2],sep = "")))
date2_2;date2_1

date3_1 <- gsub(" ", "/",gsub(" ", "/", paste("",strsplit(x3$`4.2`, " to ", fixed = FALSE)[[1]][1],sep = "")))
date3_2 <- gsub(" ", "/",gsub(" ", "/", paste("",strsplit(x3$`4.2`, " to ", fixed = FALSE)[[1]][2],sep = "")))
date3_2;date3_1
#time length
lag1 <- dmy(date1_2) - dmy(date1_1)
lag2 <- dmy(date2_2) - dmy(date2_1)
lag3 <- dmy(date3_2) - dmy(date3_1)
lag1;lag2;lag3
start <- c(13,28,43,58,74,89,104)
df1 <- cbind(read.xlsx2(tf1,sheetIndex = 2,startRow = start[1] ,endRow = start[1]+8, colIndex = c(2:(3+lag1))),
             read.xlsx2(tf2,sheetIndex = 2, startRow = start[1] ,endRow = start[1]+8, colIndex = c((2+1):(3+lag2) )),
             read.xlsx2(tf3,sheetIndex = 1, startRow = start[1] ,endRow = start[1]+8, colIndex = c((2+1):(3+lag3-2) ))) %>% 
  melt(id="Name" ) %>%  separate(variable,into = c("text", "num"),  sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  transmute(indicator = 1,area = Name,time = as.Date(as.numeric(num), origin = "1899-12-30"),
            value = as.numeric(value)) 

df<- list()
for (i in (1:length(start))){
  df[[i]] <- cbind(read.xlsx2(tf1,sheetIndex = 2,startRow = start[i] ,endRow = start[i]+8, colIndex = c(2:(3+lag1))),
                   read.xlsx2(tf2,sheetIndex = 2, startRow = start[i] ,endRow = start[i]+8, colIndex = c((2+1):(3+lag2) )),
                   read.xlsx2(tf3,sheetIndex = 1, startRow = start[i] ,endRow = start[i]+8, colIndex = c((2+1):(3+lag3-2) ))) %>% 
    melt(id="Name" ) %>% separate(variable,into = c("text", "num"),  sep = "(?<=[A-Za-z])(?=[0-9])") %>%
    transmute(indicator = i,region = Name,time = as.Date(as.numeric(num), origin = "1899-12-30"),
              value = as.numeric(value)) 
}
str(df)
mydata <- bind_rows(df[[1]],df[[2]],df[[3]],df[[4]],df[[5]],df[[6]],df[[7]])

