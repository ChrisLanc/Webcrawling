setwd("")
library(plyr)
library(stringr)
library(tidyverse)
library(rvest)
library(anytime)

total <- read.csv("sentdata.csv", header = T, stringsAsFactors = F) ##cleaned without pos and neg
agg.data <- read.csv("aggdata.csv", header = T, stringsAsFactors = F)
fcdf <- read.csv("fcast.csv", header = T, stringsAsFactors = F)
#### Functions to structure scraped data ####


##vector with all available pages of content##

base_url <- "https://www.reuters.com/news/archive/economicNews?view=page&page="
nr_pages <- 2000
urls <- paste0(base_url, 1:nr_pages)


## Function to parse text from the html data of the overview page## 

parse_overview <- function(x){
  if(is.na(x)==FALSE){
  tibble(date = html_text(html_nodes(x, ".timestamp"), TRUE),
         title = html_text(html_nodes(x, ".story-title"), TRUE),
         link = html_attr(html_nodes(x, ".story-content a"), "href"))
  }
  else return(NULL)
}


##function to collapse multi line output and drop possibly empty lines##

collapse_to_text <- function(x){
  p <- html_text(x, trim = TRUE)
  p <- p[p != ""]                   
  paste(p, collapse = "\n")
}


##function to parse text from actual articles

parse_result <- function(x){
  if(is.na(x)==FALSE){
  tibble(section = html_text(html_node(x, ".body_1gnLA"), trim = TRUE))
  }
  else return(NULL)
}

## read_html function with error treatment and random sleep time to avoid booting

####

sleeptime <- seq(1, 3, by=0.001)


readin <- function(x){
  out <- tryCatch({
    read_html(x)
  }, 
  error = function(e){
    return(NA)
  }, 
  finally={
    Sys.sleep(sample(sleeptime, 1))
  })
return(out)
}

write.csv(overview_content, "overview.csv")



### scraping overview page###


overview_content <- urls %>% 
  map(readin) %>% 
  map_df(parse_overview)

##fix incomplete url###

overview_content$link <- paste("https://www.reuters.com",overview_content$link, sep = "")

### read saved data and financial data###

overview_content <- read.csv("overview.csv", stringsAsFactors = F)
SPdata <- read.csv("SPdata.csv", sep = ",", header = T, stringsAsFactors = F)

##convert date variables##

overview_content$date[1:3] <- "May 07 2018"  ##as only time of the day of scraping was given

overview_content$date <- anydate(overview_content$date)
SPdata$Date <- anydate(SPdata$Date)


## merge by date##

total2 <- merge(overview_content, SPdata, by.x = "date", by.y = "Date")


###scraping individual pages###

write.csv(total, "sentdata.csv")    ##Names dont fit as writing and reading not in displayed order



total$text <- total$link %>% 
  map(readin) %>% 
  map(parse_result)


total2$text[sapply(total2$text, is.null)] <- NA ##as Null elements listed and unlisting returns vector of unequal length

total2$text <- unlist(total2$text)






