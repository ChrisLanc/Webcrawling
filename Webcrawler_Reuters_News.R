setwd("")
library(plyr)
library(stringr)
library(tidyverse)
library(rvest)
library(anytime)
library(SentimentAnalysis)
library(vars)
library(forecast)
library(tseries)
library(scales)
library(reshape2)
library(stargazer)
library(xtable)
library(dynlm)
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

#####Sentiment Analysis#####


sentim <- function(x){
  out <- tryCatch({
    analyzeSentiment(x, rules=list("SentimentLM"=list(ruleSentiment,loadDictionaryLM()),
                                   "PositivityLM"=list(rulePositivity,loadDictionaryLM()),
                                   "NegativityLM"=list(ruleNegativity, loadDictionaryLM())
                                   )
                     )},
  error = function(e){
    out <- data.frame()
  })
  return(out)
}



sentscores <- adply(total$text, 1, sentim)

sentscores$X1 <- as.numeric(sentscores$X1)
sentscores$X <- sentscores$X1
sentscores$X1 <- NULL
total <- merge(total, sentscores, by.x = "X")


##### word count ####

#### Some words produce error due to special signs, hence wrapping in trycatch###

wordcount <- function(x){
  out <- tryCatch({
    countWords(x)
  },
  error = function(e){
    out <- NA
  })
  return(out)
}


total$wcount <- unlist(sapply(total$text, wordcount))



####cleaning dataset####



total <- total[total$wcount > 30,]    ### delete some small articles with only reference to other sites and no information and also texts of NA from scraping

total <- total[!duplicated(total$link),] ##delete duplicates from previous merges


## calculate return variable in percent###
total$date <- as.Date(total$date)

retfun <- function(x) c(NA,exp(diff(log(x)))-1)


fdata <- aggregate(list(Close = total$Close), list(date = total$date), first) #select first row for close for each date
fdata$return.daily <- retfun(fdata$Close) ##calculate return

agg.data <- aggregate.data.frame(list(LM = total$SentimentLM,
                                      neg.LM = total$NegativityLM,
                                      pos.LM = total$PositivityLM,
                                      vol = total$Volume), list(Date = total$date), mean)

agg.data <- merge(agg.data, fdata, by.x = "Date", by.y = "date")

agg.data <- agg.data[-1,]

write.csv(agg.data, "aggdata.csv", row.names = F)

##print summary of sentiment score to latex

sumLM <- unclass(summary(agg.data$LM))
sumLM <- data.frame(sumLM)
sumLM2 <- data.frame(transpose(sumLM))
colnames(sumLM2) <- rownames(sumLM)
sumLM2$sigma.LM <- sd(agg.data$LM)
sumLM2$sigma.Neg.LM <- sd(agg.data$neg.LM)
sumLM2$sigma.Pos.LM <- sd(agg.data$pos.LM)


sumR <- unclass(summary(agg.data$adj.return))
sumR <- data.frame(sumR)
sumR2 <- data.frame(transpose(sumR))
colnames(sumR2) <- rownames(sumR)
sumR2$sigma <- sd(agg.data$adj.return)


print(xtable(sumR2, type = "latex"), file = "sumR.tex")

print(xtable(sumLM2, type = "latex"), file = "sumLM.tex")


###plot sentiment###
agg.data$Date <- as.Date(agg.data$Date)

ggplot(data=agg.data, aes(x=Date, y=LM, group=1)) +
  theme_bw() +
  geom_line(color="red")+
  geom_point() +
  ylab("Sentiment Scores") + xlab("")+
  ggtitle("Plot of Sentiment Scores over the Sample Period.")


## Prediction ##
##ARIMA Model##

#1. check stationarity

dftestout <- capture.output(print(adf.test(agg.data$return.daily)))
dftestout <- capture.output(print(adf.test(agg.data$adj.return)))

cat(dftestout,file = "dftest.txt", sep = "")


print(adf.test(agg.data$return.daily)) ###reject null of non stationarity

#2.decomposition to adjust for seasonality fluctuations 

decomp <- stl(ts(agg.data$return.daily, frequency = 365), s.window = "periodic", robust = T)

agg.data$adj.return <- decomp$time.series[,3]

###check influence with linear reg

dlm <- dynlm(agg.data$adj.return ~ L(agg.data$LM, 1))

t <- coeftest(dlm, vcov. = vcovHAC(dlm, type = "HAC"))
covlm <- vcovHAC(dlm)
se.lm <- sqrt(diag(covlm))

###lag##


dlm2 <- dynlm(agg.data$adj.return ~ L(agg.data$neg.LM, 1) + L(agg.data$pos.LM, 1))
coeftest(dlm2, vcoc. = vcovHAC(dlm2, type = "HAC"))
covlm2 <- vcovHAC(dlm2)
se.lm2 <- sqrt(diag(covlm2))

##generate lagged sentiment scores for arima model as above direct lag implementation wont work.

agg.data$lag <- c(NA, embed(agg.data$LM,2)[,2])

agg.data <- agg.data[-1,]  ## drop first observation to simplify handling later on

###
NROW(agg.data$LM[agg.data$LM < 0]) ##784
NROW(agg.data$LM[agg.data$LM > 0]) ## 9
mean(agg.data$LM)                  ## - 0.023


##Latex tables#

stargazer(dlm, dlm2, type = "html", se = list(se.lm, se.lm2), title = "Linear Model for Deseasonalized Return and Sentiment Scores",
          omit.stat = c("f", "ser"), out = "regtable.htm")

stargazer(dlm, dlm2, type = "latex", se = list(se.lm, se.lm2), title = "Linear Model for Deseasonalized Return and Sentiment Scores",
          omit.stat = c("f", "ser"), out = "regtable.tex")

#3. define breakpoint for training and test dataset and plot ACF and PACF for specifications

bpoint <- floor(NROW(agg.data$adj.return)*(29/30))

acf1 <- Acf(agg.data$adj.return[1:bpoint], lag.max = 100, main = "ACF Plot of Deseasonalized Return") ##indicates MA order of 1
pacf1 <- Pacf(agg.data$adj.return[1:bpoint], lag.max = 100, main = "PACF Plot of Deseasonalized Return") ## indicates AR order of 1


### 4. fit arima model and forecast###

fit <- auto.arima(ts(agg.data$adj.return[1:bpoint], frequency = 365), seasonal=T, stepwise = F, approximation = F, xreg = c(agg.data$lag[1:bpoint])) ## function searches for best fit, suggests (2,0,2) spec
fc <- forecast(fit, xreg = agg.data$lag[-(1:bpoint)])

fit2 <- auto.arima(ts(agg.data$adj.return[1:bpoint], frequency = 365), seasonal=T, stepwise = F, approximation = F)

fc2 <-  forecast(fit2, h = 27)


########

  
tsdisplay(residuals(fit2), lag.max = 30) # check for outlieres in residuals

accLM <- accuracy(fc,agg.data$adj.return[-(1:bpoint)])

summary(fit)

tsdisplay(residuals(fit), lag.max = 30) # check for outlieres in residuals

accN <- accuracy(fc2,agg.data$adj.return[-(1:bpoint)])

summary(fit)



##assess accuracy##


acc <- as.data.frame(accLM)
accN <- as.data.frame(accN)

print(xtable(acc, type = "latex"), file = "accplot.tex")
print(xtable(accN, type = "latex"), file = "accNplot.tex")

##test for independence of errors
Box.test(resid(fc),type="Ljung",lag=20,fitdf=4)
Box.test(resid(fc2),type="Ljung",lag=20,fitdf=4)

acf(resid(fc2))
plot(resid(fc2))


#5. Plot forecast to compare to realization

##generate df with forecast and acctualy data 

fcdf <- data.frame(list(fmean = fc2[["mean"]], fmeanLM = fc[["mean"]], flower = fc[["lower"]],
                        fupper = fc[["upper"]], actual = agg.data$adj.return[-(1:bpoint)],
                        date = agg.data$Date[-(1:bpoint)]))




colnames(fcdf)[4] <- "lower"
colnames(fcdf)[6] <- "upper"


ggplot(fcdf, aes(date, y = value, colour = variable, group = 1)) +
  theme_bw() +
  geom_line(aes(y = fmeanLM, col = "with Sentiment"), size = 1.25) +
  geom_line(aes(y = fmean, col = "w/o Sentiment"), size = 1) +
  geom_line(aes(y = actual, col = "Actual"), size = 1.5) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = "95 CI Sentiment"), inherit.aes = F, alpha=0.3) +
  scale_fill_manual("", values = "yellow") + 
  theme(legend.position = c(0.785, 0.84)) +
  ylab("Daily Return") + xlab("") +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20))



