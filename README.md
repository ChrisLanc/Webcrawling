# Webcrawling Reuters Economic News

This repo provides the codes for constructing a webcrawler that scrapes the Reuters economic news web page in R.

Furthermore another R script is provided that uses the scraped articles and perform a dictionary based sentiment analysis based on which an ARIMA model is tuned to asses the predictory power of the extracted sentiment scores for daily movements in the S§P 500.

This was part of a seminar paper delivered in 2018.

## Webcrawler

The webcrawler is based around the Rvest package.
The crawling itself is done in two distinct steps:

1. Get information about the individual article Headers and URLs from the html structure of the Overview page.
2. Use the previously gather information to crawl every article and extract the text itself.

On my local (very old at that time) machine this ran around 16 hours scraping a total of around 14799 Articles over a course of 3 years (2015-2018)

## Sentiment Analysis

The sentiment analysis is based around the package SentimentAnalysis which uses a dictionary based approach to categorize words into different classes of sentiment

Some issues encountered in the processing that had to be dealt with were:

1. Small articles with few words only containing URLs to other articles
2. Special Symbols that could not be processed properly

## Assessment of Prediction Power

In a last stept the processed sentiment information was used in a basic ARIMA model to asses its power in improving short term forecasts of daily S§P 500 returns.

As migth have been expected beforehand, the predictory power is very low :)
