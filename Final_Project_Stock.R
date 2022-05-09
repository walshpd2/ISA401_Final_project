if(require(pacman)== FALSE) install.packages("pacman")
pacman::p_load(tidyverse, 
               tidycensus, # for getting the census data
               httr, jsonlite, # pkgs that we might use for API,
               janitor,
               lubridate,# for making a column name from row 1
               magrittr, rvest )

library(readr)
congress_trading <- read_csv("congress_trading.csv")
View(congress_trading)

library(readr)
congress_trading_compiled <- read_csv("congress_stock_trade_open.csv")
View(congress_trading_compiled)

library(dplyr)
congress_trading_compiled= rename(congress_trading_compiled, "Index" = "...1")



unique_ticker = unique(congress_trading$Ticker)

library(readr)
stock_open_close <- read_csv("stock_open_close.csv")
View(stock_open_close)

congress_stock = read.csv('all_transactions.csv', stringsAsFactors = TRUE)


levels(congress_stock$representative)

summary(as.factor(congress_stock$representative))

library(readxl)

congress116 = readxl::read_xlsx('116congress.xlsx')
congress117 = readxl::read_xlsx('117congress.xlsx')

combined_congress = rbind(congress116,congress117)

congress_stock$representative<-as.character(congress_stock$representative)
str_split(congress_stock$representative, " ")
sapply(str_split(congress_stock$representative, " "), '[', 1)

new_member = grep(pattern = 'Hon.', x = congress_stock$representative, value = TRUE , fixed = TRUE)

congress_stock$representative = sub(pattern = 'Hon.', replacement = '', x = congress_stock$representative)

new_member2 = sub(pattern = 'Mr.' ,replacement = '', x = congress_stock$representative)

congress_stock$representative = new_member2

new_member3 = sub(pattern = 'None', replacement = '', x = congress_stock$representative)

congress_stock$representative = new_member3

congress_stock$representative

congress_stock$representative = trimws(congress_stock$representative, which = c('left'))

write_csv(x = congress_stock, file = 'new_congress_stock.csv')

updated_congress_stock = read_csv('new_congress_stock.csv')

updated_congress_stock$representative

updated_congress_stock$ptr_link

ticker_transactions = rjson::fromJSON(file = 'https://senate-stock-watcher-data.s3-us-west-2.amazonaws.com/aggregate/all_ticker_transactions.json')

updated_congress_stock$amount

## tried scraping but realized the website had it disabled

politicians = read_html('https://www.capitoltrades.com/politicians')

politicians %>% 
  html_elements(
    css = "div.q-cardlist > div.q-cardlist__content > a > article > header > h2") %>% 
  html_text2() -> member_name

politicians %>% 
  html_elements(
    css = "div.q-cardlist > div.q-cardlist__content > a > article > header > h3 > span.q-field.party.party") %>% 
  html_text2() -> member_party

trades = read_html('https://www.capitoltrades.com/trades?pageSize=50')

trades %>% 
  html_elements(css = 'div.trades-table-scroll-wrapper > table > tbody > tr > td.q-td.q-column--asset > div > div > div > span') %>% 
  html_text2() -> trade_ticker

trades %>% 
  html_elements(css = 'div.trades-table-scroll-wrapper > table > tbody > tr > td.q-td.q-column--politician > div > div > h3 > a') %>% 
  html_text2() ->trade_politician

trades %>% 
  html_elements(css = 'div.trades-table-scroll-wrapper > table > tbody > tr > td.q-td.q-column--txDate > div > div > div.q-value') %>% 
  html_text2() ->trade_month_day

trades %>% 
  html_elements(css = 'div.trades-table-scroll-wrapper > table > tbody > tr > td.q-td.q-column--txDate > div > div > div.q-label') %>% 
  html_text2() ->trade_year

trades %>% 
  html_elements(css = 'div.trades-table-scroll-wrapper > table > tbody > tr > td.q-td.q-column--txType > div > div > span') %>% 
  html_text2() ->trade_type

trades %>% 
  html_elements(css = 'div.trades-table-scroll-wrapper > table > tbody > tr > td.q-td.q-column--value > div > div > span > div > span') %>% 
  html_text2() ->trade_size

trades %>% 
  html_elements(css = 'div.trades-table-scroll-wrapper > table > tbody > tr > td.q-td.q-column--price > div > span') %>% 
  html_text2() ->trade_price

trades %>% 
  html_elements(css = 'div.trades-table-scroll-wrapper > table > tbody > tr > td.q-td.q-column--asset > div > div > div > h3 > a') %>% 
  html_text2() ->trade_company_name


congress_stock_trading = tibble(trade_politician,member_party ,trade_ticker, trade_company_name, trade_month_day,trade_year,trade_type, trade_size, trade_price)
trades = read_html('https://www.capitoltrades.com/trades')
baseurl = 'https://www.capitoltrades.com/trades'

total_results  = tibble()
pages = 2:5
for (i in pages) {
  congress_trading_url = paste0(baseurl,'?page=',
                               pages[i])
  trades = read_html(congress_trading_url)
  
  trades %>% 
    html_elements(css = 'div.trades-table-scroll-wrapper > table > tbody > tr > td.q-td.q-column--asset > div > div > div > span') %>% 
    html_text2() -> trade_ticker_new
  
  trades %>% 
    html_elements(css = 'div.trades-table-scroll-wrapper > table > tbody > tr > td.q-td.q-column--politician > div > div > h3 > a') %>% 
    html_text2() ->trade_politician_new
  
  trades %>% 
    html_elements(css = 'div.trades-table-scroll-wrapper > table > tbody > tr > td.q-td.q-column--txDate > div > div > div.q-value') %>% 
    html_text2() ->trade_month_day_new
  
  trades %>% 
    html_elements(css = 'div.trades-table-scroll-wrapper > table > tbody > tr > td.q-td.q-column--txDate > div > div > div.q-label') %>% 
    html_text2() ->trade_year_new
  
  trades %>% 
    html_elements(css = 'div.trades-table-scroll-wrapper > table > tbody > tr > td.q-td.q-column--txType > div > div > span') %>% 
    html_text2() ->trade_type_new
  
  trades %>% 
    html_elements(css = 'div.trades-table-scroll-wrapper > table > tbody > tr > td.q-td.q-column--value > div > div > span > div > span') %>% 
    html_text2() ->trade_size_new
  
  trades %>% 
    html_elements(css = 'div.trades-table-scroll-wrapper > table > tbody > tr > td.q-td.q-column--price > div > span') %>% 
    html_text2() ->trade_price_new
  
  trades %>% 
    html_elements(css = 'div.trades-table-scroll-wrapper > table > tbody > tr > td.q-td.q-column--asset > div > div > div > h3 > a') %>% 
    html_text2() ->trade_company_name_new
  
  results = tibble(x= trade_company_name_new, y = trade_month_day_new)
  
  total_results = rbind(total_results, results)
  
}

if(require(pacman)== FALSE) install.packages("pacman")
pacman::p_load(tidyverse, 
               tidycensus, # for getting the census data
               httr, jsonlite, # pkgs that we might use for API,
               janitor,
               lubridate,# for making a column name from row 1
               magrittr, rvest )

library(readr)
congress_trading <- read_csv("merge_spy_final_df_1.csv")
View(congress_trading)

congress_trading= rename(congress_trading, "Index" = "...1")

transactions_2021 = read_csv("dateoftransactions_2021.csv")

transactions_2021 = na.omit(transactions_2021)

pacman::p_load(calendR,tidyverse, rvest)
calendR(year = 2021,
        special.days = transactions_2021$Freq,
        special.col = 'red',
        gradient = T,
        title.size = 10,
        text.size = 3,
        months.size = 8,
        day.size = 3,
        margin = 0.5)

transactions_2021 %<>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))
transactions_2021[is.na(transactions_2021)] <- 0


