library(tidyverse)
library(quantmod)


CACHE.NAME <- 'rds/stock.rds'

read_stock.cache <- function(){
  if (file.exists(CACHE.NAME)){
    readRDS(CACHE.NAME) %>% distinct(symbol, date, .keep_all = TRUE)
  } else{
    tibble(symbol=character(), date=character(), price=numeric())
  }
}

save_stock.cache <- function(data){
  saveRDS(data %>% distinct(symbol, date, .keep_all = TRUE), CACHE.NAME)
}

check_stock_yahoo <- function(symbol){
  getSymbols(symbol, auto.assign = FALSE, from='2023-01-03')
}

get_stock_yahoo <- function(symbol, target.date){
  # have to read one by one because HK stock symbol e.g. 0700.HK causes error with auto.assign
  cat('read from yahoo', symbol, target.date, '\n')
  data <- getSymbols(symbol, from=target.date, auto.assign = FALSE)
  price <- as.numeric(Ad(data)[target.date])
  if (length(price) ==  1) price else 0
}

get_stock_data_cached <- function(symbols, start.date, end.date){
  stock.cache <- read_stock.cache()
  stopifnot(length(symbols) > 0)

  tryCatch(
    {
      start.data <- stock.cache %>% filter(symbol %in% symbols, date==start.date)
      start.missing <- setdiff(symbols, start.data$symbol)
      for (symbol in start.missing){
        price <- get_stock_yahoo(symbol, start.date)
        new.data <- tibble(symbol=symbol, price=price, date=start.date)
        start.data <- bind_rows(start.data, new.data)
        stock.cache <- bind_rows(stock.cache, new.data)
      }

      end.data <- stock.cache %>% filter(symbol %in% symbols, date==end.date)
      end.missing <- setdiff(symbols, end.data$symbol)
      for (symbol in end.missing){
        price <- get_stock_yahoo(symbol, end.date)
        new.data <- tibble(symbol=symbol, price=price, date=end.date)
        end.data <- bind_rows(end.data, new.data)
        stock.cache <- bind_rows(stock.cache, new.data)
      }
      bind_rows(start.data, end.data) %>%
        mutate(date=case_when(
          date==start.date ~ 'start.price',
          date==end.date ~ 'end.price'
        )) %>%
        pivot_wider(names_from = date, values_from = price) %>%
        filter(start.price > 0 & end.price > 0)
    },
    finally = {
      save_stock.cache(stock.cache)
    }
  )
}

get_single_stock_ret <- function(symbol, start.date, end.date){
  data <- get_stock_data_cached(symbol, start.date, end.date)
  return (data$end.price/data$start.price - 1)[0]
}
