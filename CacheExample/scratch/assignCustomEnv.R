# an example of caching from stackoverflow
# https://stackoverflow.com/questions/2628621/how-do-you-use-scoping-assignment-in-r

new_counter <- function() {
  i <- 0
  function() {
    # do something useful, then ...
    i <<- i + 1 # pushes i to the Global environment, assign() can do the same thing
    i
  }
}

# the output is a function
counter_one <- new_counter()
counter_two <- new_counter()

counter_one() # -> [1] 1
counter_one() # -> [1] 2
counter_two() # -> [1] 1

# End of the example

# Let's try caching with downloading stock data
# uncomment to remove all objects from the environment
# remove(list = ls())
library(quantmod)

# this is a helper function, but it doesn't use <<- or assign
# it returns a function that displays the table
fetchData = function(ticker) {
  # get the data from quantmod, default is yahoo data
  Q = quantmod::getQuote(Symbol = ticker)
  table = NULL

  # returns a function that creates a table of data
  # could just return the data
  function() {
    table = data.frame(Ticker = ticker, Last = Q$Last )
    return(table)
  }
}

getStockQuote <- function(ticker, forceUpdate = FALSE) {
  # get stock quotes from yahoo, don't download data we already have
  # if forceUpdate = TRUE, redownload all the tickers

  # initialize price data vector
  price = vector(mode = "numeric", length = length(ticker))
  if (!exists('stockdataenv')) {
    print("Making new env")
    stockdataenv = new.env()
    stockdataenv <<- stockdataenv  #parent = .GlobalEnv
  } else print('Env exists')

  # Iterate through the list of tickers
  for (i in seq_along(ticker)) {
    # If the ticker is already in the environment...
    if (exists(ticker[i],where = stockdataenv, inherits = FALSE)) {
      if (forceUpdate) {
        # Force an update
        print(paste0(ticker[i],': updating data'))
        val = fetchData(ticker[i])
        assign(ticker[i], val, envir = stockdataenv, inherits = FALSE)
      } else {
        # Load data
        print(paste0(ticker[i], ': recalling data'))
        val = get(ticker[i], envir = stockdataenv, inherits = FALSE)
      }
    } else {
      # fetch data
      print(paste0(ticker[i],': getting data'))
      # # https://stackoverflow.com/questions/2628621/how-do-you-use-scoping-assignment-in-r

      # call the helper function
      val = fetchData(ticker[i])

      # save to the environment
      assign(ticker[i], val, envir = stockdataenv, inherits = FALSE)
    }

    # call val function and get "Last" price
    price[i] = val()$Last
  }

  # save to data frame
  df = data.frame(ticker = ticker, price = price)
  return(df)
}
# test fetchData
fetchData("BBT")() # it returns a function

# List of tickers to investigate
ticker = c("AAPL","MSFT","CVG","BASFY","EMN")

# Initial call to get the data
result = getStockQuote(ticker)
print(result)

# What happens if we try to re-download the data
result = getStockQuote(ticker)

# Force update "MSFT"
result = getStockQuote("MSFT", force = TRUE)

# Now we have a longer list of stocks
ticker = c("AAPL","MSFT","CVG","BASFY","EMN","PG","FITB","GE")

# Only the new stocks require downloads
result = getStockQuote(ticker)
print(result)

# Actually, we can still access all the data that quantmod downloaded
# just dig into stockdataenv...
# e.g. for AAPL
# stockdataenv$AAPL has an environment
environment(stockdataenv$AAPL)

# which contains the stock quote Q
environment(stockdataenv$AAPL)$Q

# The code could be rewritten as
fetchData2 = function(ticker) {
  # get the data from quantmod, default is yahoo data
  Q = quantmod::getQuote(Symbol = ticker)


  # returns a function that returns Q
  # could just return the data
  function() {

    return(Q)
  }
}

# The main function can then be updated as
getStockQuote2 <- function(ticker, forceUpdate = FALSE) {
  # get stock quotes from yahoo, don't download data we already have
  # if forceUpdate = TRUE, redownload all the tickers

  # initialize price data vector
  price = vector(mode = "numeric", length = length(ticker))
  if (!exists('stockdataenv')) {
    print("Making new env")
    stockdataenv = new.env()
    stockdataenv <<- stockdataenv  #parent = .GlobalEnv
  } else print('Env exists')

  # Iterate through the list of tickers
  for (i in seq_along(ticker)) {
    # If the ticker is already in the environment...
    if (exists(ticker[i],where = stockdataenv, inherits = FALSE)) {
      if (forceUpdate) {
        # Force an update
        print(paste0(ticker[i],': updating data'))
        val = fetchData2(ticker[i])
        assign(ticker[i], val, envir = stockdataenv, inherits = FALSE)
      } else {
        # Load data
        print(paste0(ticker[i], ': recalling data'))
        val = get(ticker[i], envir = stockdataenv, inherits = FALSE)
      }
    } else {
      # fetch data
      print(paste0(ticker[i],': getting data'))
      # # https://stackoverflow.com/questions/2628621/how-do-you-use-scoping-assignment-in-r

      # call the helper function
      val = fetchData2(ticker[i])

      # save to the environment
      assign(ticker[i], val, envir = stockdataenv, inherits = FALSE)
    }

    # call val function and get "Last" price
    price[i] = val()$Last
  }

  # save to data frame
  df = data.frame(ticker = ticker, price = price)
  return(df)
}

result2 = getStockQuote2("MS")
result2 = getStockQuote2(c(ticker,'BA'), forceUpdate = TRUE)
