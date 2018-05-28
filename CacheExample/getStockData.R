#remove(list = ls())
library(quantmod)

# Main function
StockInfo <- function(tickers=NULL) {
  L = length(tickers)
  if (L > 0) {
    for (i in 1:seq_along(tickers)) {
      assign(tickers[i], NULL)
    }
  }

  Data = data.frame() # initialize a data frame to store downloaded data

  fetchData <- function(syms=tickers, forceUpdate=FALSE, sortAlpha=TRUE) {
    # function for downloading data
    for (i in seq_along(syms)) {

      if (dim(Data)[1] >= 1) { # if Data already has entries
        ind = which(rownames(Data) == syms[i]) # check for existing ticker match in Data
        if (length(ind) == 0 ) { # if there is no match
          print(paste0(syms[i],': downloading'))
          Q = quantmod::getQuote(Symbol = syms[i])
          Q = cbind(Q, data.frame(Updated = Sys.time()))
          Data <<- rbind(Data, Q) # Updata Data
        } else if (forceUpdate == TRUE) { # If the ticker is already in Data, but want to
          # re-download
          print(paste0(syms[i],': force update, downloading'))
          Q = quantmod::getQuote(Symbol = syms[i])
          Q = cbind(Q, data.frame(Updated = Sys.time()))
          Data[ind,] <<- Q
        } else {
          # if the ticker already exists
          print(paste0(syms[i],' already exists, not updating'))
        }

      } else {
        # if Data is empty
        print(paste0(syms[i],': downloading'))
        Q = quantmod::getQuote(Symbol = syms[i])
        Q = cbind(Q, data.frame(Updated = Sys.time()))
        Data <<- rbind(Data, Q)
      }

      # if adding a symbol using fetchData, need to update tickers
      if (!(syms[i] %in% tickers)) {
        tickers <<- c(tickers,syms[i])
      }
    }
    if (sortAlpha) { # sort alphabetically
      inds = sort(rownames(Data),index.return = TRUE)
      Data <<- Data[inds$ix,]
      tickers <<- tickers[inds$ix]
    }
  }
  getData <- function() { # Displays the data
    return(Data)
  }

  getTickers <- function() return(tickers)

  deleteData <- function(sym) {
    # Delete a row from Data based on sym
    inds = which(rownames(Data) != sym)
    Data <<- Data[inds,]
    inds = which(tickers != sym)
    tickers <<- tickers[inds]
  }
  if (L > 0) fetchData()

  list(getData = getData, fetchData = fetchData, deleteData = deleteData)

}


tickers = c('FITB','CVG','PG','GE','PNC')
si = StockInfo(tickers) # initialize
si$getData() # display Data
si$fetchData() # attempt to fetch Data for the stored tickers
si$fetchData('MS') # fetch data for an additional stock
si$deleteData('MS') # delete that stock

tickers = c('FITB','CVG','PG','GE','PNC','MEDP')
si$fetchData(tickers) # fetch the same tickers + medpace
si$getData() # display Data


si$fetchData('PG') # fetch a stock that is already in Data
si$fetchData('PG',forceUpdate = TRUE) # force an update
si$getData()

# what if no tickers are entered?
si0 = StockInfo()

# Lookup things in the environment
environment(si$getData)
environment(si$fetchData) # the functions have the same environment!
ls(environment(si$getData))
environment(si$getData)$Data
environment(si$getData)$tickers






