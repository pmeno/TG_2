ChecarPrecoAcao <- function(ticker = "", dataRef = "2010-01-01")
{
  
  precoList  <- BatchGetSymbols(tickers = ticker, first.date = dataRef, last.date = offset(dataRef, 1, 'Brazil/ANBIMA'))
  precoFinal <- precoList$df.tickers[, 'price.close'][1]
  precoFinal
  
}