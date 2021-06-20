ChecarPrecoAcao <- function(dados = data.table(), dataMercado = "2010-01-01", tipoAcao)
{
  
  tickers <- dados[ , unique(get(paste0('Ticker_', tipoAcao, '_YAHOO')))]
  precos  <- BatchGetSymbols(tickers, first.date = dataMercado, last.date = offset(dataMercado, 252, 'Brazil/ANBIMA'), do.fill.missing.prices = T, thresh.bad.data = 0, do.cache = T)
  
  precos <- precos$df.tickers
  setDT(precos)
  
  #precos[, .N, by = data.ref][order(N)]
  
  precos <- precos[, .(price.adjusted, ref.date, ticker)]
  setnames(precos, colnames(precos), c('preco', 'dataPreco', paste0('Ticker_', tipoAcao, '_YAHOO')))
  
  if(tipoAcao == 'ON')
    precos <- merge(precos, dados[, .(Ticker_ON_YAHOO, CD_CVM)], by = paste0('Ticker_', tipoAcao, '_YAHOO'))
  else
    precos <- merge(precos, dados[, .(Ticker_PN_YAHOO, CD_CVM)], by = paste0('Ticker_', tipoAcao, '_YAHOO'))
  
  precos[, tickerTipo := tipoAcao]
  setnames(precos, paste0('Ticker_', tipoAcao, '_YAHOO'), 'ticker')
  
  precos
}