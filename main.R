#### Libs utilizadas ######

require(data.table)
require(BatchGetSymbols)
require(GetDFPData2)
require(GetFREData)
require(bizdays)
require(stringr)
require(quantmod)
require(Quandl)
require(gtrendsR)

### CAMINHOS NECESSÁRIOS #####

caminhoPrincipal <- "C:/Users/pedro/Desktop/TCC/PARTE_2/codes/TG_2/"
caminhoBases     <- "C:/Users/pedro/Desktop/TCC/PARTE_2/codes/TG_2/bases/"
caminhoTickers   <- paste0(caminhoBases, "tickerBolsa.csv")
caminhoQtdAcoes  <- paste0(caminhoBases, "baseQtdAcoes.csv")
caminhoico2      <- paste0(caminhoBases, "ico2.csv")
caminhoise       <- paste0(caminhoBases, "ise.csv")

### Sources #####

source(paste0(caminhoPrincipal, 'ChecarPrecoAcao.R'))
source(paste0(caminhoPrincipal, 'DefineQtdAcoes.R'))
source(paste0(caminhoPrincipal, 'DiasUteisPreco.R'))
source(paste0(caminhoPrincipal, 'CalcularValorMercado.R'))
source(paste0(caminhoPrincipal, 'RetornaValorContabil.R'))
source(paste0(caminhoPrincipal, 'ClassificarBigSmall.R'))
source(paste0(caminhoPrincipal, 'ClassificarLowMediumHigh.R'))
source(paste0(caminhoPrincipal, 'DividirCarteiras3Fatores.R'))
source(paste0(caminhoPrincipal, 'CalcularRetornoMensal.R'))
source(paste0(caminhoPrincipal, 'copiarTabela.R'))


### Declaracoes de variaveis ####

precosFinal_ON <- data.table()
precosFinal_PN <- data.table()

#Taxa free-rate
selic <- setDT(Quandl(code = 'BCB/4390'))
selic[, year := year(Date)]
selic[, month := month(Date)]
selic <- selic[year >= 2010 & year <= 2019]
setnames(selic, 'Value', 'selic')
selic[, selic := selic/100]
selic <- selic[, .(year, month, selic)]

#Carteiras teste

ibov <- setDT(BatchGetSymbols('^BVSP', first.date = '2010-01-01', '2020-01-01', freq.data = 'monthly', how.to.aggregate = 'last', do.complete.data = T, do.fill.missing.prices = T)$df.ticker)
ibov <- ibov[, .(ref.date, ret.adjusted.prices)]
ibov[, year := year(ref.date)]
ibov[, month := month(ref.date)]
setnames(ibov, 'ret.adjusted.prices', 'Rc')
ibov <- ibov[is.na(Rc), Rc := 0]
ibov <- ibov[, .(year,month, Rc)]

### CARREGAR TODAS AS EMPRESAS DA BOLSA DE 2010 ATÉ HOJE ########

empresasBolsa <- get_info_companies()
tickersBolsa  <- read.csv(caminhoTickers, sep = ';', encoding = 'UTF-8', header = T, na.strings = "", stringsAsFactors = FALSE, colClasses = c('character','character','character','character','character'))
acoesQtdBolsa <- read.csv(caminhoQtdAcoes, sep = ';', encoding = 'UTF-8', header = T, na.strings = "NA", stringsAsFactors = FALSE)
ico2          <- read.csv(caminhoico2, sep = ';', encoding = 'UTF-8', header = T, na.strings = "", stringsAsFactors = FALSE)
ise           <- read.csv(caminhoise, sep = ';', encoding = 'UTF-8', header = T, na.strings = "", stringsAsFactors = FALSE)

setDT(empresasBolsa)
setDT(tickersBolsa)
setDT(acoesQtdBolsa)
setDT(ico2)
setDT(ise)

ico2 <- ico2[year >= 2010 & year <= 2019]
ise  <- ise[year >= 2010]

indSust <- merge(ico2, ise, by = c('year', 'month'), suffixes = c('.ico2', '.ise'))

empresasBolsa <- empresasBolsa[, .(CD_CVM, DENOM_SOCIAL, DENOM_COMERC, SETOR_ATIV, CNPJ, DT_REG, DT_CANCEL, MOTIVO_CANCEL, SIT_REG, CATEG_REG, SIT_EMISSOR, TP_MERC)]

#Formatando colunas para as classes necessarias

empresasBolsa[, DT_REG := as.Date(DT_REG, '%d/%m/%Y')]
empresasBolsa[, DT_CANCEL := as.Date(DT_CANCEL, '%d/%m/%Y')]

acoesQtdBolsa[, DT_REFER := as.Date(DT_REFER, format = '%Y-%m-%d')]
acoesQtdBolsa <- DefineQtdAcoes(acoesQtdBolsa)
acoesQtdBolsa[, CNPJ := as.character(as.numeric(str_remove_all(acoesQtdBolsa$CNPJ_CIA, pattern = '[./-]')))]

# Filtrando apenas empresas que queremos calcular os parâmetros.

empresasBolsa <- empresasBolsa[is.na(DT_CANCEL) & (SIT_REG == 'ATIVO') & (CATEG_REG == 'Categoria A') & (is.na(TP_MERC) | TP_MERC != 'BALCÃO NÃO ORGANIZADO')]
empresasBolsa <- empresasBolsa[SIT_EMISSOR %chin% c("FASE OPERACIONAL", "FASE PRÉ-OPERACIONAL")]
empresasBolsa <- merge(empresasBolsa, tickersBolsa, by = c('CNPJ', 'DENOM_SOCIAL'), all.x = TRUE)


valorMercado <- dcast(acoesQtdBolsa, CNPJ_CIA + DENOM_CIA + CD_CVM ~ stock.type, value.var = 'qtd.issued', fun.aggregate = sum)
valorMercado[, 'NA' := NULL]

valorMercado <- merge(valorMercado, empresasBolsa[, .(DENOM_SOCIAL, CD_CVM, DT_REG, Ticker_ON, Ticker_PN, Ticker_UN)], by = 'CD_CVM', all.y = T)

# Não queremos empresas registradas em 2020 para frente, data de referencia de dados de ON e PN maior que 2020 e zerados
valorMercado <- valorMercado[DT_REG < '2020-01-01']
valorMercado <- valorMercado[(ON != 0) && (PN != 0)]

#Criando data que os preços serão procurados e string de tickers

datasPreco <- DiasUteisPreco(2010:2019)

# Formatando tickers no formato necessário
valorMercado[, Ticker_ON_YAHOO := paste0(Ticker_ON, '.SA')]
valorMercado[, Ticker_PN_YAHOO := paste0(Ticker_PN, '.SA')]

#debug
#valorMercado <- valorMercado[CD_CVM %in% c(19348,9512,5410,22470,23264)]

precosON <- lapply(datasPreco, function(dataON)
  {
      print(sprintf("Baixando precos para as ações ON desde:%s", dataON))
      result <- ChecarPrecoAcao(valorMercado, dataON, 'ON')
      result
  })

precosON <- rbindlist(precosON)

precosPN <- lapply(datasPreco, function(dataPN)
{
  print(sprintf("Baixando precos para as ações ON desde:%s", dataPN))
  result <- ChecarPrecoAcao(valorMercado, dataPN, 'PN')
  result
})

precosPN <- rbindlist(precosPN)

precosFinal <- rbind(precosON, precosPN)

setkey(precosFinal, 'ticker', 'dataPreco')
precosFinal <-unique(precosFinal)

valorMercadoCalculado <- CalcularValorMercado(precosFinal, acoesQtdBolsa)

#Remover anomalias
valorMercadoCalculado <- valorMercadoCalculado[!(preco_ON < 0 | preco_PN < 0)]
valorMercadoCalculado <- valorMercadoCalculado[!(preco_ON >= 500 | preco_PN >= 500)]
valorMercadoCalculado <- valorMercadoCalculado[valorDeMercado != 0]
valorMercadoCalculado <- valorMercadoCalculado[, .SD[1], by = .(year(dataPreco), CD_CVM)]

#Adicionar tickers ON e PN
ValorMercadoFinal <- merge(valorMercadoCalculado, valorMercado[, .(CD_CVM, Ticker_ON, Ticker_PN)], by = 'CD_CVM', all.x = TRUE, allow.cartesian = TRUE)
ValorMercadoFinal <- ValorMercadoFinal[, .SD[1], by = .(CD_CVM, year)]

patrimonioLiquido <- lapply(datasPreco, function(ano)
{
  print(sprintf("Baixando balanço patrimonial para as ações no ano de: %s", year(ano)))
  result <- RetornaValorContabil(ValorMercadoFinal[, unique(CD_CVM)], ano, 'con')
  result
})

patrimonioFinal <- rbindlist(patrimonioLiquido)
patrimonioFinal[, year := year(DT_REFER)]

patrimonioFinal <- merge(ValorMercadoFinal, patrimonioFinal, by = c('CD_CVM', 'year'), all.x = T, allow.cartesian = T)

acoesFaltantes <- patrimonioFinal[is.na(VL_CONTA)]
acoesFaltantes[, DT_REFER := NULL]
acoesFaltantes[, ESCALA_MOEDA := NULL]
acoesFaltantes[, DS_CONTA := NULL]
acoesFaltantes[, VL_CONTA := NULL]

patrimonioLiquidoInd <- lapply(datasPreco, function(ano)
{
  codigoCVM <- acoesFaltantes[year == year(ano), unique(CD_CVM)]
  print(sprintf("Baixando balanço patrimonial invidual para as ações no ano de: %s", year(ano)))
  result <- RetornaValorContabil(codigoCVM, ano, 'ind')
  result[, year := year(DT_REFER)]
  result
})

patrimonioFinalFaltantes <- rbindlist(patrimonioLiquidoInd)
patrimonioFinalFaltantes <- merge(acoesFaltantes, patrimonioFinalFaltantes, by = c('CD_CVM', 'year'), all.x = TRUE)

insumos3Fatores <- rbind(patrimonioFinal, patrimonioFinalFaltantes)
insumos3Fatores <- insumos3Fatores[!is.na(VL_CONTA)]
insumos3Fatores <- insumos3Fatores[, valorPatrimonio := ifelse(ESCALA_MOEDA == 'MIL', VL_CONTA*1000, VL_CONTA)]
insumos3Fatores[, VC_VM := valorPatrimonio/valorDeMercado]
#Retirando valores negativos e outliers
insumos3Fatores <- insumos3Fatores[!(VC_VM <= 0 | VC_VM >=100)]

# Classificando empresas pequenas e grandes
insumos3Fatores <- ClassificarBigSmall(insumos3Fatores)

#Classificando empresas de crescimento
insumos3Fatores <- ClassificarLowMediumHigh(insumos3Fatores)

#Montando as carteiras para o modelo de 3 fatores
insumos3Fatores[, carteira := paste0(S_B, '/', L_M_H)]

carteiras <- DividirCarteiras3Fatores(insumos3Fatores)
insumoWide <- melt.data.table(insumos3Fatores, c('year', 'valorDeMercado', 'valorPatrimonio', 'carteira'), measure.vars = c('Ticker_ON', 'Ticker_PN'), variable.name = 'ticker')
insumoWide[, ticker := NULL]
setnames(insumoWide, 'value', 'ticker')
insumoWide <- insumoWide[!(is.na(ticker))]

resultCateiras <- lapply(carteiras, function(c)
  {
    tipoCarteira <- c[, unique(carteira)]
    tickerON <- c[!is.na(Ticker_ON), unique(Ticker_ON)]
    tickerPN <- c[!is.na(Ticker_PN), unique(Ticker_PN)]
    ticker   <- c(tickerON, tickerPN)
    retorno  <- CalcularRetornoMensal(c, ticker)
    retorno[, carteira := tipoCarteira]
    retorno  <- merge(retorno, insumoWide, by = c('ticker', 'year', 'carteira'))
    retorno  <- retorno[!(is.na(retMensal))]
    retorno[, quantilBaixa := quantile(retMensal, 0.001), by = .(year, carteira)]
    retorno[, quantilAlta := quantile(retMensal, 0.999), by = .(year, carteira)]
    retorno <- retorno[retMensal >= quantilBaixa & retMensal <= quantilAlta]
    retorno[, retPonderado := weighted.mean(retMensal, valorDeMercado, na.rm = T), by = .(year,month)]
    retorno
  })

carteiraTotal <- rbindlist(resultCateiras)

#Montando a carteira de mercado

ticker   <- insumoWide[!is.na(ticker), unique(ticker)]
retorno  <- CalcularRetornoMensal(carteiraTotal, ticker)
retorno  <- retorno[!is.na(retMensal)]
retorno  <- merge(retorno, insumoWide, by = c('ticker', 'year'))
retorno[, quantilBaixa := quantile(retMensal, 0.005, na.rm = T), by = .(year)]
retorno[, quantilAlta := quantile(retMensal, 0.995, na.rm = T), by = .(year)]
retorno <- retorno[retMensal >= quantilBaixa & retMensal <= quantilAlta]
retorno[, retPonderado := weighted.mean(retMensal, valorDeMercado, na.rm = T), by = .(year,month)]
retorno[, carteira := 'FULL']
fatorMercado <- retorno[, median(retPonderado), by = .(year,month)]
setnames(fatorMercado, 'V1', 'MKT')

#Montando a carteira para calculo do modelo de 4 fatores

anosPreco4Fatores <- c(2010:2019)

resultCateiras4Fatores <- lapply(anosPreco4Fatores, function(ano)
{
  ticker      <- insumoWide[!is.na(ticker) & year == ano, unique(ticker)]
  ret4Fatores <- GetPricesComDatasMaxMin(ticker, paste0((ano-1), '-04-01'), paste0(ano, '-06-30'))  
  ret4Fatores <- ret4Fatores[!is.na(retPeriodo) | retPeriodo != 0]
  ret4Fatores <- ret4Fatores[retPeriodo <= 10 | retPeriodo > -1]
  ret4Fatores <- ret4Fatores[, quantilWinLos := quantile(retPeriodo, 0.5), by = year(ref.date)]
  ret4Fatores <- ret4Fatores[, carteiraWinLos := ifelse(retPeriodo <= quantilWinLos, 'L', 'W')]
  ret4Fatores <- ret4Fatores[, year := year(ref.date)]
  ret4Fatores
})

carteiraMomentum <- rbindlist(resultCateiras4Fatores)


# Calculo dos 3 fatores do modelo

tresFatores <- carteiraTotal[, median(retPonderado), by = .(carteira, month, year)]
tresFatores <- dcast.data.table(tresFatores, year + month ~ carteira, value.var = 'V1')
tresFatores[, SMB := (`S/H` + `S/L` + `S/M`)/3 - (`B/H` + `B/L` + `B/M`)/3]
tresFatores[, HML := (`S/H` + `B/H`)/2 - (`S/L` + `B/L`)/2]
tresFatores <- merge(tresFatores, selic[, .(year, month, selic)], by = c('year', 'month'))
tresFatores <- merge(tresFatores, fatorMercado[, .(year, month, MKT)], by = c('year', 'month'))
tresFatores <- merge(tresFatores, ibov[, .(year, month, Rc)], by = c('year', 'month'))
tresFatores <- merge(tresFatores, indSust, by = c('year', 'month'))

# Calculo dos 4 fatores do modelo

insumos4Fatores <- merge(carteiraTotal, carteiraMomentum, by = c('ticker', 'year'))
insumos4Fatores[, carteira4Fatores := paste0(carteira, '/', carteiraWinLos)]
carteira4Fatores <- insumos4Fatores[, median(retMensal), by = .(carteira4Fatores, month, year)]
carteira4Fatores[, carteira4Fatores := str_remove_all(carteira4Fatores, '/')]
quatroFatores <- dcast.data.table(carteira4Fatores, year + month ~ carteira4Fatores, value.var = 'V1')
quatroFatores[, SMB := ((SHL + SHW + SLL + SLW + SML + SMW)/6) - ((BHL + BHW + BLL + BLW + BML + BMW)/6)]
quatroFatores[, HML := ((SHL + SHW + BHL + BHW)/4) - ((SLL + SLW + BLL + BLW)/4)]
quatroFatores[, WinMLos := ((BHW + BLW + BMW + SHW + SLW + SMW)/6) - ((BHL + BLL + BML + SHL + SLL + SML)/6)]
quatroFatores <- merge(quatroFatores, selic[, .(year, month, selic)], by = c('year', 'month'))
quatroFatores <- merge(quatroFatores, fatorMercado[, .(year, month, MKT)], by = c('year', 'month'))
quatroFatores <- merge(quatroFatores, ibov[, .(year, month, Rc)], by = c('year', 'month'))
quatroFatores <- merge(quatroFatores, indSust, by = c('year', 'month'))

#Pesquisa no google trends

sust <- setDT(gtrends(keyword = 'sustentabilidade', time = 'all', onlyInterest = T)$interest_over_time)
govn <- setDT(gtrends(keyword = 'governança', time = 'all', onlyInterest = T)$interest_over_time)
esg  <- setDT(gtrends(keyword = 'ESG', time = 'all', onlyInterest = T)$interest_over_time)

sust[, year := year(date)]
sust[, month := month(date)]
govn[, year := year(date)]
govn[, month := month(date)]
esg[, year := year(date)]
esg[, month := month(date)]

sust <- sust[, .(year, month, hits)]
govn <- govn[, .(year, month, hits)]
esg  <- esg[, .(year, month, hits)]

gt <- merge(sust,govn, by = c('year', 'month'), suffixes = c('.sust', '.govn'))
gt <- merge(gt, esg, by = c('year', 'month'))
setnames(gt, 'hits', 'hits.esg')
gt <- gt[year >= 2010 & year <= 2019]

gt[, hits.sust := rescale(hits.sust, to = c(20,100))]
gt[, hits.govn := rescale(hits.govn, to = c(20,100))]
gt[, hits.esg := rescale(hits.esg, to = c(20,100))]
gt[, mediaGT := (hits.sust + hits.govn + hits.esg)/3]

gt[, hits.sust := log(hits.sust + 1)]
gt[, hits.govn := log(hits.govn + 1)]
gt[, hits.esg := log(hits.esg + 1)]
gt[, mediaGT := log(mediaGT + 1)]

modelEmp <- merge(quatroFatores, gt, by = c('year', 'month'))

regressao_CAPM_IBOV <- lm(data = modelEmp, formula = (Rc - selic) ~ (MKT - selic))
regressao_CAPM_ISE  <- lm(data = modelEmp, formula = (Rc.ise - selic) ~ (MKT - selic))
regressao_CAPM_ICO2 <- lm(data = modelEmp, formula = (Rc.ico2 - selic) ~ (MKT - selic))

regressao_3FAT_IBOV <- lm(data = tresFatores, formula = (Rc - selic) ~ (MKT - selic) + SMB + HML)
regressao_3FAT_ISE  <- lm(data = tresFatores, formula = (Rc.ise - selic) ~ (MKT - selic) + SMB + HML)
regressao_3FAT_ICO2 <- lm(data = tresFatores, formula = (Rc.ico2 - selic) ~ (MKT - selic) + SMB + HML)

regressao_4FAT_IBOV <- lm(data = quatroFatores, formula = (Rc - selic) ~ (MKT - selic) + SMB + HML + WinMLos)
regressao_4FAT_ISE  <- lm(data = quatroFatores, formula = (Rc.ise - selic) ~ (MKT - selic) + SMB + HML + WinMLos)
regressao_4FAT_ICO2 <- lm(data = quatroFatores, formula = (Rc.ico2 - selic) ~ (MKT - selic) + SMB + HML + WinMLos)

regressao_EMP_IBOV <- lm(data = modelEmp, formula = (Rc - selic) ~ mediaGT + (MKT - selic) + SMB + HML + WinMLos)
regressao_EMP_ISE  <- lm(data = modelEmp, formula = (Rc.ise - selic) ~ mediaGT + (MKT - selic) + SMB + HML + WinMLos)
regressao_EMP_ICO2 <- lm(data = modelEmp, formula = (Rc.ico2 - selic) ~ mediaGT + (MKT - selic) + SMB + HML + WinMLos)










