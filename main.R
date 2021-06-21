#### Libs utilizadas ######

require(data.table)
require(BatchGetSymbols)
require(GetDFPData2)
require(GetFREData)
require(bizdays)
require(stringr)

### CAMINHOS NECESSÁRIOS #####

caminhoPrincipal <- "C:/Users/pedro/Desktop/TCC/PARTE_2/codes/TG_2/"
caminhoBases     <- "C:/Users/pedro/Desktop/TCC/PARTE_2/codes/TG_2/bases/"
caminhoTickers   <- paste0(caminhoBases, "tickerBolsa.csv")
caminhoQtdAcoes  <- paste0(caminhoBases, "baseQtdAcoes.csv")

### Sources #####

source(paste0(caminhoPrincipal, 'ChecarPrecoAcao.R'))
source(paste0(caminhoPrincipal, 'DefineQtdAcoes.R'))
source(paste0(caminhoPrincipal, 'DiasUteisPreco.R'))
source(paste0(caminhoPrincipal, 'CalcularValorMercado.R'))
source(paste0(caminhoPrincipal, 'RetornaValorContabil.R'))


### Declaracoes de variaveis ####

precosFinal_ON <- data.table()
precosFinal_PN <- data.table()

### CARREGAR TODAS AS EMPRESAS DA BOLSA DE 2010 ATÉ HOJE ########

empresasBolsa <- get_info_companies()
tickersBolsa  <- read.csv(caminhoTickers, sep = ';', encoding = 'UTF-8', header = T, na.strings = "", stringsAsFactors = FALSE, colClasses = c('character','character','character','character','character'))
acoesQtdBolsa <- read.csv(caminhoQtdAcoes, sep = ';', encoding = 'UTF-8', header = T, na.strings = "NA", stringsAsFactors = FALSE)

setDT(empresasBolsa)
setDT(tickersBolsa)
setDT(acoesQtdBolsa)

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








