#### Libs utilizadas ######

require(data.table)
require(BatchGetSymbols)
require(GetDFPData2)
require(GetFREData)
require(bizdays)

### CAMINHOS NECESSÁRIOS #####

caminhoPrincipal <- "C:/Users/pedro/Desktop/TCC/PARTE_2/codes/TG_2/"
caminhoBases     <- "C:/Users/pedro/Desktop/TCC/PARTE_2/codes/TG_2/bases/"
caminhoTickers   <- paste0(caminhoBases, "tickerBolsa.csv")
caminhoQtdAcoes  <- paste0(caminhoBases, "baseQtdAcoes.csv")

### Sources #####

source(paste0(caminhoPrincipal, 'ChecarPrecoAcao.R'))


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

acoesQtdBolsa[, 'DT_REFER'] <- acoesQtdBolsa[, as.Date(DT_REFER, origin = "1899-12-30")]

# Filtrando apenas empresas que queremos calcular os parâmetros.

empresasBolsa <- empresasBolsa[is.na(DT_CANCEL) & (SIT_REG == 'ATIVO') & (CATEG_REG == 'Categoria A') & (is.na(TP_MERC) | TP_MERC != 'BALCÃO NÃO ORGANIZADO')]
empresasBolsa <- empresasBolsa[SIT_EMISSOR %chin% c("FASE OPERACIONAL", "FASE PRÉ-OPERACIONAL")]
empresasBolsa <- merge(empresasBolsa, tickersBolsa, by = c('CNPJ', 'DENOM_SOCIAL'), all.x = TRUE)


valorMercado <- dcast(acoesQtdBolsa, CNPJ_CIA + DT_REFER + CD_CVM ~ stock.type, value.var = 'qtd.issued', fun.aggregate = sum)
valorMercado[, 'NA' := NULL]

valorMercado <- merge(valorMercado, empresasBolsa[, .(DENOM_SOCIAL, CD_CVM, DT_REG, Ticker_ON, Ticker_PN, Ticker_UN)], by = 'CD_CVM', all.y = T)

# Não queremos empresas registradas em 2020 para frente, data de referencia de dados de ON e PN maior que 2020 e zerados
valorMercado <- valorMercado[DT_REG < '2020-01-01']
valorMercado <- valorMercado[DT_REFER <= '2019-01-01']
valorMercado <- valorMercado[(ON != 0) && (PN != 0)]

#Criando data que os preços serão procurados e string de tickers
valorMercado[, dataPreco := offset(as.Date(paste0(year(DT_REFER),'-01-01')), 120, 'Brazil/ANBIMA')]
valorMercado[, Ticker_ON_YAHOO := paste0(Ticker_ON, '.SA')]
valorMercado[, Ticker_PN_YAHOO := paste0(Ticker_PN, '.SA')]
dataPrecos <- valorMercado[, unique(dataPreco)]

for(data in dataPrecos)
{
  data <- as.Date(data)
  precoInicial   <- data.table()
  precoInicial   <- ChecarPrecoAcao(valorMercado, data, 'ON') 
  precosFinal_ON <- rbind(precosFinal_ON, precoInicial)
  
}

for(data in dataPrecos)
{
  data <- as.Date(data)
  precoInicial   <- data.table()
  precoInicial   <- ChecarPrecoAcao(valorMercado, data, 'PN') 
  precosFinal_PN <- rbind(precosFinal_PN, precoInicial)
  
}

valorMercado <- merge(valorMercado, precosFinal_ON, by = c('dataPreco', 'Ticker_ON_YAHOO'), all.x = T)
valorMercado <- merge(valorMercado, precosFinal_PN, by = c('dataPreco', 'Ticker_PN_YAHOO'), all.x = T)
