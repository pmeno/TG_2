DefineQtdAcoes <- function(dados = data.table())
{
  auxiliar <- dados[, ultimaAtualizacao := max(DT_REFER), by = c('CNPJ_CIA', 'stock.type', 'CD_CVM')]
  auxiliar <- auxiliar[DT_REFER == ultimaAtualizacao, .(CNPJ_CIA, DENOM_CIA, CD_CVM, stock.type, qtd.issued, ultimaAtualizacao, DT_REFER)]
  result   <- auxiliar[, sum(qtd.issued), by = c('CNPJ_CIA','DENOM_CIA', 'CD_CVM', 'stock.type')]
  setnames(result, 'V1', 'qtd.issued')
  result
}