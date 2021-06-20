DiasUteisPreco <- function(anos)
{
    diasUteis <- data.table(ano = anos)
    diasUteis[, dataPreco := offset(as.Date(paste0(anos,'-01-01')), 1, 'Brazil/ANBIMA')]
    du <- diasUteis[, dataPreco]
    du
}