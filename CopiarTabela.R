.copiarTabela <- function(dt, dec=',', col.names = TRUE, row.names=FALSE)
{
  write.table(dt, "clipboard-16384", sep = '\t', row.names=row.names, col.names=col.names, dec = dec)
}