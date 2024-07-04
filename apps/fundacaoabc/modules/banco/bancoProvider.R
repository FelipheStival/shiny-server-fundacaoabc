# Metodo para abrir a conexao com o banco de dados
banco.provider.openConnection = function(dbname) {
  
  # criando conexao
  if(dbname == DB_DATABASE){
    
    # configurando driver
    driver = dbDriver("SQLite")
    
    conn = dbConnect(driver, "sqlite//clima.sqlite")

    return(conn)
    
    
  } else {
    
    # configurando driver
    driver = dbDriver("SQLite")
    
    conn = dbConnect(driver, "sqlite//ensaios.sqlite")
    
    return(conn)
    
  }
  
  return(conn)
  
}

# metodo para executar a query
banco.provider.executeQuery = function(statement, dbname = 'meteoro') {
  connection = banco.provider.openConnection(dbname)
  result = dbGetQuery(connection, statement)
  result = set_utf8(result)
  dbDisconnect(connection)
  return(result)
}

# Metodo para transformar os caracteres em utf-8
set_utf8 = function(x) {
  # Declare UTF-8 encoding on all character columns:
  chr <- sapply(x, is.character)
  x[, chr] <- lapply(x[, chr, drop = FALSE], `Encoding<-`, "UTF-8")
  # Same on column names:
  Encoding(names(x)) <- "UTF-8"
  x
}