#================================================================
# Gerenciar Provider
#================================================================
inserirDadosClima = function(novosDados){
  
  tryCatch(
    expr = {
      
      # Abrindo conexao
      conn = banco.provider.openConnection(DB_DATABASE)
      
      # Percorrendo linhas e checando se cidade e estado existe
      nlinhas = nrow(novosDados)
      
      for(i in 1:nlinhas){
        
        linha = novosDados[i,]
        
        estado = linha$estado
        cidade = linha$cidade
        
        # Verificando se existe estado caso não exista irá inserir
        temp = str_to_upper(str_replace_all(estado, ' ', ''))
        statment = sprintf("SELECT * FROM estados WHERE normalize_text(nome) = '%s'", temp)
        dadosEstado = banco.provider.executeQuery(statment, DB_DATABASE)
        
        if(nrow(dadosEstado) == 0){
          statment = sprintf("INSERT INTO estados(nome) VALUES ('%s')", estado)
          dbSendQuery(conn, statment)
        }
        
        statment = sprintf("SELECT * FROM estados WHERE normalize_text(nome) = '%s'", temp)
        dadosEstado = banco.provider.executeQuery(statment, DB_DATABASE)
        
        # Verificando se existe cidade caso não exista irá inserir
        temp = str_to_upper(str_replace_all(cidade, ' ', ''))
        statment =  sprintf("SELECT * FROM cidades WHERE normalize_text(nome) = '%s'", temp)
        dadosCidade = banco.provider.executeQuery(statment, DB_DATABASE)
        
        if(nrow(dadosCidade) == 0){
          statment = sprintf("INSERT INTO cidades(nome) VALUES ('%s')", cidade)
          dbSendQuery(conn, statment)
        }
        
        statment =  sprintf("SELECT * FROM cidades WHERE normalize_text(nome) = '%s'", temp)
        dadosCidade = banco.provider.executeQuery(statment, DB_DATABASE)
        
        # Montando data.frame para ser inserido no banco
        dadosInsert = data.frame(
          data = linha$data,
          tmax = linha$tmax,
          tmin = linha$tmin,
          tmed = linha$tmed,
          urmed = linha$urmed,
          vento = linha$vento,
          vtmax = linha$vtmax,
          rad = linha$rad,
          precip = linha$precip,
          tsolo = linha$tsolo,
          id_cidade = dadosCidade$id
        )
        
        dadosInsert[dadosInsert == 'NULL'] = NA
        
        # Formatando dados
        dadosInsert$data = as.Date(dadosInsert$data, format = "%d/%m/%Y")
        
        # Salvando tabela no banco
        statment = sqlAppendTable(conn, 'clima', dadosInsert)
        dbSendUpdate(conn, statment)
        
      }
      
    },
    error = function(e){ 
      
      shinyalert(
        title = 'Erro',
        text = e$message,
        type = "error"
      )
      
      return(FALSE)
    }
  )
  
  return(TRUE)
  
}

#========================================================
# Método para realizar a persistencia dos dados das doenças
#========================================================
inserirDadosDoencas = function(arquivo){
  
  tryCatch(
    expr = {
      
      # Abrindo conexao
      conn = banco.provider.openConnection(DOENCA_DB_DATABASE)
      
      #selecionando colunas PROD e GENOTIPO
      requiredColumn = c("valor_fo", "valor_fs", "genotipo", "tipo_de_grao")
      indexColumn = which(names(arquivo) %in% requiredColumn)
      
      # Percorrendo linhas e checando se cidade e estado existe
      nlinhas = nrow(arquivo)
      
      for(i in 1:nlinhas){
        
        linha = arquivo[i,]
        
        genotipo = linha$genotipo
        sigla = linha$tipo_de_grao
        
        # Verificando se existe a sigla do genotipo caso nao exista irá inserir
        temp = str_to_upper(str_replace_all(sigla, ' ', ''))
        statment = sprintf("SELECT * FROM tipos_de_graos WHERE normalize_text(sigla) = '%s'", temp)
        tiposDeGraoDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        if(nrow(tiposDeGraoDate) == 0){
          statment = sprintf("INSERT INTO tipos_de_graos(sigla) VALUES ('%s');", sigla)
          dbSendQuery(conn, statment)
        }
        
        statment = sprintf("SELECT * FROM tipos_de_graos WHERE normalize_text(sigla) = '%s'", temp)
        tiposDeGraoDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        # Verificando se existe o genotipo caso nao exita ira inserir
        temp = str_to_upper(str_replace_all(genotipo, ' ', ''))
        statment = sprintf("SELECT * FROM genotipos WHERE normalize_text(nome) = '%s'", temp)
        genotipoDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        if(nrow(genotipoDate) == 0){
          statment = sprintf("INSERT INTO estados(nome) VALUES ('%s')", estado)
          dbSendQuery(conn, statment)
        }
        
        statment = sprintf("SELECT * FROM genotipos WHERE normalize_text(nome) = '%s'", temp)
        genotipoDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        # dados insert
        dadosInsert = data.frame(
          valor_fo = linha$valor_fo,
          valor_fs = linha$valor_fs,
          id_genotipo = 1 
        )
        
        # Salvando tabela no banco
        statment = sqlAppendTable(conn, 'genotipos_doencas', dadosInsert)
        dbSendUpdate(conn, statment)
        
        # Criando objeto retorno
        retorno = data.frame(
          status = TRUE,
          message = 'Dados atualizados com sucesso!'
        )
        
        return(retorno)
        
      }
      
    },
    error = function(e){ 
      
      # Criando objeto retorno
      retorno = data.frame(
        status = FALSE,
        message = e$message
      )
      
      return(retorno)
      
    }
  )
  
  # Criando objeto retorno
  retorno = data.frame(
    status = FALSE,
    message = 'Erro desconhecido!'
  )
  
  return(retorno)
  
}

#========================================================
# Método para realizar a persistencia dos dados das doenças
#========================================================
inserirDadosExperimentos = function(arquivo){
  
  tryCatch(
    expr = {
      
      # Abrindo conexao
      conn = banco.provider.openConnection(DOENCA_DB_DATABASE)
      
      #selecionando colunas PROD e GENOTIPO
      requiredColumn = c("valor_fo", "valor_fs", "genotipo", "tipo_de_grao")
      indexColumn = which(names(arquivo) %in% requiredColumn)
      
      # Percorrendo linhas e checando se cidade e estado existe
      nlinhas = nrow(arquivo)
      
      for(i in 1:nlinhas) {
        
        linha = arquivo[i,]
        
        local = linha$local
        cidade = linha$cidade
        estados = linha$estado
        genotipo = linha$genotipo
        tipo_de_grao = linha$tipo_de_grao
        cultura = linha$cultura
        
        #=====================================================================
        # Verificando se existe o estado, caso não exista será inserido
        #=====================================================================
        temp = str_to_upper(str_replace_all(estados, ' ', ''))
        statment = sprintf("SELECT * FROM estados WHERE normalize_text(nome) = '%s'", temp)
        estadoDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        if(nrow(estadoDate) == 0) {
          statment = sprintf("INSERT INTO estados(nome) VALUES ('%s');", estados)
          dbSendUpdate(conn, statment)
        }
        
        statment = sprintf("SELECT * FROM estados WHERE normalize_text(nome) = '%s'", temp)
        estadoDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        #=====================================================================
        # Verificando se existe a cidade
        #=====================================================================
        temp =  str_to_upper(str_replace_all(cidade, ' ', ''))
        statment = sprintf("SELECT * FROM cidades WHERE normalize_text(nome) = '%s'", temp)
        cidadeDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        if(nrow(cidadeDate) == 0) {
          statment = sprintf("INSERT INTO cidades(nome, id_estado) VALUES ('%s',%s);", cidade, estadoDate[1, 'id'])
          dbSendUpdate(conn, statment)
        }
        
        statment = sprintf("SELECT * FROM cidades WHERE normalize_text(nome) = '%s'", temp)
        cidadeDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        #=====================================================================
        # Verificando se existe o local caso não exista será inserido
        #=====================================================================
        temp = str_to_upper(str_replace_all(local, ' ', ''))
        statment = sprintf("SELECT * FROM locais WHERE normalize_text(nome) = '%s'", local)
        localDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        if(nrow(localDate) == 0){
          statment = sprintf("INSERT INTO locais(nome, id_cidade) VALUES ('%s',%s);", local, cidadeDate[1, 'id'])
          dbSendUpdate(conn, statment)
        }
        
        statment = sprintf("SELECT * FROM locais WHERE normalize_text(nome) = '%s'", temp)
        localDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        #====================================================================
        # Verificando se tipo de grao exista, caso não exista será inserido
        #====================================================================
        temp = str_to_upper(str_replace_all(tipo_de_grao, ' ', ''))
        statment = sprintf("SELECT * FROM tipos_de_graos WHERE normalize_text(sigla) = '%s'", temp)
        tipoDeGraoDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        if(nrow(tipoDeGraoDate) == 0){
          statment = sprintf("INSERT INTO tipos_de_graos(sigla) VALUES ('%s');", tipo_de_grao)
          dbSendUpdate(conn, statment)
        }
        
        statment = sprintf("SELECT * FROM tipos_de_graos WHERE normalize_text(sigla) = '%s'", temp)
        tipoDeGraoDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        #=====================================================================
        # Verificando se existe genotipo caso não exista será inserido
        #=====================================================================
        temp = str_to_upper(str_replace_all(genotipo, ' ', ''))
        statment = sprintf("SELECT * FROM genotipos WHERE normalize_text(nome) = '%s'", genotipo)
        genotipoDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        if(nrow(genotipoDate) == 0){
          statment = sprintf("INSERT INTO genotipos(nome) VALUES ('%s');", genotipo)
          dbSendUpdate(conn, statment)
        }
        
        statment = sprintf("SELECT * FROM genotipos WHERE normalize_text(nome) = '%s'", temp)
        genotipoDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        #=====================================================================
        # Verificando se existe a cultura caso não exista será inserido
        #=====================================================================
        temp = str_to_upper(str_replace_all(cultura, ' ', ''))
        statment = sprintf("SELECT * FROM cultura WHERE normalize_text(nome) = '%s'", temp)
        culturaDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        if(nrow(culturaDate) == 0){
          statment = sprintf("INSERT INTO cultura(nome) VALUES ('%s');", cultura)
          dbSendUpdate(conn, statment)
        }
        
        statment = sprintf("SELECT * FROM cultura WHERE normalize_text(nome) = '%s'", temp)
        culturaDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        #=====================================================================
        # Preparando data.frame para ser inserido no banco
        #=====================================================================
        dadosInsert = data.frame(
          id_ensaio = linha$id_ensaio,
          safra = linha$safra,
          irrigacao = as.character(linha$irrigacao),
          fungicida = as.character(linha$fungicida),
          repeticao = linha$repeticao,
          produtividade = round(as.numeric(linha$produtividade), 2),
          data_semeadura = linha$data_semeadura,
          data_emergencia = linha$data_emergencia,
          data_inicio_floracao = linha$data_inicio_floracao,
          data_inicio_ponto_colheita = linha$data_inicio_ponto_colheita,
          data_inicio_colheita = linha$data_inicio_colheita,
          id_local = localDate[1, 'id'],
          id_genotipo = genotipoDate[1, 'id'],
          id_cultura = culturaDate[1, 'id']
        )
        
        # Tratando fugincidade e repeticao
        dadosInsert$fungicida = str_to_upper(dadosInsert$fungicida)
        dadosInsert$irrigacao = str_to_upper(dadosInsert$repeticao)
        
        dadosInsert$fungicida = ifelse(dadosInsert$fungicida == 'SIM', 'TRUE', 'FALSE')
        dadosInsert$irrigacao = ifelse(dadosInsert$irrigacao == 'NAO', 'TRUE', 'FALSE')
        
        # Formatando datas
        dadosInsert$data_semeadura = as.Date(dadosInsert$data_semeadura, format = "%d/%m/%Y")
        dadosInsert$data_emergencia = as.Date(dadosInsert$data_emergencia, format = "%d/%m/%Y")
        dadosInsert$data_inicio_floracao = as.Date(dadosInsert$data_inicio_floracao, format = "%d/%m/%Y")
        dadosInsert$data_inicio_ponto_colheita = as.Date(dadosInsert$data_inicio_ponto_colheita, format = "%d/%m/%Y")
        dadosInsert$data_inicio_colheita = as.Date(dadosInsert$data_inicio_colheita, format = "%d/%m/%Y")
        
        # Salvando tabela no banco
        statment = sqlAppendTable(conn, 'ensaios', dadosInsert)
        dbSendUpdate(conn, statment)
        
        # Criando objeto retorno
        retorno = data.frame(
          status = TRUE,
          message = 'Dados atualizados com sucesso!'
        )
        
        return(retorno)
        
      }
      
    },
    error = function(e) { 
      
      # Criando objeto retorno
      retorno = data.frame(
        status = FALSE,
        message = e$message
      )
      
      return(retorno)
      
    }
  )
  
  
  # Criando objeto retorno
  retorno = data.frame(
    status = FALSE,
    message = 'Erro desconhecido'
  )
  
  return(retorno)
  
}

#========================================================
# Método para verificar as colunas do arquivo para
# a atualizacao da base de dados do clima
#========================================================
verificarColunasClima = function(arquivo){
  
  # Selecionando colunas PROD e GENOTIPO
  requiredColumn = c("data","tmax", "tmin", "tmed", "urmed", "vento", "vtmax", "rad", "precip", "tsolo", "cidade", "estado")
  indexColumn = which(names(arquivo) %in% requiredColumn)
  
  if(length(indexColumn) == 12){
    return(TRUE)
  }
  
  return(FALSE)
  
}

#========================================================
# Método para verificar as colunas do arquivo 
# para atualização da base de dados das doenças
#========================================================
verificarColunaDoencas = function(arquivo){
  
  # Selecionando colunas PROD e GENOTIPO
  requiredColumn = c("valor_fo", "valor_fs", "genotipo", "tipo_de_grao")
  indexColumn = which(names(arquivo) %in% requiredColumn)
  
  if(length(indexColumn) == 4){
    return(TRUE)
  }
  
  return(FALSE)
  
}
#========================================================
# Método para verificar as colunas do arquivo 
# para atualização da base de dados dos ensaios
#========================================================
verificarColunasExperimentos = function(arquivo){
  
  #selecionando colunas PROD e GENOTIPO
  requiredColumn = c("id_ensaio", "safra", "irrigacao", "fungicida", "repeticao", "produtividade",
                     "data_semeadura", "data_emergencia", "data_inicio_floracao", "data_inicio_ponto_colheita",
                     "data_inicio_colheita", "local", "cidade", "estado", 
                     "genotipo", "tipo_de_grao", "cultura")
  
  indexColumn = setdiff(requiredColumn, names(arquivo))
  
  if(length(indexColumn) == 0){
    return(TRUE)
  }
  
  return(FALSE)
  
}

  