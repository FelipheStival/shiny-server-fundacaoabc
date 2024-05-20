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
        statment = sprintf("SELECT * FROM estados WHERE nome = '%s'", estado)
        dadosEstado = banco.provider.executeQuery(statment, DB_DATABASE)
        
        if(nrow(dadosEstado) == 0){
          statment = sprintf("INSERT INTO public.estados(nome) VALUES ('%s')", estado)
          dbSendQuery(conn, statment)
        }
        
        # Verificando se existe cidade caso não exista irá inserir
        statment =  sprintf("SELECT * FROM cidades WHERE nome = '%s'", cidade)
        dadosCidade = banco.provider.executeQuery(statment, DB_DATABASE)
        
        if(nrow(dadosCidade) == 0){
          statment = sprintf("INSERT INTO public.cidades(nome) VALUES ('%s')", cidade)
          dbSendQuery(conn, statment)
        }
        
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
        
        # Salvando tabela no banco
        statment = sqlAppendTable(conn, 'clima', dadosInsert)
        dbSendUpdate(conn, statment)
        
      }
      
    },
    error = function(e){ 
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
        statment = sprintf("SELECT * FROM tipos_de_graos WHERE sigla = '%s'", sigla)
        tiposDeGraoDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        if(nrow(tiposDeGraoDate) == 0){
          statment = sprintf("INSERT INTO public.tipos_de_graos(sigla) VALUES ('%s');", sigla)
          dbSendQuery(conn, statment)
        }
        
        # Verificando se existe o genotipo caso nao exita ira inserir
        statment = sprintf("SELECT * FROM genotipos WHERE nome = '%s'", genotipo)
        genotipoDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        if(nrow(genotipoDate) == 0){
          statment = sprintf("INSERT INTO public.estados(nome) VALUES ('%s')", estado)
          dbSendQuery(conn, statment)
        }
        
        # dados insert
        dadosInsert = data.frame(
          valor_fo = linha$valor_fo,
          valor_fs = linha$valor_fs,
          id_genotipo = 1 
        )
        
        # Salvando tabela no banco
        statment = sqlAppendTable(conn, 'genotipos_doencas', dadosInsert)
        dbSendUpdate(conn, statment)
        
      }
      
    },
    error = function(e){ 
      return(FALSE)
    }
  )
  
  
  return(TRUE)
  
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
      
      for(i in 1:nlinhas){
        
        linha = arquivo[i,]
        
        local = linha$local
        cidade = linha$cidade
        estados = linha$estados
        genotipo = linha$genotipo
        tipo_de_grao = linha$tipo_de_grao
        cultura = linha$cultura
        
        #=====================================================================
        # Verificando se existe o estado, caso não exista será inserido
        #=====================================================================
        statment = sprintf("SELECT * FROM estados WHERE nome = '%s'", estados)
        estadoDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        if(nrow(estadoDate) == 0){
          statment = sprintf("INSERT INTO public.estados(nome) VALUES ('%s');", estados)
          dbSendUpdate(conn, statment)
        }
        
        statment = sprintf("SELECT * FROM estados WHERE nome = '%s'", estados)
        estadoDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        #=====================================================================
        # Verificando se existe a cidade
        #=====================================================================
        statment = sprintf("SELECT * FROM cidades WHERE nome = '%s'", cidade)
        cidadeDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        if(nrow(cidadeDate) == 0){
          statment = sprintf("INSERT INTO public.cidades(nome, id_estado) VALUES ('%s',%s);", cidade, estadoDate[1, 'id'])
          dbSendUpdate(conn, statment)
        }
        
        statment = sprintf("SELECT * FROM cidades WHERE nome = '%s'", cidade)
        cidadeDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        #=====================================================================
        # Verificando se existe o local caso não exista será inserido
        #=====================================================================
        statment = sprintf("SELECT * FROM locais WHERE nome = '%s'", local)
        localDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        if(nrow(localDate) == 0){
          statment = sprintf("INSERT INTO public.locais(nome, id_cidade) VALUES ('%s',%s);", local, cidadeDate[1, 'id'])
          dbSendUpdate(conn, statment)
        }
        
        statment = sprintf("SELECT * FROM locais WHERE nome = '%s'", local)
        localDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        #====================================================================
        # Verificando se tipo de grao exista, caso não exista será inserido
        #====================================================================
        statment = sprintf("SELECT * FROM tipos_de_graos WHERE sigla = '%s'", tipo_de_grao)
        tipoDeGraoDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        if(nrow(tipoDeGraoDate) == 0){
          statment = sprintf("INSERT INTO public.tipos_de_graos(sigla) VALUES ('%s');", tipo_de_grao)
          dbSendUpdate(conn, statment)
        }
        
        statment = sprintf("SELECT * FROM tipos_de_graos WHERE sigla = '%s'", tipo_de_grao)
        tipoDeGraoDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        #=====================================================================
        # Verificando se existe genotipo caso não exista será inserido
        #=====================================================================
        statment = sprintf("SELECT * FROM genotipos WHERE nome = '%s'", genotipo)
        genotipoDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        if(nrow(genotipoDate) == 0){
          statment = sprintf("INSERT INTO public.genotipos(nome) VALUES ('%s');", genotipo)
          dbSendUpdate(conn, statment)
        }
        
        statment = sprintf("SELECT * FROM genotipos WHERE nome = '%s'", genotipo)
        genotipoDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        #=====================================================================
        # Verificando se existe a cultura caso não exista será inserido
        #=====================================================================
        statment = sprintf("SELECT * FROM cultura WHERE nome = '%s'", cultura)
        culturaDate = banco.provider.executeQuery(statment, DOENCA_DB_DATABASE)
        
        if(nrow(culturaDate) == 0){
          statment = sprintf("INSERT INTO public.cultura(nome) VALUES ('%s');", cultura)
          dbSendUpdate(conn, statment)
        }
        
        statment = sprintf("SELECT * FROM cultura WHERE nome = '%s'", cultura)
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
        
        # Salvando tabela no banco
        statment = sqlAppendTable(conn, 'ensaios', dadosInsert)
        dbSendUpdate(conn, statment)
        
      }
      
    },
    error = function(e){ 
      return(FALSE)
    }
  )
  
  
  return(TRUE)
  
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
  requiredColumn = c("id_ensaio", "safra", "irrigacao", "fungicida", "repeticao", "produtividade", "numero_de_plantas_na_parcela_util_vegetativo_linha_um",
                     "data_semeadura", "data_emergencia", "data_inicio_floracao", "data_inicio_ponto_colheita", "data_inicio_colheita",
                     "epoca", "local", "cidade", "estados", "genotipo", "tipo_de_grao", "cultura")
  
  indexColumn = which(names(arquivo) %in% requiredColumn)
  
  if(length(indexColumn) == 19){
    return(TRUE)
  }
  
  return(FALSE)
  
}

  