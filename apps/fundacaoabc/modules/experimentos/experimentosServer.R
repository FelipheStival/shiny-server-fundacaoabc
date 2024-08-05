# Doenca service
experimentoServer = function(input, output, session) {
  
  # Botao retornar
  observeEvent(input$btnRetonarExperimentos,
               change_page('/')
  )
  
  # dados doenca input service
  dadosEnsaios = reactive({
    dados = experimentos.provider.dados()
    return(dados)
  })
  
  # Dados filtrados
  dadosFiltrados = reactive({
    dados = experimentos.provider.dadosFiltrados(dadosEnsaios(), input)
    return(dados)
  })
  
  # dados analise estatistica
  dadosEstatistica = reactive({
    y = service.getY(dadosFiltrados())
    return(y)
  })
  
  # dados analise estatistica media
  dadosEstatisticaMedia = reactive({
    y = service.getMean(dadosFiltrados(), input)
    return(y)
  })
  
  # dados para gerar o relatorio
  dadosRelatorio = reactive({
    y = service.getY(dadosFiltrados())
    y = model.dadosRelatorio(y)
    return(y)
  })
  
  # Dados experimentos
  dadosExperimentos = reactive({
    
    mediaSelect = input$select_analiseEstatistica_media
    experimentosSelect = input$select_analiseEstatistica_experimentos
    
    y = calcula_predict(dadosFiltrados(), "produtividade", "repeticao", "local","genotipo", "safra", mediaSelect, experimentosSelect)
    return(y$pred)
    
  })
  
  # Dados gráficos gge
  dadosGGE = reactive({
    
    indexTodos = which(input$inputGenotiposGGE %in% 'Todos')
    dadosFiltrados = dadosFiltrados()
    
    if(length(indexTodos) == 0 && length(input$inputGenotiposGGE) > 0){
      dadosFiltrados = dadosFiltrados[dadosFiltrados$genotipo %in% input$inputGenotiposGGE, ]
    }
    
    return(dadosFiltrados)
    
  })
  
  # Evento para desabilitar o input
  observe({
    
    if(!is.null(input$tabGraficosExperimentos) && !is.null(input$subTabGraficosExperimentos)){
      
      if(input$tabGraficosExperimentos == 'Gráfico media local'){
        
        shinyjs::enable('select_analiseEstatistica_local')
        shinyjs::disable('select_analiseEstatistica_media')
        
      } else {
        
        shinyjs::disable('select_analiseEstatistica_local')
        shinyjs::enable('select_analiseEstatistica_media')
        
        if(input$subTabGraficosExperimentos == 'Gráfico cluster'){
          shinyjs::disable('select_analiseEstatistica_media')
        } else if(input$subTabGraficosExperimentos == 'Gráfico média harmônica') {
          shinyjs::disable('select_analiseEstatistica_media')
          shinyjs::disable('select_analiseEstatistica_local')
        }
      
        
      }
      
      # Atualiza o conteúdo da tab
      updateTabsetPanel(session, "tabs", selected = input$tabGraficosExperimentos)
      
    }
    
  })
  
  # Atualizando input cultura
  observe({
    culturas = experimentos.provider.unique(dadosEnsaios(), 'cultura')
    updateSelectInput(
      session = session,
      inputId = "culturaInputDoencas",
      choices = culturas,
      selected = culturas[1]
    )
  })
  
  # Atualizando input ensaios
  observeEvent(input$culturaInputDoencas, {
    
    ensaios = experimentos.provider.unique(dadosFiltrados(), 'id_ensaio')
    updateSelectInput(
      session = session,
      inputId = "ensaiosInputDoencas",
      choices = c('Todos', ensaios),
      selected = 'Todos'
    )
    
  })
  
  # Atualizando input safras
  observeEvent(c(input$culturaInputDoencas, input$ensaiosInputDoencas), {
    
    safras = sort(experimentos.provider.unique(dadosFiltrados(), 'safra'))
    
    updateSelectInput(
      session = session,
      inputId = "safraInputDoencas",
      choices = c('Todos', safras),
      selected = 'Todos'
    ) 
    
  })
  
  # Atualizando input estado
  observeEvent(c(input$culturaInputDoencas, input$ensaiosInputDoencas, input$safraInputDoencas), {
    
    estados = experimentos.provider.unique(dadosFiltrados(), 'estado')
    
    updateSelectInput(
      session = session,
      inputId = "estadoInputDoencas",
      choices = c("Todos", estados),
      selected = "Todos"
    ) 
    
  })
  
  # Atualizando input cidades
  observeEvent(c(input$culturaInputDoencas, input$ensaiosInputDoencas, input$safraInputDoencas, input$estadoInputDoencas), {
    
    cidades = experimentos.provider.unique(dadosFiltrados(), 'cidade')
    
    updateSelectInput(
      session = session,
      inputId = "cidadeInputDoencas",
      choices = c("Todos", cidades),
      selected = "Todos"
    ) 
    
  })
  
  # Atualizando input irrigacao
  observeEvent(c(input$culturaInputDoencas, input$ensaiosInputDoencas, input$safraInputDoencas, input$estadoInputDoencas, input$cidadeInputDoencas), {
    
    irrigacao = experimentos.provider.unique(dadosFiltrados(), 'irrigacao')
    
    updateSelectInput(
      session = session,
      inputId = "irrigacaoInputDoencas",
      choices = c("Todos", irrigacao),
      selected = "Todos"
    ) 
    
  })
  
  # Atualizando input fungicida
  observeEvent(c(input$culturaInputDoencas, input$ensaiosInputDoencas, input$safraInputDoencas, input$estadoInputDoencas, input$cidadeInputDoencas, input$irrigacaoInputDoencas), {
    
    fungicida = experimentos.provider.unique(dadosFiltrados(), 'fungicida')
    
    updateSelectInput(
      session = session,
      inputId = "fungicidaInputDoencas",
      choices = c("Todos", fungicida),
      selected = "Todos"
    )
    
  })
  
  # Atualizando input tipo de grao
  observeEvent(c(input$culturaInputDoencas, input$ensaiosInputDoencas, input$safraInputDoencas, input$estadoInputDoencas, input$cidadeInputDoencas, input$irrigacaoInputDoencas, input$fungicidaInputDoencas), {
    
    tipoGraos = experimentos.provider.unique(dadosFiltrados(), 'tipo_de_grao')
    
    updateSelectInput(
      session = session,
      inputId = "tipodegraoInputDoencas",
      choices = c("Todos", tipoGraos),
      selected = "Todos"
    ) 
    
    if(all(tipoGraos == 'Não possui')) {
      shinyjs::disable('tipodegraoInputDoencas')
    } else {
      shinyjs::enable('tipodegraoInputDoencas')
    }
    
  })
  
  # Atualizando input grupo de maturação
  observeEvent(c(input$culturaInputDoencas, input$ensaiosInputDoencas, input$safraInputDoencas, input$estadoInputDoencas, input$cidadeInputDoencas, input$irrigacaoInputDoencas, input$fungicidaInputDoencas, input$tipodegraoInputDoencas), {
    
    grupoMaturacao = experimentos.provider.unique(dadosFiltrados(), 'grupo_maturacao')
    grupoMaturacao = grupoMaturacao[!is.na(grupoMaturacao)]
    
    updateSelectInput(
      session = session,
      inputId = "grupoMaturacaoInputDoencas",
      choices = c("Todos", grupoMaturacao),
      selected = "Todos"
    ) 
    
    if(length(grupoMaturacao) == 0) {
      shinyjs::disable('grupoMaturacaoInputDoencas')
    } else {
      shinyjs::enable('grupoMaturacaoInputDoencas')
    }
    
  })
  
  # Atualizando input de epoca
  observeEvent(c(input$culturaInputDoencas, input$ensaiosInputDoencas, input$safraInputDoencas, input$estadoInputDoencas, input$cidadeInputDoencas, input$irrigacaoInputDoencas, input$fungicidaInputDoencas, input$tipodegraoInputDoencas), {
    
    epoca = experimentos.provider.unique(dadosFiltrados(), 'epoca')
    
    updateSelectInput(
      session = session,
      inputId = "epocaInputDoencas",
      choices = c("Todos", epoca),
      selected = "Todos"
    )
    
    if(all(is.na(epoca))) {
      shinyjs::disable('epocaInputDoencas')
    } else {
      shinyjs::enable('epocaInputDoencas')
    }
    
  })
  
  # Atualizando input de local analise estatistica
  observe({
    
    locais = experimentos.provider.unique(dadosFiltrados(), 'local')
    updateSelectInput(
      session = session,
      inputId = "select_analiseEstatistica_local",
      choices = locais,
      selected = locais[1]
    )
    
  })
  
  # Atualiando input de experimentos Analise estatistica
  observe({
    
    # Atualizando input de experimentos
    if(!is.null(input$select_analiseEstatistica_local) && input$tabGraficosExperimentos == 'Gráfico media local') {
      
      experimentos = dadosFiltrados()
      experimentos = unique(experimentos[experimentos$local %in% input$select_analiseEstatistica_local, 'id_ensaio'])
      
    } else {
      
      experimentos = experimentos.provider.unique(dadosFiltrados(), 'id_ensaio')
      
    }
    
    updateSelectInput(
      session = session,
      inputId = "select_analiseEstatistica_experimentos",
      choices = c('Todos', experimentos),
      selected = 'Todos'
    )
    
  })
  
  
  # Atualizando input local Potencial genótipo produtivo
  observe({
    
    locais = experimentos.provider.unique(dadosFiltrados(), 'local')
    updateSelectInput(
      session = session,
      inputId = "select_analiseEstatistica_local_potencial_genotipo",
      choices = locais
    )
    
  })
  
  # Atualizando lista genotipos grafico linhas e GGE
  observe({
    
    genotipos = experimentos.provider.unique(dadosFiltrados(), 'genotipo')
    
    updateSelectInput(
      session = session,
      inputId = "GenotipoSelectDoencas",
      choices = genotipos,
      selected = genotipos[1]
    )
    
    updateSelectInput(
      session = session,
      inputId = 'inputGenotiposGGE',
      choices = c('Todos', genotipos),
      selected = 'Todos'
    )
    
  })
  
  #==============================================#
  output$tabelaExperimentosDownload = renderDataTable({
    
    dados = dadosFiltrados()
    
    names(dados) = c('id', 'ID ensaio', 'Estado', 'Cidade', 'Local', 'Tipo de grão', 'Genotipo',
                     'Safra', 'Repetição', 'Produtividade', 'Grupo maturação', 'Data de semeadura',
                     'Data de emergência', 'Data início floração', 'Data início ponto colheita',
                     'Data início colheita', 'Cultura', 'Irrigação', 'Fungicida', 'Cultura', 'Epoca')
    
    
    return(dados)
    
  }, options = list(lengthMenu = c(5,10, 25), pageLength = 10, scrollX = TRUE))
  
  #==============================================#
  
  #==============================================#
  #====== Baixar dados experimentos =============#
  output$downloadDadosExperimentos = downloadHandler(
    
    filename = function() {
      paste(
        'dados',
        "-",
        as.character(Sys.time()),
        '.csv',
        sep = ''
      )
    },
    content = function(con) {
      
      dados = dadosFiltrados()
      
      names(dados) = c('id', 'ID ensaio', 'Estado', 'Cidade', 'Local', 'Tipo de grão', 'Genotipo',
                       'Safra', 'Repetição', 'Produtividade', 'Grupo maturação', 'Data de semeadura',
                       'Data de emergência', 'Data início floração', 'Data início ponto colheita',
                       'Data início colheita', 'Cultura', 'Irrigação', 'Fungicida', 'Cultura', 'Epoca')
      
      write.csv(dados, con)
      
    }
  )
  #=============================================#
  

  
  #==============================================#
  #====== Resultado diagnotico ==================#
  output$tabela_diagnostico_Exibir = renderDataTable({
    
    diagnostico = service.getDiagostico(dadosFiltrados(), input)
    
    validate(
      need(!is.null(diagnostico), "Nao ha dados suficientes para exibicao da tabela.")
    )
    
    return(diagnostico)
    
  },options = list(lengthMenu = c(5,10, 25), pageLength = 5, scrollX = TRUE))
  #==============================================#
  
  #==============================================#
  output$botao_diagnostico_Download = downloadHandler(
    
    filename = function() {
      paste('diagnostico_', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      
      diagnostico = service.getDiagostico(dadosFiltrados())
      write.csv(diagnostico, con)
      
    }
  )
  
  output$grafico_dadosPerdidos_Estatistica = renderPlot({
    
    #====================================#
    # Validacao
    
    validate.ids_data = length(unique((dadosFiltrados()$id_ensaio)))
    
    validate(
      need(validate.ids_data > 0,
           "Nao ha dados suficientes para exibicao do grafico.")
    )
    
    #====================================#
    graphics.dadosPerdidos_Estatistica(dadosFiltrados())
    
  })
  
  
  #==============================================#
  # Grafico "Resumo"
  output$grafico_analiseEstatistica_Resumo = renderPlotly({
    
    #====================================#
    # Validacao
    validate(
      need(!is.null(dadosFiltrados()), "Nao ha dados suficientes para exibicao do grafico."),
      need(length(unique(dadosFiltrados()$rep)) > 1, "Nao ha repetições suficientes para exibicao do grafico.")
    )
    
    #====================================#
    mediaSelect = input$select_analiseEstatistica_media
    grafico.analiseEstatistica_Resumo(dadosExperimentos(), mediaSelect)
    
  })
  #==============================================#
  
  #==============================================#
  # Grafico "Unitario"
  output$grafico_analiseEstatistica_Unitario = renderPlotly({
    #====================================#
    # Validacao
    
    validate(
      need(!is.null(dadosEstatistica()),
           "Nao ha dados suficientes para exibicao do grafico."),
      need(length(unique(dadosFiltrados()$rep)) > 1, "Nao ha repetições suficientes para exibicao do grafico.")
    )
    
    #====================================#
    dataPlot = calcula_predict(dadosFiltrados(), "produtividade", "repeticao", "local","genotipo", "safra", NULL, input$select_analiseEstatistica_experimentos)
    grafico.analiseEstatistica_Unitario(dataPlot$pred, input$select_analiseEstatistica_local)
    
  })
  #==============================================#
  
  #==============================================#
  # Grafico "Heatmap"
  output$grafico_analiseEstatistica_Heatmap = renderPlotly({
    
    #====================================#
    # Validacao
    validate.ids_data = length(unique((dadosFiltrados()$id_ensaio)))
    
    validate(
      need(validate.ids_data > 1, "Nao ha dados suficientes para exibicao do grafico."),
      need(length(unique(dadosFiltrados()$rep)) > 1, "Nao ha repetições suficientes para exibicao do grafico.")
    )
    #====================================#
    
    dataPlot = calcula_predict(dadosFiltrados(), "produtividade", "repeticao", "local","genotipo", "safra")
    grafico.analiseEstatistica_Heatmap(dataPlot$pred)
    
  })
  #==============================================#
  
  #==============================================#
  # Grafico "Analise cluster"
  output$grafico_geral_cluster = renderPlotly({
    
    #====================================#
    # Validacao
    validate.ids_data = length(unique((dadosFiltrados()$id_ensaio)))
    
    validate(
      need(validate.ids_data > 1, "Nao ha dados suficientes para exibicao do grafico."),
      need(length(unique(dadosFiltrados()$rep)) > 1, "Nao ha repetições suficientes para exibicao do grafico.")
    )
    #====================================#
    grafico.analiseCluster(dadosExperimentos())
    
  })
  
  #==============================================#
  # Grafico "Media harmonica"
  output$grafico_media_harmonica = renderPlotly({
    
    #====================================#
    # Validacao
    validate.ids_data = length(unique((dadosFiltrados()$id_ensaio)))
    
    validate(
      need(validate.ids_data > 1, "Nao ha dados suficientes para exibicao do grafico.")
    )
    #====================================#
    grafico.mediaHarmonica(dadosFiltrados(), input$select_analiseEstatistica_experimentos)
    
  })
  
  #==============================================#
  # Grafico "linhas"
  output$graficolinha = renderPlot({
    
    #====================================#
    # Validacao
    validate.ids_data = length(unique((dadosFiltrados()$id_ensaio)))
    
    validate(
      need(validate.ids_data > 1,
           "Nao ha dados suficientes para exibicao do grafico.")
    )
    
    grafico.GraficoLinhas(dadosEstatisticaMedia())
    
  })
  
  #==============================================#
  # Grafico "Ambiental relatvo"
  output$graficoAmbientalRelativo = renderPlot({
    
    #====================================#
    # Validacao
    validate.ids_data = length(unique((dadosFiltrados()$id_ensaio)))
    
    validate(
      need(validate.ids_data > 1, "Nao ha dados suficientes para exibicao do grafico."),
      need(length(input$GenotipoSelectDoencas) > 1, 'Selecione mais de um genotipo')
    )
    
    grafico.ambientalRelativo(dadosFiltrados(), input$GenotipoSelectDoencas)
    
  })
  
  #==============================================#
  # Grafico "Ambiental relatvo"
  output$graficoAmbientalLocal = renderPlot({
    
    #====================================#
    # Validacao
    validate.ids_data = length(unique((dadosFiltrados()$id_ensaio)))
    
    validate(
      need(validate.ids_data > 1, "Nao ha dados suficientes para exibicao do grafico."),
      need(length(input$GenotipoSelectDoencas) > 1, 'Selecione mais de um genotipo')
    )
    
    grafico.graficoAmbientalLocal(dadosFiltrados(), input$GenotipoSelectDoencas)
    
  })
  
  #==============================================#
  # Grafico "Quem vence e aonde"
  output$grafico_analiseGGE_QuemVenceEAonde = renderPlot({
    
    gge = model.GGE(dadosGGE()) 
    
    #====================================#
    # Validacao
    validate.ids_data = length(unique((dadosGGE()$id_ensaio)))
    
    validate(
      need(validate.ids_data > 1 & !is.null(gge),
           "Nao ha dados suficientes para exibicao do grafico.")
    )
    #====================================#
    grafico.analiseGGE_QuemVenceEAonde(gge)
    
  })
  #==============================================#
  
  #==============================================#
  # Grafico "Ordem de Ambiente"
  output$grafico_analiseGGE_OrdemDeAmbiente = renderPlot({
    
    gge = model.GGE(dadosGGE())
    
    #====================================#
    # Validacao
    
    validate.ids_data = length(unique((dadosGGE()$id_ensaio)))
    
    validate(
      need(validate.ids_data > 1 & !is.null(gge),
           "Nao ha dados suficientes para exibicao do grafico.")
    )
    #====================================#
    
    
    grafico.analiseGGE_OrdemDeAmbiente(gge)
    
  })
  #==============================================#
  
  #==============================================#
  # Grafico "Ordem de genotipo"
  output$grafico_analiseGGE_OrdemDeGenotipo = renderPlot({
    
    gge = model.GGE(dadosGGE())
    
    #====================================#
    # Validacao
    
    validate.ids_data = length(unique((dadosGGE()$id_ensaio)))
    
    validate(
      need(validate.ids_data > 1 & !is.null(gge),
           "Nao ha dados suficientes para exibicao do grafico.")
    )
    #====================================#
    
    grafico.analiseGGE_OrdemDeGenotipo(gge)
    
  })
  #==============================================#
  
  #==============================================#
  # Grafico "Relacao entre ambientes"
  output$grafico_analiseGGE_RelacaoEntreAmbientes = renderPlot({
    
    gge = model.GGE(dadosGGE())
    
    #====================================#
    # Validacao
    
    validate.ids_data = length(unique((dadosGGE()$id_ensaio)))
    
    validate(
      need(validate.ids_data > 1 & !is.null(gge),
           "Nao ha dados suficientes para exibicao do grafico.")
    )
    #====================================#
    
    grafico.analiseGGE_RelacaoEntreAmbientes(gge)
    
  })
  #==============================================#
  
  #==============================================#
  # Grafico "Estabilidade / Media"
  output$grafico_analiseGGE_EstabilidadeMedia = renderPlot({
    
    gge = model.GGE(dadosGGE())
    
    #====================================#
    # Validacao
    
    validate.ids_data = length(unique((dadosGGE()$id_ensaio)))
    
    validate(
      need(validate.ids_data > 1 & !is.null(gge),
           "Nao ha dados suficientes para exibicao do grafico.")
    )
    #====================================#
    
    grafico.analiseGGE_EstabilidadeMedia(gge)
  })
  #==============================================#
  
  #==============================================#
  # Grafico "Denograma"
  output$grafico_analiseGGE_Denograma = renderPlot({
    
    deno = model.deno(dadosGGE())
    
    #====================================#
    # Validacao
    validate.ids_data = length(unique((dadosGGE()$id_ensaio)))
    
    validate(
      need(validate.ids_data > 1 & !is.null(deno),
           "Nao ha dados suficientes para exibicao do grafico.")
    )
    #====================================#
    
    grafico.analiseGGE_Denograma(deno)
  })
  #==============================================#
  
  
  #==============================================#
  # Grafico "Potencial Genotipo"
  output$potencialGenotipoPlot = renderPlot({
    
    localSelecionado = input$select_analiseEstatistica_local_potencial_genotipo
    
    dadosPlot = dadosFiltrados();
    dadosPlot = dadosPlot[!is.na(dadosPlot$produtividade),]
    
    #====================================#
    # Validacao
    validate.ids_data = nrow(dadosPlot)
    
    validate(
      need(validate.ids_data > 0,
           "Nao ha dados suficientes para exibicao do grafico.")
    )
    #====================================#
    
    dadosPlot = gen_prod_pot(dadosPlot)
    grafico.pontecialProdutivo(dadosPlot, localSelecionado)
    
  })
  #==============================================#
  
  #==============================================#
  # Download relatorio"
  output$downloadRelatorio = downloadHandler(
    
    filename = function() {
      
      nomeArquivo = paste('relatório', format(Sys.time(), "%d/%m/%Y %H:%M:%S"))
      nomeArquivo = str_replace_all(nomeArquivo, ' ', '_')
      
      paste(nomeArquivo, sep = ".", switch(
        input$inputRelatorioFormato, PDF = "pdf", HTML = "html", CSV = "csv"
      ))
      
    },
    
    content = function(file) {
      
      if(input$inputRelatorioFormato == 'HTML') {
        
        src = normalizePath("report.Rmd")
        owd = setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, "report.Rmd", overwrite = TRUE)
        
        out = render("report.Rmd", switch(
          input$inputRelatorioFormato,
          PDF = pdf_document(), HTML = html_document(), Word = word_document()
        ))
        
        file.rename(out, file)
        
      } else if(input$inputRelatorioFormato == 'CSV') {
        
        if(input$tabGraficosExperimentos == 'Gráfico geral') {
          
          dados = dadosExperimentos()
          names(dados) = c('Genotipo', 'Local', 'Safra', 'Predição')
          
        } else if(input$tabGraficosExperimentos == 'Gráfico media local') {
          dados = dadosFiltrados()
        }
      
        write.csv(dados, file)
        
      }
      
    }
  )
  
  #==============================================#
  # Info box com informações do numero de genotipos
  output$numeroExperimentosInfo = renderInfoBox({
    
    ensaios = dadosFiltrados()
    indexTodos = which(input$select_analiseEstatistica_experimentos %in% 'Todos')
    
    if(length(indexTodos) == 0 && !is.null(input$select_analiseEstatistica_experimentos)) {
      
      numeroExperimentos = length(input$select_analiseEstatistica_experimentos)
      
    } else {
      
      if(!is.null(input$select_analiseEstatistica_local) && input$tabGraficosExperimentos == 'Gráfico media local') {
        numeroExperimentos = length(unique(ensaios[ensaios$local %in% input$select_analiseEstatistica_local, 'id_ensaio']))
      } else {
        numeroExperimentos = length(unique(ensaios$id_ensaio))
      }
      
    }
     
    infoBox(
      width = 12,
      title = "Número de experimentos",
      value = numeroExperimentos,
      icon = icon('hashtag')
    )
    
  })
  
  #==============================================#
  
  
}