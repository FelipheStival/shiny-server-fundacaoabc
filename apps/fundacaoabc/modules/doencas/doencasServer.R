# Doenca service
doencaServer = function(input, output, session) {
  
  
  # Reactive para conseguir os dados dos municipios
  dadosDoencas = reactive({
    
    dados = getDadosDoencasProvider(input)
    return(dados)
    
  })
  
  # Botao Voltar
  observeEvent(input$btnRetonarDoencas,
               change_page('/')
  )
  
  # Evento para desabilitar o input
  observeEvent(input$doencasTabGraficos, {
    
    # Observa qual a tab selecionada para desativar o input
    if(input$doencasTabGraficos == 'Gráfico local'){
      shinyjs::enable('select_doencas_local')
    } else {
      shinyjs::disable('select_doencas_local')
    }
    
    # Atualiza o conteúdo da tab
    updateTabsetPanel(session, "tabs", selected = input$doencasTabGraficos)
    
  })
  
  # Atualizando safras
  observe({
    
    dados = dadosDoencas()
    dados = dados[[1]]
    
    safras = sort(unique(dados$safra))
    updateSelectInput(
      session = session,
      inputId = "safraInputDoencas2",
      choices = safras,
      selected = safras[1]
    ) 
    
  })
  
  # Atualizando locais
  observe({
    
    if(!is.null(input$safraInputDoencas2)){
      
      dados = dadosDoencas()
      dados = dados[[1]]
      dados = dados[dados$safra %in% input$safraInputDoencas2,]
      
      if(nrow(dados) > 0){
        updateSelectInput(
          session = session,
          inputId = "select_doencas_local",
          choices = unique(dados$cidade),
          selected = unique(dados$cidade)[1]
        ) 
      }
    }
    
  })
  
  # Atualizando cultura
  observe({
    
    if(!is.null(input$culturaInputDoencas)){
      
      dados = dadosDoencas()
      dados = dados[[1]]
      
      if(nrow(dados) > 0){
        updateSelectInput(
          session = session,
          inputId = "culturaInputDoencas2",
          choices = unique(dados$cultura),
          selected = unique(dados$cultura)[1]
        ) 
      }
    }
    
  })
  
  # output grafico media geral
  output$graficoDoencasPlot1 = renderPlot({
    
    #====================================#
    # Validacao
    
    validate(
      need(!is_null(dadosDoencas()),"Não há dados suficientes para exibicao do gráfico.")
    )
    
    #====================================#
    localSelect = input$select_doencas_local
    culturaSelect = input$culturaInputDoencas2
    
    gerador_graficos_cidade(dadosDoencas(), localSelect, culturaSelect)
    
  })
  
  # output grafico local
  output$graficoDoencasPlot2 = renderPlot({
    
    #====================================#
    # Validacao
    
    validate(
      need(!is_null(dadosDoencas()),"Não há dados suficientes para exibicao do gráfico.")
    )
    
    #====================================#
    safraSelect = input$safraInputDoencas2
    culturaSelect = input$culturaInputDoencas2
    
    gerador_graficos(dadosDoencas(), safraSelect, culturaSelect)
    
  })
  
}