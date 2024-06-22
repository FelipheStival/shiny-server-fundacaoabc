# Gerenciar server
gerenciarServer = function(input, output, session) {
  
  # Botao retornar
  observeEvent(input$btnRetonarGerenciar,
               change_page('/')
  )
  
  #====================================#
  # Upload do arquivo
  dadosUpload = reactive({
    
    dados = NULL
    
    if(!is.null(input$arquivo)){
      
      dados = read.table(input$arquivo$datapath,
                         header = input$cabecalho,
                         sep = input$separador,
                         stringsAsFactors = F)
    }
    
    return(dados)
    
  })
  
  #====================================#
  
  
  #====================================#
  # Exibição da tabela para o usuário
  
  observe({
    
    if(!is.null(dadosUpload())){
      
      #====================================#
      # Validando arquivo
      
      if(ncol(dadosUpload()) > 1){
        
        switch(input$baseGerenciarInput,
               
               #====================================#
               # Validando e exibindo dados doenca
               
               "doencas" = {
                 
                 if(verificarColunaDoencas(dadosUpload())){
                    
                   output$UItabela = renderUI({
                     
                     dataTableOutput("tabelaUpload")
                     
                   })
                   
                 } else {
                   
                   shinyalert(
                     title = 'Colunas obrigatorias',
                     text = 'A tabela para a atualização dos dados das doenças é inválida',
                     type = "error"
                   )
                   
                 }
                 
                 #====================================#
                 
               },
               
               #====================================#
               # Validando e exibindo dados experimentos
               
               "experimentos" = {
                 
                 if(verificarColunasExperimentos(dadosUpload())){
                   
                   output$UItabela = renderUI({
                     
                     dataTableOutput("tabelaUpload")
                     
                   })
                   
                 } else {
                   
                   shinyalert(
                     title = 'Colunas obrigatorias',
                     text = 'A tabela para a atualização dos dados climáticos é inválida',
                     type = "error"
                   )
                   
                 }
                 
                 #====================================#
                 
               },
               
               #====================================#
               # Validando e exibindo dados clima
               
               "clima" = {
                 
                 if(verificarColunasClima(dadosUpload())) {
                   
                   output$UItabela = renderUI({
                     
                     dataTableOutput("tabelaUpload")
                     
                   })
                   
                 } else {
                   
                   shinyalert(
                     title = 'Colunas obrigatorias',
                     text = 'A tabela para a atualização dos dados climáticos é inválida',
                     type = "error"
                   )
                   
                 }
                 
               }
               
               #====================================#
        )
        
      } else {
        
        shinyalert(
          title = 'Erro separador',
          text = 'Selecione outro separador',
          type = "error"
        )
      }
      
      
    } else{
      
      #====================================#
      # Escreve aviso para usuário importar os dados
      
      output$UItabela = renderUI({
        HTML('<center><h4>Escolha um arquivo para começar</h4></center>')
      })
      
      #====================================#
      
      
    }
    
  })
  
  #====================================#
  
  
  #====================================#
  # Escreve Tabela para visualização
  
  output$tabelaUpload = renderDataTable({
    
    datatable(dadosUpload(),options = list(scrollY = '500px'))
    
  })
  
  #====================================#
  
  
  
  #====================================#
  # Botão de atualizaros dados
  
  observeEvent(input$btnAtualizarDados, {
    
    # Verificando se o arquivo foi inserido
    if(!is.null(dadosUpload())){
      
      # Verificando seperador do arquivo
      if(ncol(dadosUpload()) > 1){
        
        switch(input$baseGerenciarInput,
               "doencas" = {
                 
                 #============================================
                 # Método para atualizar os dados de doencas
                 #============================================
                 if(verificarColunaDoencas(dadosUpload())){
                   
                   show_modal_spinner(spin = 'circle')
                   result = inserirDadosDoencas(dadosUpload())
                   
                   if(result$status){
                     
                     shinyalert(
                       title = 'Sucesso',
                       text = result$message,
                       type = "success"
                     )
                     
                   } 
                 } else {
                   
                   shinyalert(
                     title = 'Erro',
                     text = result$message,
                     type = "error"
                   )
                   
                 }
                 
                 remove_modal_progress()
                 
               },
               
               #=================================================
               # Método para atualiazr os dados de experimentos
               #=================================================
               "experimentos" = {
                 
                 if(verificarColunasExperimentos(dadosUpload())){
                   
                   show_modal_spinner(spin = 'circle')
                   result = inserirDadosExperimentos(dadosUpload())
                   
                   if(result$status){
                     
                     shinyalert(
                       title = 'Sucesso',
                       text = result$message,
                       type = "success"
                     )
                     
                   } else {
                     
                     shinyalert(
                       title = 'Erro',
                       text = result$message,
                       type = "error"
                     )
                     
                   }
                   
                   remove_modal_progress()
                  
                 }
                 
               },
               
               #===========================================
               # Método para atualizar os dados de clima
               #===========================================
               "clima" = {
                 
                 if(verificarColunasClima(dadosUpload())){
                   
                    show_modal_spinner(spin = 'circle')
                    result = inserirDadosClima(dadosUpload())
                   
                    if(result$status){
                      
                      shinyalert(
                        title = 'Sucesso',
                        text = result$message,
                        type = "success"
                      )
                      
                    } 
                 } else {
                   
                   shinyalert(
                     title = 'Erro',
                     text = result$message,
                     type = "error"
                   )
                   
                 }
                 
                 remove_modal_progress()
                 
               }
        )    
        
      } else {
        
        shinyalert(
          title = 'Erro',
          text = 'O separador informado é inválido, por favor teste outro',
          type = "error"
        )
        
      }
      
    } else {
      
      shinyalert(
        title = 'Erro',
        text = 'Nenhum arquivo informado',
        type = "error"
      )
      
    }
    
  })
  
}