# Gerenciar server
gerenciarServer = function(input, output, session) {
  
  # Botao retornar
  observeEvent(input$btnRetonarGerenciar,
               change_page('/')
  )
  
  # Upload arquivo
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
  
  # Escrevendo tabela para o usuário
  observe({
    
    if(!is.null(dadosUpload())){
      
      # Verificando separador do arquivo
      if(ncol(dadosUpload()) > 1){
        
        switch(input$baseGerenciarInput,
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
                 
               },
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
                 
               },
               "clima" = {
                 
                 if(verificarColunasClima(dadosUpload())){
                   
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
        )
        
      } else {
        
        shinyalert(
          title = 'Erro separador',
          text = 'Selecione outro separador',
          type = "error"
        )
      }
      
      
    } else{
      
      #Visualizacao tabela
      output$UItabela = renderUI({
        HTML('<center><h4>Escolha um arquivo para começar</h4></center>')
      })
      
    }
    
  })
  
  # Escrendo tabela
  output$tabelaUpload = renderDataTable({
    datatable(dadosUpload(),options = list(scrollY = '500px'))
  })
  
  
  #=======================Evento botao atualizar======================
  observeEvent(input$btnnatualizarDados, {
    
    # Verificando se o arquivo foi inserido
    if(!is.null(dadosUpload())){
      
      # Verificando seperador do arquivo
      if(ncol(dadosUpload()) > 1){
        
        switch(input$baseGerenciarInput,
               "doencas" = {
                 if(verificarColunaDoencas(dadosUpload())){
                   
                   show_modal_spinner(spin = 'circle')
                   
                   if(inserirDadosDoencas(dadosUpload())){
                     
                     shinyalert(
                       title = 'Sucesso',
                       text = 'Dados atualizados com sucesso',
                       type = "success"
                     )
                     
                   } 
                 } else {
                   
                   shinyalert(
                     title = 'Erro',
                     text = 'Erro ao atualizar dados',
                     type = "error"
                   )
                   
                 }
                 
                 remove_modal_progress()
                 
               },
               "experimentos" = {
                 if(verificarColunasExperimentos(dadosUpload())){
                   
                   show_modal_spinner(spin = 'circle')
                   
                   if(inserirDadosExperimentos(dadosUpload())){
                     
                     shinyalert(
                       title = 'Sucesso',
                       text = 'Dados atualizados com sucesso',
                       type = "success"
                     )
                     
                   } else {
                     
                     shinyalert(
                       title = 'Erro',
                       text = 'Erro ao atualizar dados',
                       type = "error"
                     )
                     
                   }
                   
                   remove_modal_progress()
                  
                 }
                 
               },
               "clima" = {
                 if(verificarColunasClima(dadosUpload())){
                   
                    show_modal_spinner(spin = 'circle')
                   
                    if(inserirDadosClima(dadosUpload())){
                      
                      shinyalert(
                        title = 'Sucesso',
                        text = 'Dados atualizados com sucesso',
                        type = "success"
                      )
                      
                    } 
                 } else {
                   
                   shinyalert(
                     title = 'Erro',
                     text = 'Erro ao atualizar dados',
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