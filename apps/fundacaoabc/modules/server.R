# Global server
server = shinyServer(function(input, output, session) {
  
  # Autenticacao
  res_auth = secure_server(
    check_credentials = check_credentials(credenciais)
  )
  
  # Router
  router$server(input, output, session)
  
  # Doencas Server
  doencaServer(input, output, session)
  
  # clima Service
  mapaServer(input,output,session)
  
  # Doenca Service
  experimentoServer(input,output,session)
  
  # Gerenciar Server
  gerenciarServer(input, output, session)
  
  # Agricultor Server
  agricultorServer(input, output, session)
  
  
})
