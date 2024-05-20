# Registrando rotas.
router = make_router(
  route("/", bemVindoUI()),
  route("clima", climaUI),
  route("experimentos", experimentosUI),
  route("doencas", doencasUI),
  route("agricultor", agricultorUI),
  route("gerenciar", gerenciarUI())
)

# Pagina reponsavel pelo redirecionamento.
ui = fluidPage(router$ui)

if(LOGIN){
  
  ui = secure_app(
    fluidPage(router$ui),
    fab_position = 'none',
    language = "pt-BR",
    theme = 'flatly'
  )
  
}
