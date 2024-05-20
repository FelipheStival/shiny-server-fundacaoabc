var menuItem = document.getElementById("container");

/*
* Funcao para redirecionar o usuário clicando nos cartões.
*
*/
menuItem.onclick = function(e){
  let opcao = e.target.innerText;
  let href = window.location.href
   switch (opcao) {
  case 'Clima':
    window.location.href = href + "clima"
    break;
  case 'Doenças':
    window.location.href = href + "doencas"
    break;
  case 'Experimentos':
    window.location.href = href + "experimentos"
    break;
  case 'Agricultor':
    window.location.href = href + "agricultor"
    break;
  case 'Gerenciar':
    window.location.href = href + "gerenciar"
    break;
  default:
    console.log('Não escolheu nenhum');
  }
  console.log(opcao);
}

$(document).ready(function() {
  document.title = 'Fundação ABC';
});
