/**
 * Método para mostrar o carregamento
 */
function showLoading() {
  
  let loading = $('<div class = "loading-page"><img src="logos/abc.png"></div>');
  $('body').append(loading);
  
}


$('#wave-background').ready(function() {
  
  $el = $(this);
  $menu = $el.find('.menu-item');
  
  $menu.on('click', function() {
    
    let $el = $(this);
    let id = $el.attr('id');
    let href =  window.location.href;
    
    switch(id) {
      case 'opcao-clima':
        window.location.href = href + 'clima';
        break;
      case 'opcao-experimentos':
        window.location.href = href + 'experimentos';
        break;
      case 'opcao-doencas':
        window.location.href = href + 'doencas';
        break;
      case 'opcao-agricultor':
        window.location.href = href + 'agricultor';
        break;
      case 'opcao-gerenciar':
        window.location.href = href + 'gerenciar';
        break;
      default:
      
    }
    
  });
  
});