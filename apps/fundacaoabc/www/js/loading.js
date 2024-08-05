/**
 * Método para mostrar o carregamento
 */
function showLoading() {
  
  let loading = $('<div class = "loading-page"><img src="logos/abc.png"></div>');
  $('body').append(loading);
  
}

/***
 * Método para remover o loading da tela
 */
function removeLoading() {
  console.log('remove o loading');
}

/**
 * Verifica se é para remover o loading
 */
$(document).on('loadedFeliphe', () => {
  
  console.log('aqui');
  
});