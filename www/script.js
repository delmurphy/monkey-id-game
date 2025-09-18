$(document).ready(function() {
  Shiny.addCustomMessageHandler("changeBackgroundImage", function(message) {
    $('body').toggleClass('background2');
  });
});