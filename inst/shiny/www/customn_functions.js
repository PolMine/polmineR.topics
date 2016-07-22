Shiny.addCustomMessageHandler(
  type = "jsCode",
  function(message) {Shiny.onInputChange("deleteConfirmChoice", eval(message.value));}
)