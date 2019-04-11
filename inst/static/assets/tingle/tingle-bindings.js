
var modal, close_label, footer_btn_class;

Shiny.addCustomMessageHandler('show_tingle',
  function(obj) {
    if (obj.hasOwnProperty('id_close')) {
      Shiny.setInputValue(obj.id_close, null);
    }
    var config = obj.opts;
    if (obj.hasOwnProperty('footer')) {
      if (obj.footer.footer_tingle) {
        close_label = obj.footer.footer_content.label;
        footer_btn_class = obj.footer.footer_content.class;
      }
      config.footer = true;
    } else {
      config.footer = false;
    }
    config.onClose = function() {
      if (obj.hasOwnProperty('id_close')) {
        Shiny.setInputValue(obj.id_close, Date.now());
      }
    };
    modal = new tingle.modal(config);
    modal.setContent('<div id="tingle-modal"></div>');
    if (obj.hasOwnProperty('footer')) {
      if (obj.footer.footer_tingle) {
        modal.addFooterBtn(close_label, footer_btn_class, function() {
          modal.close();
        });
      } else {
        modal.setFooterContent('<div id="tingle-footer"></div>');
      }
    }
    modal.open();
    modal.checkOverflow();
});

Shiny.addCustomMessageHandler('remove_tingle',
  function(obj) {
    modal.close();
});
