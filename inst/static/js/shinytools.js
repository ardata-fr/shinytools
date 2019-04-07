function inherits(id, className) {
  var x = document.getElementById(id);
  var out = null;
  if (x.classList){
    out = x.classList.contains(className);
  } else {
    var classname = x.className;
    out = !!classname.match(new RegExp('(\\s|^)' + className + '(\\s|$)'));
  }
  return out;
}

function addClass(id, className) {
  var x = document.getElementById(id);
  if (x.classList){
    x.classList.add(className);
  } else if (!inherits(x, className)){
    x.className += " " + className;
  }
}
Shiny.addCustomMessageHandler('addClass',
  function(data) {
    addClass(data.id, data.className);
  }
);

function unClass(id, className) {
  var x = document.getElementById(id);
  if (x.classList){
    x.classList.remove(className);
  } else if (inherits(x, className)) {
    var regex = new RegExp('(\\s|^)' + className + '(\\s|$)');
    x.className=x.className.replace(regex, ' ');
  }
}
Shiny.addCustomMessageHandler('unClass',
  function(data) {
    unClass(data.id, data.className);
  }
);

/* activate */
function activate(id, state){
  if( state ){
    addClass(id, 'active');
  } else {
    unClass(id, 'active');
  }
}
Shiny.addCustomMessageHandler('activate',
  function(data) {
    activate(data.id, data.state);
  }
);


/* enable */
function enable(id, state){

  if( inherits(id, 'form-group')){
    var elts = document.querySelectorAll('#' + id + ' input');
    for( var i = 0 ; i < elts.length ; i++) {
      if( state ){
        elts[i].removeAttribute("disabled");
      } else {
        elts[i].setAttribute("disabled", true);
      }
    }
    return;
  }
  var x = document.getElementById(id);
  if( inherits(id, 'selectized') && !state ){
    x.selectize.disable();
    return;
  } else if( inherits(id, 'selectized') && state ){
    x.selectize.enable();
    return;
  }

  if( inherits(id, 'js-range-slider')  ){
    $("#" + id).data("ionRangeSlider").update({ disable : !state });
    return;
  }

  if( state ){
    x.removeAttribute("disabled");
  } else {
    x.setAttribute("disabled", true);
  }
}



Shiny.addCustomMessageHandler('enable',
  function(data) {
    setTimeout(enable(data.id, data.state), 2000);

  }
);

/* toogle */
function html_toogle(id){
  var x = document.getElementById(id);
  if( x.style.display === "" ){
    x.style.display = "none";
  } else {
    x.style.display = null;
  }
}
function html_set_visible(id){
  var x = document.getElementById(id);
  x.style.display = null;
}
function html_set_hidden(id){
  var x = document.getElementById(id);
  x.style.display = "none";
}

Shiny.addCustomMessageHandler('html_toogle',
  function(data) {
    html_toogle(data.id);
  }
);
Shiny.addCustomMessageHandler('html_set_visible',
  function(data) {
    html_set_visible(data.id);
  }
);
Shiny.addCustomMessageHandler('html_set_hidden',
  function(data) {
    html_set_hidden(data.id);
  }
);

/* add counter variable */
function registerCounter(id, event_id, event_type){
  document.addEventListener('DOMContentLoaded', function() {
    var x = document.getElementById(id);
    window[event_id] = 0;
    x.addEventListener(event_type, function(e){
        window[event_id]++;
        Shiny.onInputChange(event_id, window[event_id]);
    },false);

  });

}

