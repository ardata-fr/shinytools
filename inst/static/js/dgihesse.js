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

function set_active(id, state){
  if( state ){
    addClass(id, 'active');
  } else {
    unClass(id, 'active');
  }
}
Shiny.addCustomMessageHandler('set_active',
  function(data) {
    set_active(data.id, data.state);
  }
);

function disabled(id, state){

  if( inherits(id, 'form-group')){
    var elts = document.querySelectorAll('#' + id + ' input');
    for( var i = 0 ; i < elts.length ; i++) {
      if( state ){
        elts[i].setAttribute("disabled", true);
      } else {
        elts[i].removeAttribute("disabled");
      }
    }
    return;
  }
  var x = document.getElementById(id);
  if( inherits(id, 'selectized') && state ){
    x.selectize.disable();
    return;
  } else if( inherits(id, 'selectized') && !state ){
    x.selectize.enable();
    return;
  }

  if( inherits(id, 'js-range-slider')  ){
    $("#" + id).data("ionRangeSlider").update({ disable : state });
    return;
  }

  if( state ){
    x.setAttribute("disabled", true);
  } else {
    x.removeAttribute("disabled");
  }
}



Shiny.addCustomMessageHandler('disabled',
  function(data) {
    setTimeout(disabled(data.id, data.state), 2000);

  }
);

function toogle(id){
  var x = document.getElementById(id);
  if( x.style.display === "" ){
    x.style.display = "none";
  } else {
    x.style.display = null;
  }
}

Shiny.addCustomMessageHandler('toogle',
  function(data) {
    toogle(data.id);
  }
);
