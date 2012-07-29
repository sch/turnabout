// mini-pico-tiny convenience micro-framework, ymmv
function $(id){ return document.getElementById(id); }
function html(id, html){ $(id).innerHTML = html; }
function css(id, style){ $(id).style.cssText += ';'+style; }
function anim(id, transform, opacity, dur){
  css(id, '-webkit-transition:-webkit-transform'+
    ',opacity '+(dur||0.5)+'s,'+(dur||0.5)+'s;-webkit-transform:'+
    transform+';opacity:'+(1||opacity)); 
}
