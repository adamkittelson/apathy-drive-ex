var addToScroll, adjustScrollTop, clearScroll, command_history, disableField, focus, focusNext, focusPrevious, history_marker, setFocus, updateRoom, socket, push;
focus = null;
$('html').on('click', function(event) {
  if (window.getSelection().type !== "Range") {
    return setFocus("#command");
  }
});

updateRoom = function(data) {
  $('#room .title').html(data['name']);
  $('#room .description').html(data['description']);
  if (data['entities'].length > 0) {
    $('#room .entities').html("<span class='dark-magenta'>Also here:</span> <span class='magenta'>" + (data['entities'].join(', ')) + "</span><span class='dark-magenta'>.</span>");
  } else {
    $('#room .entities').html("");
  }
  $('#room .exits').html("Obvious exits: " + (data['exits'].join(', ') || 'NONE'));
  return adjustScrollTop();
};

clearScroll = function() {
  return $('#scroll').html("");
};

adjustScrollTop = function() {
  if ($(window).scrollTop() + $(window).height() > $(document).height() - 250) {
    return window.scrollTo(0, $('#scroll')[0].scrollHeight);
  }
};

setFocus = function(selector) {
  focus = selector;
  selector = $(selector);
  var x = window.scrollX, y = window.scrollY;
  selector.focus();
  window.scrollTo(x, y);
  return selector;
};
adjustScrollTop();

var socket = new Phoenix.Socket("" + (window.location.origin.replace('http', 'ws')) + "/ws");
socket.connect();
var chan = socket.chan("mud", {character: characterID});

chan.join()

chan.on("room", function(message){
  updateRoom(message.html);
});

chan.on("clear scroll", function(message){
  clearScroll();
});

chan.on("focus", function(message){
  setFocus(message.html).select();
});

chan.on("disable", function(message){
  disableField(message.html);
});

chan.on("update prompt", function(message){
  $("#prompt").text(message.html);
});

chan.on("redirect", function(message){
  window.location = "" + window.location.origin + message.url;
});

chan.on("up", function(message){
  command_history("up");
});

chan.on("scroll", function(message){
  addToScroll("#scroll", message.html);
});

push = function(event, message) {
         chan.push(event, message)
       };

addToScroll = function(elem, text) {
  $(elem).append(text);
  $(elem).append($("#prompt").parent().detach());
  setFocus(focus);
  return adjustScrollTop();
};

focusNext = function(elem) {
  var field, fields;
  fields = $("#scroll").find('input:not([disabled])');
  field = fields.eq(fields.index(elem) + 1)[0];
  if (field) {
    return setFocus("#" + field.id).select();
  }
};

focusPrevious = function(elem) {
  var field, fields;
  fields = $("#scroll").find(':input');
  field = fields.eq(fields.index(elem) - 1)[0];
  if (field) {
    return setFocus("#" + field.id).select();
  }
};

disableField = function(selector) {
  return $(selector).prop('disabled', true).removeAttr('id');
};

history_marker = null;
command_history = function(direction) {
  var history;
  history = $('.prompt:disabled');
  if (history.length === 0) {
    return;
  }
  if (history_marker === null) {
    history_marker = history.length;
  }
  if (direction === "up") {
    history_marker = Math.max(0, history_marker - 1);
  } else if (direction === "down") {
    history_marker = Math.min(history.length - 1, history_marker + 1);
  }
  $("#command").val(history[history_marker].value);
  return setFocus("#command").select();
};

$(document).on('keydown', "input", function(event) {
  if (event.which === 9 && !event.shiftKey) {
    return event.preventDefault();
  }
});

$(document).on('keydown', function(event) {
  if (!(event.ctrlKey || event.shiftKey || event.metaKey)) {
    setFocus("#command");
  } else if (event.which === 75 && event.metaKey) {
    var prompt = $("#prompt").parent().detach();
    clearScroll();
    $("#scroll").append(prompt);
    setFocus("#command");
  }
});

$(document).on('keyup', function(event) {
  setFocus("#command");
});

$(document).on('keyup', "input", function(event) {
  var command, params;
  event.preventDefault();
  if (event.which === 13 || (event.which === 9 && !event.shiftKey)) {
    history_marker = null;
    command = $(event.target).val();
    if (command === "reroll") {
      if (confirm("Rerolling will allow you to change your name and/or faction, but you will only retain 10% of your current experience. Are you sure you wish to reroll?") === true) {
        return push(event.target.id, command);
      }
      else {
        $(event.target).val("")
      }
    }
    else {
      return push(event.target.id, command);
    }
  } else if (event.which === 38) {
    return command_history("up");
  } else if (event.which === 40) {
    return command_history("down");
  }
});
