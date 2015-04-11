$(function() {
  var addToScroll, adjustScrollTop, clearScroll, command_history, disableField, focus, focusNext, focusPrevious, history_marker, setFocus, updateRoom, socket, send;
  focus = null;
  $('body').on('click', function(event) {
    return setFocus(focus);
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
    $("#scroll_container").css("top", $("#room").height() + 10 + "px");
    return $('#scroll').scrollTop($('#scroll')[0].scrollHeight);
  };
  setFocus = function(selector) {
    focus = selector;
    return $(selector).focus();
  };
  adjustScrollTop();

  var spirit_id;

  spirit_id = parseInt($("#spirit_id").text());

  socket = new Phoenix.Socket("" + (window.location.origin.replace('http', 'ws')) + "/ws");
  socket.join("mud", {spirit: spirit_id}, function(chan){

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
      console.log("message: " + message.html);
      addToScroll("#scroll", message.html);
    });

    send = function(event, message) {
             chan.send(event, message)
           };
  });

  addToScroll = function(elem, text) {
    $(elem).append(text);
    $(elem).append($("#prompt").parent().detach());
    setFocus(focus);
    return $('#scroll').scrollTop($('#scroll')[0].scrollHeight);
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
  return $(document).on('keyup', "input", function(event) {
    var command, params;
    event.preventDefault();
    if (event.which === 13 || (event.which === 9 && !event.shiftKey)) {
      history_marker = null;
      command = $(event.target).val();
      return send(event.target.id, command);
    } else if (event.which === 38) {
      return command_history("up");
    } else if (event.which === 40) {
      return command_history("down");
    }
  });
});
