(function() {

  $(function() {
    var addToScroll, adjustScrollTop, clearScroll, command_history, disableField, focus, focusNext, focusPrevious, history_marker, setFocus, updateRoom, webSocket;
    focus = null;
    $('body').on('click', function(event) {
      return setFocus(focus);
    });
    updateRoom = function(data) {
      console.log(data);
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
    webSocket = new WebSocket("" + (window.location.origin.replace('http', 'ws:')) + "/_ws");
    webSocket.onopen = function(event) {
      var pathparts, url;
      console.log("Connected!");
      pathparts = window.location.pathname.split("/");
      url = pathparts[pathparts.length - 1];
      return webSocket.send(JSON.stringify({
        login: url
      }));
    };
    webSocket.onmessage = function(event) {
      var message;
      message = JSON.parse(event.data);
      switch (message[0]) {
        case "room":
          return updateRoom(message[1]);
        case "clear scroll":
          return clearScroll();
        case "focus":
          return setFocus(message[1]).select();
        case "disable":
          return disableField(message[1]);
        case "update":
          return $(message[1]).html(message[2]);
        case "set field":
          return $(message[1]).val(message[2]);
        case "update prompt":
          return $("#prompt").text(message[1]);
        case "redirect":
          return window.location = "" + window.location.origin + message[1];
        default:
          return addToScroll("#scroll", message[1]);
      }
    };
    webSocket.onclose = function(event) {
      $("#scroll").append("<p>Disconnected from server.</p>");
      disableField("#command");
      return console.log("Connection closed!");
    };
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
        params = {};
        params[event.target.id] = command;
        return webSocket.send(JSON.stringify(params));
      } else if (event.which === 38) {
        return command_history("up");
      } else if (event.which === 40) {
        return command_history("down");
      }
    });
  });

}).call(this);