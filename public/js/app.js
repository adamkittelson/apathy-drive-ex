(function() {

  $(function() {
    var addToScroll, adjustScrollTop, clearScroll, disableField, focus, focusNext, focusPrevious, setFocus, updateRoom, webSocket;
    focus = null;
    $('body').on('click', function(event) {
      return setFocus(focus);
    });
    updateRoom = function(data) {
      console.log(data);
      $('#room .title').html(data['name']);
      $('#room .description').html(data['description']);
      $('#room .exits').html("Obvious exits: " + (data['exits'].join(', ') || 'NONE'));
      return adjustScrollTop();
    };
    clearScroll = function() {
      return $('#scroll').html("");
    };
    adjustScrollTop = function() {
      return $("#scroll_container").css("top", $("#room").height() + 10 + "px");
    };
    setFocus = function(selector) {
      focus = selector;
      return $(selector).focus().select();
    };
    adjustScrollTop();
    webSocket = new WebSocket('ws://localhost:3000/_ws');
    webSocket.onopen = function(event) {
      return console.log("Connected!");
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
          return setFocus(message[1]);
        case "disable":
          return disableField(message[1]);
        default:
          return addToScroll("#scroll", message[1]);
      }
    };
    webSocket.onclose = function(event) {
      return console.log("Connection closed!");
    };
    addToScroll = function(elem, text) {
      $(elem).append(text);
      return $('#scroll').scrollTop($('#scroll')[0].scrollHeight);
    };
    focusNext = function(elem) {
      var field, fields;
      fields = $("#scroll").find('input:not([disabled])');
      field = fields.eq(fields.index(elem) + 1)[0];
      if (field) {
        return setFocus("#" + field.id);
      }
    };
    focusPrevious = function(elem) {
      var field, fields;
      fields = $("#scroll").find(':input');
      field = fields.eq(fields.index(elem) - 1)[0];
      if (field) {
        return setFocus("#" + field.id);
      }
    };
    disableField = function(selector) {
      return $(selector).prop('disabled', true).removeAttr('id');
    };
    return $(document).on('keyup', "input", function(event) {
      var command, params;
      event.preventDefault();
      if (event.which === 13) {
        command = $(event.target).val();
        if (event.target.id === "command") {
          addToScroll('#scroll', "<p><span class='dark-yellow'>" + command + "</span></p>");
          $(event.target).val("");
        } else {
          focusNext($(event.target));
        }
        params = {};
        params[event.target.id] = command;
        return webSocket.send(JSON.stringify(params));
      } else if (event.which === 38) {
        return focusPrevious($(event.target));
      } else if (event.which === 40) {
        return focusNext($(event.target));
      }
    });
  });

}).call(this);
