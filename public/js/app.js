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
          return setFocus(message[1]).select();
        case "disable":
          return disableField(message[1]);
        case "update":
          return $(message[1]).html(message[2]);
        case "set field":
          return $(message[1]).val(message[2]);
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
    return $(document).on('keyup', "input", function(event) {
      var command, params, value;
      event.preventDefault();
      if (event.which === 13) {
        command = $(event.target).val();
        if (event.target.id !== "command") {
          $("#validation").html("");
          focusNext($(event.target));
        }
        params = {};
        params[event.target.id] = command;
        return webSocket.send(JSON.stringify(params));
      } else if (event.which === 38) {
        value = parseInt($(event.target).val());
        if (!isNaN(value)) {
          $("#validation").html("");
          $(event.target).val(value + 1);
          params = {};
          params[event.target.id] = "" + (value + 1);
          return webSocket.send(JSON.stringify(params));
        }
      } else if (event.which === 40) {
        value = parseInt($(event.target).val());
        if (!isNaN(value)) {
          $("#validation").html("");
          $(event.target).val(value - 1);
          params = {};
          params[event.target.id] = "" + (value - 1);
          return webSocket.send(JSON.stringify(params));
        }
      } else if (event.which === 32) {
        params = {};
        params["cycle"] = event.target.id;
        return webSocket.send(JSON.stringify(params));
      }
    });
  });

}).call(this);
