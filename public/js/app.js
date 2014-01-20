(function() {

  $(function() {
    var addToScroll, adjustScrollTop, clearScroll, updateRoom, webSocket;
    $('body').on('click', function(event) {
      return $('#command').focus();
    });
    $('#command').focus();
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
    return $('#command').on('keyup', function(event) {
      var command;
      if (event.which === 13) {
        command = $(event.target).val();
        $(event.target).val("");
        addToScroll('#scroll', "<p><span class='dark-yellow'>" + command + "</span></p>");
        return webSocket.send(JSON.stringify({
          command: command
        }));
      }
    });
  });

}).call(this);
