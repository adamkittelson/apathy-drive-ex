(function() {

  $(function() {
    var addToScroll, adjustScrollTop, webSocket;
    adjustScrollTop = function() {
      return $("#scroll_container").css("top", $("#room").height() + 10 + "px");
    };
    adjustScrollTop();
    webSocket = new WebSocket('ws://localhost:3000/_ws');
    webSocket.onopen = function(event) {
      return console.log("Connected!");
    };
    webSocket.onmessage = function(event) {
      console.log("Received message " + event.data);
      return addToScroll("#scroll", "<div>Message Received: " + event.data + "</div>");
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
        $(event.target).select();
        addToScroll('#scroll', "<div>Message Sent: " + command + "</div>");
        return webSocket.send(JSON.stringify({
          command: {
            text: command
          }
        }));
      }
    });
  });

}).call(this);
