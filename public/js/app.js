(function() {

  $(function() {
    var addToScroll, webSocket;
    webSocket = new WebSocket('ws://localhost:3000/_ws');
    webSocket.onopen = function(event) {
      return console.log("Connected!");
    };
    webSocket.onmessage = function(event) {
      console.log("Received message " + event.data);
      return addToScroll("#scroll", "<p>Message Received: " + event.data + "</p>");
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
        addToScroll('#scroll', "<p>Message Sent: " + command + "</p>");
        return webSocket.send(JSON.stringify({
          command: {
            text: command
          }
        }));
      }
    });
  });

}).call(this);
