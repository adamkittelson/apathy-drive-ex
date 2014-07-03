var socket = new Phoenix.Socket("ws://" + location.host + "/ws");
var $messages = $("#messages");
var $messageInput = $("#message-input");
var $usernameInput = $("#username");
var blah = "";

socket.join("mud", "lobby", {username: "shaitan"}, function(chan){

  chan.on("user:entered", function(message){
    console.log("user entered: " + message.username )
  });

  chan.on("new:message", function(msg){
    console.log("new:message: " + msg.username + " " + msg.content )
  });

  blah = function(username, message) {
    console.log(chan);
    chan.send("new:message", {
      content: message,
      username: username
    })
  };
});

