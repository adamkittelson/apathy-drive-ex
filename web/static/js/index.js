import {Socket} from "deps/phoenix/web/static/js/phoenix"

var update_war_status = function(war_status) {
  $('#war-status').html(war_status.stats.join("\n    "))
}

var socket = new Socket("" + (window.location.origin.replace('http', 'ws')) + "/ws");
socket.connect();
var chan = socket.channel("index", {});

chan.join().receive("ok", (message) => {
  update_war_status(message)
})

chan.on("war-status", function(message){
  update_war_status(message)
});
