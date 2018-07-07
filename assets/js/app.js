import "phoenix_html"
import { Socket } from "phoenix"
import $ from "js/jquery-1.10.2.min";

var pruneBackscroll, addToScroll, adjustScrollTop, clearScroll, command_history, disableField, focus, history_marker, setFocus, updateRoom, socket, push;
focus = null;
$('html').on('click', function (event) {
  if (window.getSelection().type !== "Range") {
    return setFocus("#command");
  }
});

updateRoom = function (data) {
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

clearScroll = function () {
  return $('#scroll').html("");
};

adjustScrollTop = function () {
  if ($(window).scrollTop() + $(window).height() > $(document).height() - 250) {
    return window.scrollTo(0, $('#scroll')[0].scrollHeight);
  }
};

setFocus = function (selector) {
  focus = selector;
  selector = $(selector);
  var x = window.scrollX, y = window.scrollY;
  selector.focus();
  window.scrollTo(x, y);
  return selector;
};
adjustScrollTop();

var socket = new Socket("" + (window.location.origin.replace('http', 'ws')) + "/ws");
socket.connect();
var chan = socket.channel("mud:play", { character: characterID });

chan.join().receive("error", ({ reason }) => window.location = "" + window.location.origin)

// chan.on("room", function(message){
//   updateRoom(message.html);
// });

chan.on("update_room", function (message) {
  if (player.power != message.power || player.level != message.level) {
    player.power = message.power;
    player.level = message.level;
    draw_map(message.room_id);
  }
  else {
    center_on_room(message.room_id)
  }

});

chan.on("clear scroll", function (message) {
  clearScroll();
});

chan.on("focus", function (message) {
  setFocus(message.html).select();
});

chan.on("disable", function (message) {
  disableField(message.html);
});

chan.on("update prompt", function (message) {
  $("#prompt").html(message.html);
});

chan.on("update score", function (score_data) {
  $("#score-name").text(_.padEnd(score_data.name, 12));
  $("#score-level").text(_.padEnd(score_data.level, 10));
  $("#score-accuracy").text(score_data.accuracy);
  $("#score-race").text(_.padEnd(score_data.race, 12));
  $("#score-class").text(_.padEnd(score_data.class, 10));
  $("#score-dodge").text(score_data.dodge);
  $("#score-ac").text(_.padEnd(score_data.physical_resistance, 12));
  $("#score-mr").text(_.padEnd(score_data.magical_resistance, 10));
  $("#score-parry").text(score_data.parry);
  $("#score-hp").text(_.padEnd(score_data.hp + "/" + score_data.max_hp, 12));
  $("#score-mana").text(_.padEnd(score_data.mana + "/" + score_data.max_mana, 10));
  $("#score-block").text(score_data.block);
  $("#score-stealth").text(score_data.stealth);
  $("#score-strength").text(_.padEnd(score_data.strength, 7));
  $("#score-agility").text(_.padEnd(score_data.agility, 8));
  $("#score-perception").text(score_data.perception);
  $("#score-intellect").text(_.padEnd(score_data.intellect, 7));
  $("#score-health").text(_.padEnd(score_data.health, 8));
  $("#score-crits").text(score_data.crits);
  $("#score-willpower").text(_.padEnd(score_data.willpower, 7));
  $("#score-charm").text(_.padEnd(score_data.charm, 8));
  $("#score-casting").text(score_data.spellcasting);
  $("#score-stone").text(_.padEnd(score_data.resistances.Stone, 5));
  $("#score-lightning").text(score_data.resistances.Electricity);
  $("#score-fire").text(_.padEnd(score_data.resistances.Fire, 5));
  $("#score-cold").text(score_data.resistances.Cold);
  $("#score-water").text(_.padEnd(score_data.resistances.Water, 5));
  $("#score-poison").text(score_data.resistances.Poison);
  $("#score-effects").text(_.join(score_data.effects, "\n"));
});

chan.on("update room essence", function (message) {
  $('.room-' + message.room_id + '-default').text(message.default);
  $('.room-' + message.room_id + '-good').text(message.good);
  $('.room-' + message.room_id + '-evil').text(message.evil);
});

chan.on("redirect", function (message) {
  window.location = "" + window.location.origin + message.url;
});

chan.on("open tab", function (message) {
  window.open(window.location.origin + message.url, "_blank")
});

chan.on("up", function (message) {
  command_history("up");
});

chan.on("scroll", function (message) {
  addToScroll("#scroll", _.unescape(message.html));
});

window.push = function (event, message) {
  chan.push(event, message)
};

pruneBackscroll = function () {
  var backscroll_size = 5000;

  if ($("#scroll").children().length > backscroll_size) {
    $("#scroll").children().first().remove();
    $("#scroll").children().first().remove();
  };
};

addToScroll = function (elem, text) {
  $(elem).append(text);
  pruneBackscroll();
  return adjustScrollTop();
};

disableField = function (selector) {
  return $(selector).prop('disabled', true).removeAttr('id');
};

history_marker = null;
command_history = function (direction) {
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

$(document).on('keydown', "input", function (event) {
  if (event.which === 9 && !event.shiftKey) {
    return event.preventDefault();
  }
});

$(document).on('keydown', function (event) {
  if (!(event.ctrlKey || event.shiftKey || event.metaKey)) {
    setFocus("#command");
  } else if (event.which === 75 && event.metaKey) {
    clearScroll();
    setFocus("#command");
  }
});

$(document).on('keyup', function (event) {
  setFocus("#command");
});

$(document).on('keyup', "input", function (event) {
  var command, params;
  event.preventDefault();
  if (event.which === 13 || (event.which === 9 && !event.shiftKey)) {
    history_marker = null;
    command = $(event.target).val();
    setFocus("#command").select();
    addToScroll($("#scroll"), "<p>" + $("#prompt").html() + " <span class='dark-yellow'>" + command + "</span></p>")
    return window.push(event.target.id, command);
  } else if (event.which === 38) {
    return command_history("up");
  } else if (event.which === 40) {
    return command_history("down");
  }
});
