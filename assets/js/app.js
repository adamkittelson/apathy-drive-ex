import "phoenix_html"
import { Socket } from "phoenix"

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
  if ($('#scroll').scrollTop() + $('#scroll').height() > $('#scroll')[0].scrollHeight - 400) {
    return $("#scroll").scrollTop($('#scroll')[0].scrollHeight);
  }
};

window.linkify = function (text) {
  var exp = /(\b(https?):\/\/[-A-Z0-9+&@#\/%?=~_|!:,.;]*[-A-Z0-9+&@#\/%=~_|])/ig;

  return text.replace(exp, function (str) {
    var url = new URL(str);
    return "<a class='autolink' href='" + str + "' target='blank'>link(" + url.hostname + ")</a>";
  });
}


window.adjustChatTop = function () {
  if ($('#chat').scrollTop() + $('#chat').height() > $('#chat')[0].scrollHeight - 400) {
    return $("#chat").scrollTop($('#chat')[0].scrollHeight);
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
  }
  center_on_room(message.room_id)
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

var update_score_attribute = function (attribute, new_value) {
  var original_value = $("#score-" + attribute).text()
  if (original_value !== new_value) {
    $("#score-" + attribute).text(new_value);
  }
}

chan.on("update exp bar", function (data) {
  progress($(".score .experience"), data.percentage)
})


chan.on("update attribute bar", function (data) {
  progress($(".score ." + data.attribute), data.percentage)
})

chan.on("update energy bar", function (data) {
  if (data.player) {
    progress($("#player-bars .energy"), data.percentage, data.time_to_full)
  }

  progress($("#" + data.ref + "-bars .energy"), data.percentage, data.time_to_full)
})

chan.on("update mana bar", function (data) {
  if (data.player) {
    progress($("#player-bars .mana"), data.percentage, data.time_to_full)
  }

  progress($("#" + data.ref + "-bars .mana"), data.percentage, data.time_to_full)
})

chan.on("update hp bar", function (data) {
  if (data.player) {
    progress($("#player-bars .hp"), data.percentage, data.time_to_full)
  }

  progress($("#" + data.ref + "-bars .hp"), data.percentage, data.time_to_full)
})

chan.on("update mob list", function (data) {
  $("#moblist").html(data.html);
  adjustChatTop();
})

chan.on("update score", function (score_data) {
  update_score_attribute("name", _.padEnd(score_data.name, 12));
  update_score_attribute("level", _.padEnd(score_data.level, 10));
  update_score_attribute("accuracy", score_data.accuracy);
  update_score_attribute("race", _.padEnd(score_data.race, 12));
  update_score_attribute("class", _.padEnd(score_data.class, 10));
  update_score_attribute("dodge", score_data.dodge);
  update_score_attribute("alignment", _.padEnd(_.capitalize(score_data.alignment), 7));
  update_score_attribute("acmr", _.padEnd(score_data.physical_resistance + '/' + score_data.magical_resistance, 10));
  update_score_attribute("parry", score_data.parry);
  update_score_attribute("hp", _.padEnd(score_data.hp + "/" + score_data.max_hp, 12));
  update_score_attribute("mana", _.padEnd(score_data.mana + "/" + score_data.max_mana, 10));
  update_score_attribute("block", score_data.block);
  update_score_attribute("stealth", score_data.stealth);
  update_score_attribute("strength", _.padEnd(score_data.strength, 7))
  update_score_attribute("agility", _.padEnd(score_data.agility, 8));
  update_score_attribute("perception", score_data.perception);
  update_score_attribute("intellect", _.padEnd(score_data.intellect, 7));
  update_score_attribute("health", _.padEnd(score_data.health, 8));
  update_score_attribute("crits", score_data.crits);
  update_score_attribute("willpower", _.padEnd(score_data.willpower, 7));
  update_score_attribute("charm", _.padEnd(score_data.charm, 8));
  update_score_attribute("stone", _.padEnd(score_data.resistances.Stone, 5));
  update_score_attribute("lightning", score_data.resistances.Electricity);
  update_score_attribute("fire", _.padEnd(score_data.resistances.Fire, 5));
  update_score_attribute("cold", score_data.resistances.Cold);
  update_score_attribute("water", _.padEnd(score_data.resistances.Water, 5));
  update_score_attribute("poison", score_data.resistances.Poison);
  update_score_attribute("effects", _.join(score_data.effects, "\n  "));
});

window.pulsate_attribute = function (attribute) {
  $("#score-" + attribute).stop().animate({ color: "lime" }, 250, function () { $("#score-" + attribute).animate({ color: "teal" }, 250) })
}

chan.on("pulse score attribute", function (data) {
  pulsate_attribute(data.attribute);
})

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
  addToScroll("#scroll", message.html);
});

chan.on("chat", function (message) {
  message = linkify(message.html);
  addToScroll("#scroll", message);
  $("#chat").append(message);
  return adjustChatTop();
});

chan.on("chat-sidebar", function (message) {
  message = linkify(message.html);
  $("#chat").append(message);
  return adjustChatTop();
});

window.push = function (event, message) {
  chan.push(event, message)
};

pruneBackscroll = function () {
  var backscroll_size = 1000;

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

window.progress = function (elem, percent, time_to_full) {
  if (percent == 100) {
    // I don't remember why this was here, but it seems to
    // cause flickering to 100 when it shouldn't
    // elem.find('div').stop().width(elem.width())
  }
  else {
    if (time_to_full) {
      if (time_to_full > 0) {
        var currentWidth = percent * elem.width() / 100;

        if (currentWidth >= 0) {
          elem.find('div').finish().animate({ width: currentWidth }, { duration: 0 }).animate({ width: elem.width() }, { duration: time_to_full, easing: "linear" });
        }
      }
      else if (time_to_full < 0) {
        var currentWidth = percent * elem.width() / 100;

        if (currentWidth >= 0) {
          elem.find('div').finish().animate({ width: currentWidth }, { duration: 0 }).animate({ width: 0 }, { duration: -time_to_full, easing: "linear" });
        }
      }

    } else {
      var currentWidth = percent * elem.width() / 100;

      elem.find('div').width(currentWidth)
    }

  }

}
