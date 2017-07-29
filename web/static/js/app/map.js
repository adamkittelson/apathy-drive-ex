import {Socket} from "deps/phoenix/web/static/js/phoenix"
import $ from "web/static/js/app/jquery-1.10.2.min";

$(document).ready(function() {

  var prefix = "", _addEventListener, onwheel, support;

  // detect event model
  if ( window.addEventListener ) {
      _addEventListener = "addEventListener";
  } else {
      _addEventListener = "attachEvent";
      prefix = "on";
  }

  // detect available wheel event
  support = "onwheel" in document.createElement("div") ? "wheel" : // Modern browsers support "wheel"
            document.onmousewheel !== undefined ? "mousewheel" : // Webkit and IE support at least "mousewheel"
            "DOMMouseScroll"; // let's assume that remaining browsers are older Firefox

  function addWheelListener( elem, callback, useCapture ) {
      _addWheelListener( elem, support, callback, useCapture );

      // handle MozMousePixelScroll in older Firefox
      if( support == "DOMMouseScroll" ) {
          _addWheelListener( elem, "MozMousePixelScroll", callback, useCapture );
      }
  };

  function _addWheelListener( elem, eventName, callback, useCapture ) {
      elem[ _addEventListener ]( prefix + eventName, support == "wheel" ? callback : function( originalEvent ) {
          !originalEvent && ( originalEvent = window.event );

          // create a normalized event object
          var event = {
              // keep a ref to the original event object
              originalEvent: originalEvent,
              target: originalEvent.target || originalEvent.srcElement,
              type: "wheel",
              deltaMode: originalEvent.type == "MozMousePixelScroll" ? 0 : 1,
              deltaX: 0,
              delatZ: 0,
              preventDefault: function() {
                  originalEvent.preventDefault ?
                      originalEvent.preventDefault() :
                      originalEvent.returnValue = false;
              }
          };

          // calculate deltaY (and deltaX) according to the event
          if ( support == "mousewheel" ) {
              event.deltaY = - 1/40 * originalEvent.wheelDelta;
              // Webkit also support wheelDeltaX
              originalEvent.wheelDeltaX && ( event.deltaX = - 1/40 * originalEvent.wheelDeltaX );
          } else {
              event.deltaY = originalEvent.detail;
          }

          // it's time to fire the callback
          return callback( event );

      }, useCapture || false );
  }

// Autodetect, create and append the renderer to the body element
  window.renderer = PIXI.autoDetectRenderer(400, 300, { backgroundColor: 0x000000, antialias: true });
  $('nav').prepend(renderer.view);

  // Create the main stage for your display objects
  window.stage = new PIXI.Container();


  window.zoom = 1;
  stage.scale.x = zoom;
  stage.scale.y = zoom;

  window.background = new PIXI.Graphics();

  background.beginFill(0x000000);
  background.lineStyle(2, 0xFFFFFF, 1);
  background.drawRect(0, 0, $("canvas").innerWidth() / zoom, $("canvas").innerHeight() / zoom);
  background.endFill();

  // Add the graphics to the stage
  stage.addChild(background);

  window.player = new PIXI.Graphics();
  player.beginFill(0x000000);
  player.drawCircle(0, 0, 5)
  player.endFill();

  window.text = new PIXI.Text('Loading...',{font: '16px Arial', fill: 0x333333, align: 'center', padding: 1});

  var text_left_padding = 5;

  text.scale.x = 1 / zoom;
  text.scale.y = 1 / zoom;
  text.position.x = text_left_padding / zoom;

  window.title = new PIXI.Graphics();

  var title_height = text.height * zoom;

  title.beginFill(0xFFFFFF);
  title.drawRect(0, 0, $("canvas").innerWidth() / zoom, title_height / zoom);
  title.endFill();

  stage.addChild(title);

  stage.addChild(text);

  function doZoom(x, y, isZoomIn) {
    var direction = isZoomIn ? 1 : -1;
    var factor = (1 + direction * 0.05);
    stage.scale.x *= factor;
    stage.scale.y *= factor;

    zoom = stage.scale.x;

    background.width = $("canvas").innerWidth() / zoom;
    background.height = $("canvas").innerHeight() / zoom;
    title.width = $("canvas").innerWidth() / zoom;
    title.height = title_height / zoom;
    text.scale.x = 1 / zoom;
    text.scale.y = 1 / zoom;


    var beforeTransform = renderer.plugins.interaction.mouse.getLocalPosition(stage, {global: { x: x, y: y}});
    renderer.render(stage);
    var afterTransform = renderer.plugins.interaction.mouse.getLocalPosition(stage, {global: { x: x, y: y}});

    var x_diff = afterTransform.x - beforeTransform.x
    var y_diff = afterTransform.y - beforeTransform.y

    stage.position.x += x_diff * stage.scale.x;
    background.position.x = -(stage.position.x / stage.scale.x);
    title.position.x = -(stage.position.x / stage.scale.x);
    text.position.x = -(stage.position.x / stage.scale.x) + (text_left_padding / stage.scale.x);

    stage.position.y += y_diff * stage.scale.y;
    background.position.y = -(stage.position.y / stage.scale.y);
    title.position.y = -(stage.position.y / stage.scale.y);
    text.position.y = -(stage.position.y / stage.scale.y);

    renderer.render(stage);
  }

  addWheelListener($("canvas").get(0), function(e) {
    e.preventDefault();
    doZoom(e.clientX, e.clientY, e.deltaY < 0);
  });

  stage.interactive = true;

  var pointInRoom = function(point, room) {
    var shape = room.shape;
    return ((shape.x <= point.x + 16) && ((shape.x + shape.width) >= point.x - 16)) && ((shape.y <= point.y + 16) && ((shape.y + shape.height) >= point.y - 16));
  }

  var findRoomByCoords = function(point) {
    for (var i = 0; i < highlighted_rooms.length; i++) {
      var room = rooms[highlighted_rooms[i]];
      if (pointInRoom(point, room)) {
        return room;
      } else {
        continue;
      }
    }

    for (var room_id in rooms) {
      var room = rooms[room_id];
      if (pointInRoom(point, room)) {
        return room;
      } else {
        continue;
      }
    }
  }

  var mouseDown = false;
  var pointInTitle = false;
  var prevX, prevY;

  var mouseOverTitle = function(point) {
    return ((title.position.x <= point.x) && ((title.position.x + title.width) >= point.x)) && ((title.position.y <= point.y) && ((title.position.y + title.height) >= point.y));
  }

  var onDragStart = function(event) {
    var local = renderer.plugins.interaction.mouse.getLocalPosition(stage, {global: { x: event.clientX, y: event.clientY}});

    prevX = event.clientX; prevY = event.clientY;
    mouseDown = true;
    pointInTitle = mouseOverTitle(local);
  }

  var onDragEnd = function(event) {
    mouseDown = false;
    pointInTitle = false;
  }

  var interactEvent;

  $("canvas").mousemove(function(event) {
    onDragMove(event);
  }).mousedown(function(event) {
    onDragStart(event);
  }).mouseleave(function(event) {
    highlight_area(player.room);
  });

  $(document).mouseup(function(event) {
    onDragEnd(event);
  })

  var onDragMove = function(event) {

    var local = renderer.plugins.interaction.mouse.getLocalPosition(stage, {global: { x: event.clientX, y: event.clientY}});

    if (!mouseDown) {
      // var pos = event.data.global;
      // var local = renderer.plugins.interaction.mouse.getLocalPosition(stage, {global: pos});
      var room = findRoomByCoords(local)

      if (room) {
        highlight_area(room);
        text.text = room.area + " - " + room.id + " - " + room.name;
      }
      return;
    }

    if (pointInTitle && interactEvent) {
      var target = interactEvent.target,
          // keep the dragged position in the data-x/data-y attributes
          x = (parseFloat(target.getAttribute('data-x')) || 0) + interactEvent.dx,
          y = (parseFloat(target.getAttribute('data-y')) || 0) + interactEvent.dy;

      // translate the element
      target.style.webkitTransform =
      target.style.transform =
        'translate(' + x + 'px, ' + y + 'px)';

      // update the posiion attributes
      target.setAttribute('data-x', x);
      target.setAttribute('data-y', y);
      return;
    } else {
      var pos = { x: event.clientX, y: event.clientY };
      var dx = pos.x - prevX;
      var dy = pos.y - prevY;

      stage.position.x += dx;
      stage.position.y += dy;
      background.position.x = -(stage.position.x / stage.scale.x);
      background.position.y = -(stage.position.y / stage.scale.y);
      title.position.x = -(stage.position.x / stage.scale.x);
      title.position.y = -(stage.position.y / stage.scale.y);
      text.position.x = -(stage.position.x / stage.scale.x) + (text_left_padding / stage.scale.x);
      text.position.y = -(stage.position.y / stage.scale.y);
      prevX = pos.x; prevY = pos.y;
    }
  }

  window.center_on_room = function(room_id) {
    if (rooms[room_id]) {
      player.room = rooms[room_id];
      highlight_area(rooms[room_id]);
      var pos = rooms[room_id].shape;
      stage.position.x = (-((pos.x * stage.scale.x) - (($("canvas").innerWidth()) / 2) + ((pos.width * stage.scale.x) / 2)));
      stage.position.y = (-((pos.y * stage.scale.y) - (($("canvas").innerHeight()) / 2) + ((pos.height * stage.scale.y) / 2)));

      background.position.x = -(stage.position.x / stage.scale.x);
      background.position.y = -(stage.position.y / stage.scale.y);
      title.position.x = -(stage.position.x / stage.scale.x);
      title.position.y = -(stage.position.y / stage.scale.y);
      text.position.x = -(stage.position.x / stage.scale.x) + (text_left_padding / stage.scale.x);
      text.position.y = -(stage.position.y / stage.scale.y);
      player.position.x = pos.x + 8;
      player.position.y = pos.y + 8;
      stage.addChild(player);
    }
  }

  window.draw_map = function(room_id) {
    for (var area_name in areas) {
      draw_area(area_name);
    };
    center_on_room(room_id);
  }

  // Start animating
  animate();
  function animate() {
    //Render the stage
    renderer.render(stage);
    requestAnimationFrame(animate);
  }

  var socket = new Socket("" + (window.location.origin.replace('http', 'ws')) + "/ws");
  socket.connect();
  var chan = socket.channel("map", {});

  chan.join()

  window.rooms = {};
  window.areas = {};

  var add_room = function(room_id, room_data) {
    if (room_data.coords) {
      rooms[room_id] = room_data
      areas[room_data.area] = areas[room_data.area] || {
        map: new PIXI.Graphics(),
        rooms: {}
      }
      areas[room_data.area].rooms[room_id] = room_data
    }
  }

  var highlighted_area;
  var highlighted_z;
  var highlighted_rooms = [];

  var highlight_area = function(room) {
    if (!room) {
      return;
    }
    if (highlighted_area != room.area || highlighted_z != room.coords.z || highlighted_rooms.indexOf("" + room.id + "") === -1) {
      var old_highlighted_area = highlighted_area;
      highlighted_z = room.coords.z;

      if (old_highlighted_area && old_highlighted_area != room.highlighted_area) {
        draw_area(old_highlighted_area);
      }

      highlighted_area = room.area;

      stage.removeChild(areas[highlighted_area].map)
      draw_area(highlighted_area, room.coords.z);
    }
    text.text = room.area;
    stage.addChild(player);
    // puts them at the beginning of the children array
    // which draws them above other areas on the map
  }

  var draw_area = function(area, z) {
    var map = areas[area].map;

    stage.addChild(map);
    stage.removeChild(title);
    stage.addChild(title);
    stage.addChild(text);
    map.clear();
    highlighted_rooms = [];
    var unhighlighted_rooms = [];

    for (var room_id in areas[area].rooms) {
      var room = rooms[room_id];

      if (highlighted_area == room.area && z == room.coords.z) {
        highlighted_rooms.push(room_id);
      } else {
        unhighlighted_rooms.push(room_id);
      }
    }

    for (var i = 0; i < unhighlighted_rooms.length; i++) {
      draw_room(map, unhighlighted_rooms[i], false);
    }

    for (var i = 0; i < highlighted_rooms.length; i++) {
      draw_room(map, highlighted_rooms[i], true);
    }

  };

  window.room_power = function(room, level) {
    var base = Math.floor(75 * (1 + (room.level / 10))) * 6;

    return (base + ((base / 10) * (Math.max(room.level, level) - 1))) / 10;
  }

  var draw_room = function(map, room_id, highlighted) {
    var room = rooms[room_id];

    var x = (room.coords.x * 32) + 2650
    var y = (room.coords.y * 32) + 7600

    var start_x;
    var start_y;
    var end_x;
    var end_y;

    var roompower = room_power(room, player.level);

    if (player.room && (player.room.id === room_id)) {
      console.log("room power: " + roompower + ", room level: " + room.level + ", player level: " + player.level);
    }

    if (highlighted) {
      map.lineStyle(2, 0xFFFFFF, 1);

      if (roompower < (player.power * 0.66)) {
        map.beginFill(0x008080);
      }
      else if (roompower < (player.power * 1.33)) {
        map.beginFill(0x7fff00);
      }
      else if (roompower < (player.power * 1.66)) {
        map.beginFill(0x0000ff);
      }
      else if (roompower < (player.power * 2.0)) {
        map.beginFill(0x8b008b);
      }
      else {
        map.beginFill(0xFF0000);
      }
    }
    else {
      map.lineStyle(2, 0x666666, 1);

      if (roompower < (player.power * 0.66)) {
        map.beginFill(0x002020);
      }
      else if (roompower < (player.power * 1.33)) {
        map.beginFill(0x006400);
      }
      else if (roompower < (player.power * 1.66)) {
        map.beginFill(0x00008b);
      }
      else if (roompower < (player.power * 2.0)) {
        map.beginFill(0x240024);
      }
      else {
        map.beginFill(0x8b0000);
      }
    }

    map.drawRect(x, y, 16, 16);

    map.endFill();

    var rect = map.graphicsData[map.graphicsData.length - 1].shape;
    rooms[room_id].shape = rect;

    room.directions.forEach(function(direction) {
      switch (direction) {
        case 'north':
          start_x = x + 8;
          start_y = y;
          end_x   = x + 8;
          end_y   = y + 8 - 16;
          break;
        case 'northeast':
          start_x = x + 16;
          start_y = y;
          end_x   = x + 8 + 16;
          end_y   = y + 8 - 16;
          break;
        case 'east':
          start_x = x + 16;
          start_y = y + 8;
          end_x   = x + 8 + 16;
          end_y   = y + 8;
          break;
        case 'southeast':
          start_x = x + 16;
          start_y = y + 16;
          end_x   = x + 8 + 16;
          end_y   = y + 8 + 16;
          break;
        case 'south':
          start_x = x + 8;
          start_y = y + 16;
          end_x   = x + 8;
          end_y   = y + 8 + 16;
          break;
        case 'southwest':
          start_x = x;
          start_y = y + 16;
          end_x   = x + 8 - 16;
          end_y   = y + 8 + 16;
          break;
        case 'west':
          start_x = x;
          start_y = y + 8;
          end_x   = x + 8 - 16;
          end_y   = y + 8;
          break;
        case 'northwest':
          start_x = x;
          start_y = y;
          end_x   = x + 8 - 16;
          end_y   = y + 8 - 16;
          break;
      }

      map.moveTo(start_x, start_y);
      map.lineTo(end_x, end_y);
    });
  }

  chan.on("update_map", function(area){
    text.text = "Apotheosis";
    for (var area_name in area) {
      for (var room_id in area[area_name]) {
        add_room(parseInt(room_id), area[area_name][room_id]);
      }
    }
  });

  chan.on("request_room_id", function() {
    push("map", "request_room_id");
  });

  chan.on("area_change", function(data) {
    var room = areas[data.old_area].rooms[data.room_id];
    room.area = data.new_area;
    delete areas[data.old_area].rooms[data.room_id];
    add_room(data.room_id, room);

    center_on_room(player.room.id);
  });

  chan.on("room name change", function(data) {
    var room = rooms[data.room_id];
    room.name = data.name;
    add_room(data.room_id, room);

    center_on_room(player.room.id);
  });

  chan.on("room coords change", function(data) {
    var room = rooms[data.room_id];
    room.coords = {x: data.x, y: data.y, z: data.z};
    add_room(data.room_id, room);

    draw_area(room.area);
    draw_area(player.room.area);
    center_on_room(player.room.id);
  });

  chan.on("room control change", function(data) {
    var room = rooms[data.room_id];

    if (room) {
      room.controlled_by = data.controlled_by;
      add_room(data.room_id, room);

      draw_area(room.area);
      draw_area(player.room.area);
      center_on_room(player.room.id);
    } else {
      console.log("could not find room: " + data.room_id);
      console.log(data);
    }
  });

  chan.on("room admin updated", function(data) {
    var room = rooms[data.id];
    room.name = data.name;
    room.coords = data.coords;
    room.directions = data.directions;

    add_room(room.id, room);

    draw_area(room.area);
    draw_area(player.room.area);
    center_on_room(player.room.id);
  });

  function dragMoveListener (event) {
    interactEvent = event;
    return;
  }

  interact('canvas')
    .draggable({
      onmove: dragMoveListener
    })
    .resizable({
      preserveAspectRatio: false,
      edges: { left: true, right: true, bottom: true, top: true }
    })
    .on('resizemove', function (event) {
      var target = event.target,
          x = (parseFloat(target.getAttribute('data-x')) || 0),
          y = (parseFloat(target.getAttribute('data-y')) || 0);

      // update the element's style
      target.style.width  = event.rect.width + 'px';
      target.style.height = event.rect.height + 'px';

      // translate when resizing from top or left edges
      x += event.deltaRect.left;
      y += event.deltaRect.top;

      target.style.webkitTransform = target.style.transform =
          'translate(' + x + 'px,' + y + 'px)';

      target.setAttribute('data-x', x);
      target.setAttribute('data-y', y);

      var w = $("canvas").innerWidth();
      var h = $("canvas").innerHeight();

      //this part resizes the canvas but keeps ratio the same
      renderer.view.style.width = w + "px";
      renderer.view.style.height = h + "px";

      background.width = w / zoom;
      background.height = h / zoom;
      title.width = w / zoom;
      title.height = title_height / zoom;

      //this part adjusts the ratio:
      renderer.resize(w,h);
    });


});
