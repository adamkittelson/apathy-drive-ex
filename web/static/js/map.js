import {Socket} from "deps/phoenix/web/static/js/phoenix"

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
  window.renderer = PIXI.autoDetectRenderer(window.innerWidth, window.innerHeight, { backgroundColor: 0x000000, antialias: true });
  document.body.appendChild(renderer.view);

  // Create the main stage for your display objects
  window.stage = new PIXI.Container();


  window.zoom = 0.1;
  stage.scale.x = zoom;
  stage.scale.y = zoom;

  window.background = new PIXI.Graphics();

  background.beginFill(0x000000);
  background.drawRect(0, 0, window.innerWidth / zoom, window.innerHeight / zoom);
  background.endFill();

  // Add the graphics to the stage
  stage.addChild(background);


  window.onresize = function(event) {
    var w = window.innerWidth;
    var h = window.innerHeight;

    //this part resizes the canvas but keeps ratio the same
    renderer.view.style.width = w + "px";
    renderer.view.style.height = h + "px";

    background.width = w / zoom;
    background.height = h / zoom;

    //this part adjusts the ratio:
    renderer.resize(w,h);
  }

  function doZoom(x, y, isZoomIn) {
    var direction = isZoomIn ? 1 : -1;
    var factor = (1 + direction * 0.05);
    stage.scale.x *= factor;
    stage.scale.y *= factor;

    zoom = stage.scale.x;
    
    background.width = window.innerWidth / zoom;
    background.height = window.innerHeight / zoom;

    var beforeTransform = renderer.plugins.interaction.mouse.getLocalPosition(stage, {global: { x: x, y: y}});
    renderer.render(stage);
    var afterTransform = renderer.plugins.interaction.mouse.getLocalPosition(stage, {global: { x: x, y: y}});

    var x_diff = afterTransform.x - beforeTransform.x
    var y_diff = afterTransform.y - beforeTransform.y

    stage.position.x += x_diff * stage.scale.x;
    background.position.x = -(stage.position.x / stage.scale.x)
    stage.position.y += y_diff * stage.scale.y;
    background.position.y = -(stage.position.y / stage.scale.y)
    renderer.render(stage);
  }

  addWheelListener($("canvas").get(0), function(e) {
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

  var dragging = false;
  var prevX, prevY;

  var onDragStart = function(event) {
    var pos = event.data.global;

    prevX = pos.x; prevY = pos.y;
    dragging = true;
  }

  var onDragEnd = function(event) {
    dragging = false;
  }

  var onDragMove = function(event) {
    if (!dragging) {
      var local = renderer.plugins.interaction.mouse.getLocalPosition(stage, {global: pos});

      var room = findRoomByCoords(local)

      if (room) {
        highlight_area(room);
      }
      return;
    }

    var pos = event.data.global;
    var dx = pos.x - prevX;
    var dy = pos.y - prevY;

    stage.position.x += dx;
    stage.position.y += dy;
    background.position.x = -(stage.position.x / stage.scale.x)
    background.position.y = -(stage.position.y / stage.scale.y)
    prevX = pos.x; prevY = pos.y;
  }

  stage
  .on('mousedown',  onDragStart)
  .on('touchstart', onDragStart)
  .on('mousemove',  onDragMove)
  .on('touchmove',  onDragMove)
  .on('mouseup', onDragEnd)
  .on('mouseupoutside', onDragEnd)
  .on('touchend', onDragEnd)
  .on('touchendoutside', onDragEnd);

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
  var highlighted_rooms = [];

  var highlight_area = function(room) {
    if (highlighted_area != room.area) {
      var old_highlighted_area = highlighted_area;
      highlighted_area = room.area;

      if (old_highlighted_area) {
        draw_area(old_highlighted_area);
      }

      stage.removeChild(areas[highlighted_area].map)
      draw_area(highlighted_area);
    }
    $("#info").text(room.area + ", " + room.name);
    // puts them at the beginning of the children array
    // which draws them above other areas on the map
  }

  var draw_map = function() {
    for (var area in areas) {
      draw_area(area);
    }
  }

  var draw_area = function(area) {
    var map = areas[area].map;

    stage.addChild(map);
    map.clear();
    highlighted_rooms = [];

    for (var room_id in areas[area].rooms) {

      var room = rooms[room_id]
      var x = (room.coords.x * 32) + 2650
      var y = (room.coords.y * 32) + 7600

      var start_x;
      var start_y;
      var end_x;
      var end_y;

      if (room.controlled_by == "good") {
        map.beginFill(0xFFFFFF);
      } else if (room.controlled_by == "evil") {
        map.beginFill(0xFF00FF);
      } else {
        map.beginFill(0x008080);
      }

      if (highlighted_area == room.area) {
        highlighted_rooms.push(room_id)
        map.lineStyle(2, 0x0000FF, 1);
      } else {
        map.lineStyle(2, 0xFFFFFF, 1);
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
    };
  };

  chan.on("update_map", function(rooms){
    $("#info").text("Apotheosis");
    for (var room_id in rooms) {
      add_room(parseInt(room_id), rooms[room_id]);
    }
    draw_map();
  });

});
