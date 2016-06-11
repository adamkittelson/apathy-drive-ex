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
  var renderer = PIXI.autoDetectRenderer(1200, 800, { backgroundColor: 0x000000, antialias: true });
  document.body.appendChild(renderer.view);

  // Create the main stage for your display objects
  var stage = new PIXI.Container();

  function doZoom(x, y, isZoomIn) {
    var direction = isZoomIn ? 1 : -1;
    var factor = (1 + direction * 0.05);
    stage.scale.x *= factor;
    stage.scale.y *= factor;

    var beforeTransform = renderer.plugins.interaction.mouse.getLocalPosition(stage, {global: { x: x, y: y}});
    renderer.render(stage);
    var afterTransform = renderer.plugins.interaction.mouse.getLocalPosition(stage, {global: { x: x, y: y}});

    var x_diff = afterTransform.x - beforeTransform.x
    var y_diff = afterTransform.y - beforeTransform.y

    stage.position.x += x_diff * stage.scale.x;
    stage.position.y += y_diff * stage.scale.y;
    renderer.render(stage);
  }

  addWheelListener($("canvas").get(0), function(e) {
    doZoom(e.clientX, e.clientY, e.deltaY < 0);
  });

  stage.interactive = true;

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
      return;
    }

    var pos = event.data.global;
    var dx = pos.x - prevX;
    var dy = pos.y - prevY;

    stage.position.x += dx;
    stage.position.y += dy;
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


  var zoom = 0.1;
  stage.scale.x = zoom;
  stage.scale.y = zoom;

  var background = new PIXI.Graphics();

  background.beginFill(0x000000);
  background.drawRect(0, 0, 1200 / zoom, 800 / zoom);
  background.endFill();

  // Add the graphics to the stage
  stage.addChild(background);



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

  var map = {};

  var createArea = function(name) {
    var area = new PIXI.Graphics();
    // area.interactive = true;
    area.name = name;
    area.rooms = {};
    // area.on('mouseover', onAreaOver)
    stage.addChild(area);
    return area;
  }

  var add_room = function(room) {
    if (room.coords) {
      map[room.area] = map[room.area] || createArea(room.area)
      var area = map[room.area]
      area["rooms"][room.id] = room
    }
  }

  window.highlight_area = function(area_name) {
    if (map[area_name]) {
      draw_area(map[area_name], true)
    }
  }

  var draw_area = function(area, highlight) {
    area.clear();

    if (highlight) {
      area.lineStyle(2, 0x0000FF, 1);
    } else {
      area.lineStyle(2, 0xFFFFFF, 1);
    }

    $.each(area.rooms, function(room_id, room) {
      var x = (room.coords.x * 32) + 2650
      var y = (room.coords.y * 32) + 7600

      var start_x;
      var start_y;
      var end_x;
      var end_y;

      if (room.controlled_by == "good") {
        area.beginFill(0xFFFFFF);
      } else if (room.controlled_by == "evil") {
        area.beginFill(0xFF00FF);
      } else {
        area.beginFill(0x008080);
      }

      area.drawRect(x, y, 16, 16);

      area.endFill();

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

        area.moveTo(start_x, start_y);
        area.lineTo(end_x, end_y);
      });
    });
  }

  chan.on("update_room", function(room){
    add_room(room)
    draw_area(map[room.area])
  });

  $(document).on('keyup', function(event) {
    event.preventDefault();
    if (event.which === 187) {
      zoom = zoom + 0.01;
      stage.scale.x = zoom;
      stage.scale.y = zoom;
    } else if (event.which === 189) {
      zoom = zoom - 0.01;
      stage.scale.x = zoom;
      stage.scale.y = zoom;
    } else if (event.which === 38) {
      stage.y = stage.y + 100;
    } else if (event.which === 39) {
      stage.x = stage.x - 100;
    } else if (event.which === 40) {
      stage.y = stage.y - 100;
    } else if (event.which === 37) {
      stage.x = stage.x + 100;
    }
  });

});
