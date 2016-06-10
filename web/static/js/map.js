import {Socket} from "deps/phoenix/web/static/js/phoenix"

$(document).ready(function() {

// Autodetect, create and append the renderer to the body element
  var renderer = PIXI.autoDetectRenderer(1200, 800, { backgroundColor: 0x000000, antialias: true });
  document.body.appendChild(renderer.view);

  // Create the main stage for your display objects
  var stage = new PIXI.Container();

  stage.interactive = true;

  var dragging = false;
  var position;
  var mouse_data;

  var onDragStart = function(event) {
    dragging = true;
    mouse_data = event.data;
    position = mouse_data.getLocalPosition(stage);
  }

  var onDragEnd = function(event) {
    dragging = false;
    mouse_data = null;
  }

  var onDragMove = function(event) {
    if (dragging) {
      var newPosition = mouse_data.getLocalPosition(stage);
      var x_diff = newPosition.x - position.x;
      var y_diff = newPosition.y - position.y;
      stage.x = stage.x + (x_diff * zoom);
      stage.y = stage.y + (y_diff * zoom);
      position = newPosition;
    }
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

  // Initialize the pixi Graphics class
  var graphics = new PIXI.Graphics();

  // Add the graphics to the stage
  stage.addChild(graphics);

  graphics.beginFill(0x000000);
  // graphics.lineStyle(0, 0xFFFFFF, 1);
  graphics.drawRect(0, 0, 1200 / zoom, 800 / zoom);
  graphics.endFill();

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

  var draw_room = function(room) {
    if (room.coords) {
      // Set a new fill color
      graphics.beginFill(0x000000); // Blue

      graphics.lineStyle(2, 0xFFFFFF, 1);

      // Draw a rectangle
       // drawRect(x, y, width, height)
      var x = (room.coords.x * 32) + 2650
      var y = (room.coords.y * 32) + 7600

      var start_x;
      var start_y;
      var end_x;
      var end_y;

      graphics.drawRect(x, y, 16, 16);

      graphics.endFill();

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

        graphics.moveTo(start_x, start_y);
        graphics.lineTo(end_x, end_y);
      });

    }
  }

  chan.on("update_room", function(room){
    draw_room(room)
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
