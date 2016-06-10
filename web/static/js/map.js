import {Socket} from "deps/phoenix/web/static/js/phoenix"

$(document).ready(function() {

// Autodetect, create and append the renderer to the body element
  var renderer = PIXI.autoDetectRenderer(1200, 800, { backgroundColor: 0x000000, antialias: true });
  document.body.appendChild(renderer.view);

  // Create the main stage for your display objects
  var stage = new PIXI.Container();

  var zoom = 0.1;
  stage.scale.x = zoom;
  stage.scale.y = zoom;

  // Initialize the pixi Graphics class
  var graphics = new PIXI.Graphics();
  
  // Add the graphics to the stage
  stage.addChild(graphics);

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

      graphics.endFill();

    }
  }

  chan.on("update_room", function(room){
    console.log(room);
    draw_room(room)
  });

  chan.on("presence_diff", function(message){
    for (var key in message.joins) {
      message.joins[key].metas.forEach(function(room) {
        draw_room(room)
      });
    }
  });

  chan.on("full_map", function(world){
    for (var room_id in world) {
      draw_room(world[room_id])
    }
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
