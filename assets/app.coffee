$ ->

  $('body').on 'click', (event) ->
    $('#command').focus()

  $('#command').focus()

  updateRoom = (data) ->
    console.log(data)
    $('#room .title').html(data['name'])
    $('#room .description').html(data['description'])
    $('#room .exits').html("Obvious exits: #{data['exits'].join(', ') || 'NONE'}")
    adjustScrollTop()

  clearScroll = ->
    $('#scroll').html("")

  adjustScrollTop = ->
    $("#scroll_container").css("top", $("#room").height() + 10 + "px")

  adjustScrollTop()

  webSocket = new WebSocket('ws://localhost:3000/_ws')

  webSocket.onopen = (event) ->
    console.log "Connected!"

  webSocket.onmessage = (event) ->
    message = JSON.parse(event.data)
    switch message[0]
      when "room" then updateRoom(message[1])
      when "clear scroll" then clearScroll()
      else addToScroll("#scroll", message[1])

  webSocket.onclose = (event) ->
    console.log "Connection closed!"

  addToScroll = (elem, text) ->
    $(elem).append(text)
    $('#scroll').scrollTop($('#scroll')[0].scrollHeight)

  $('#command').on 'keyup', (event) ->
    if event.which is 13 # enter key
      command = $(event.target).val()
      $(event.target).val("")
      addToScroll('#scroll', "<p><span class='dark-yellow'>#{command}</span></p>")
      webSocket.send JSON.stringify({ command: command })
