$ ->

  $('body').on 'click', (event) ->
    $('#command').focus()

  $('#command').focus()

  updateRoom = (data) ->
    $('#room .title').html(data['name'])
    $('#room .description').html(data['description'])

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
      else addToScroll("#scroll", "<div>Message Received: #{message[1]}</div>")

  webSocket.onclose = (event) ->
    console.log "Connection closed!"

  addToScroll = (elem, text) ->
    $(elem).append(text)
    $('#scroll').scrollTop($('#scroll')[0].scrollHeight)

  $('#command').on 'keyup', (event) ->
    if event.which is 13 # enter key
      command = $(event.target).val()
      $(event.target).val("")
      addToScroll('#scroll', "<div>Message Sent: #{command}</div>")
      webSocket.send JSON.stringify({ command: { text: command }})
