$ ->
  adjustScrollTop = ->
    $("#scroll_container").css("top", $("#room").height() + 10 + "px")

  adjustScrollTop()

  webSocket = new WebSocket('ws://localhost:3000/_ws')

  webSocket.onopen = (event) ->
    console.log "Connected!"

  webSocket.onmessage = (event) ->
    console.log "Received message #{event.data}"
    addToScroll("#scroll", "<div>Message Received: #{event.data}</div>")

  webSocket.onclose = (event) ->
    console.log "Connection closed!"

  addToScroll = (elem, text) ->
    $(elem).append(text)
    $('#scroll').scrollTop($('#scroll')[0].scrollHeight)

  $('#command').on 'keyup', (event) ->
    if event.which is 13 # enter key
      command = $(event.target).val()
      $(event.target).select()
      addToScroll('#scroll', "<div>Message Sent: #{command}</div>")
      webSocket.send JSON.stringify({ command: { text: command }})
