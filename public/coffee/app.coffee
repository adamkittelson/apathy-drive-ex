$ ->
  webSocket = new WebSocket('ws://localhost:3000/_ws')

  webSocket.onopen = (event) ->
    console.log "Connected!"

  webSocket.onmessage = (event) ->
    console.log "Received message #{event.data}"
    addToScroll("#scroll", "<p>Message Received: #{event.data}</p>")

  webSocket.onclose = (event) ->
    console.log "Connection closed!"

  addToScroll = (elem, text) ->
    $(elem).append(text)
    $('#scroll').scrollTop($('#scroll')[0].scrollHeight)

  $('#command').on 'keyup', (event) ->
    if event.which is 13 # enter key
      command = $(event.target).val()
      $(event.target).select()
      addToScroll('#scroll', "<p>Message Sent: #{command}</p>")
      webSocket.send JSON.stringify({ command: { text: command }})
