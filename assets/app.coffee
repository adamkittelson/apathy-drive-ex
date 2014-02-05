$ ->

  focus = null

  $('body').on 'click', (event) ->
    setFocus(focus)

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

  setFocus = (selector) ->
    focus = selector
    $(selector).focus().select()

  adjustScrollTop()

  webSocket = new WebSocket('ws://localhost:3000/_ws')

  webSocket.onopen = (event) ->
    console.log "Connected!"

  webSocket.onmessage = (event) ->
    message = JSON.parse(event.data)
    switch message[0]
      when "room" then updateRoom(message[1])
      when "clear scroll" then clearScroll()
      when "focus" then setFocus(message[1])
      when "disable" then disableField(message[1])
      else addToScroll("#scroll", message[1])

  webSocket.onclose = (event) ->
    console.log "Connection closed!"

  addToScroll = (elem, text) ->
    $(elem).append(text)
    $('#scroll').scrollTop($('#scroll')[0].scrollHeight)

  focusNext = (elem) ->
    fields = $("#scroll").find('input:not([disabled])')
    field = fields.eq(fields.index(elem) + 1)[0]
    if field
      setFocus("##{field.id}")

  focusPrevious = (elem) ->
    fields = $("#scroll").find(':input')
    field = fields.eq(fields.index(elem) - 1)[0]
    if field
      setFocus("##{field.id}")

  disableField = (selector) ->
    $(selector).prop('disabled', true).removeAttr('id')

  $(document).on 'keyup', "input", (event) ->
    event.preventDefault()
    if event.which is 13 # enter key
      command = $(event.target).val()
      if event.target.id is "command"
        addToScroll('#scroll', "<p><span class='dark-yellow'>#{command}</span></p>")
        $(event.target).val("")
      else
        focusNext($(event.target))
      params = {}
      params[event.target.id] = command
      webSocket.send JSON.stringify(params)
    else if event.which is 38
      focusPrevious($(event.target))
    else if event.which is 40
      focusNext($(event.target))
