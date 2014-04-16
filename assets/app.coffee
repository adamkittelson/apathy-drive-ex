$ ->

  focus = null

  $('body').on 'click', (event) ->
    setFocus(focus)

  updateRoom = (data) ->
    console.log(data)
    $('#room .title').html(data['name'])
    $('#room .description').html(data['description'])
    if data['entities'].length > 0
      $('#room .entities').html("<span class='dark-magenta'>Also here:</span> <span class='magenta'>#{data['entities'].join(', ')}</span><span class='dark-magenta'>.</span>")
    else
      $('#room .entities').html("")
    $('#room .exits').html("Obvious exits: #{data['exits'].join(', ') || 'NONE'}")
    adjustScrollTop()

  clearScroll = ->
    $('#scroll').html("")

  adjustScrollTop = ->
    $("#scroll_container").css("top", $("#room").height() + 10 + "px")
    $('#scroll').scrollTop($('#scroll')[0].scrollHeight)

  setFocus = (selector) ->
    focus = selector
    $(selector).focus()

  adjustScrollTop()

  webSocket = new WebSocket('ws://localhost:3000/_ws')

  webSocket.onopen = (event) ->
    console.log "Connected!"

  webSocket.onmessage = (event) ->
    message = JSON.parse(event.data)
    switch message[0]
      when "room" then updateRoom(message[1])
      when "clear scroll" then clearScroll()
      when "focus" then setFocus(message[1]).select()
      when "disable" then disableField(message[1])
      when "update" then $(message[1]).html(message[2])
      when "set field" then $(message[1]).val(message[2])
      else addToScroll("#scroll", message[1])

  webSocket.onclose = (event) ->
    addToScroll("#scroll}", "<p>Disconnected from server.</p>")
    console.log "Connection closed!"

  addToScroll = (elem, text) ->
    $(elem).append(text)
    $(elem).append($("#prompt").parent().detach())
    setFocus(focus)
    $('#scroll').scrollTop($('#scroll')[0].scrollHeight)

  focusNext = (elem) ->
    fields = $("#scroll").find('input:not([disabled])')
    field = fields.eq(fields.index(elem) + 1)[0]
    if field
      setFocus("##{field.id}").select()

  focusPrevious = (elem) ->
    fields = $("#scroll").find(':input')
    field = fields.eq(fields.index(elem) - 1)[0]
    if field
      setFocus("##{field.id}").select()

  disableField = (selector) ->
    $(selector).prop('disabled', true).removeAttr('id')

  $(document).on 'keyup', "input", (event) ->
    event.preventDefault()
    if event.which is 13 # enter key
      command = $(event.target).val()
      unless event.target.id is "command"
        $("#validation").html("")
        focusNext($(event.target))
      params = {}
      params[event.target.id] = command
      webSocket.send JSON.stringify(params)
    else if event.which is 38
      value = parseInt($(event.target).val())
      unless isNaN value
        $("#validation").html("")
        $(event.target).val(value + 1)
        params = {}
        params[event.target.id] = "#{value + 1}"
        webSocket.send JSON.stringify(params)
    else if event.which is 40
      value = parseInt($(event.target).val())
      unless isNaN value
        $("#validation").html("")
        $(event.target).val(value - 1)
        params = {}
        params[event.target.id] = "#{value - 1}"
        webSocket.send JSON.stringify(params)
    else if event.which is 32
      params = {}
      params["cycle"] = event.target.id
      webSocket.send JSON.stringify(params)
