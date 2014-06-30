$ ->

  focus = null

  $('body').on 'click', (event) ->
    setFocus(focus)

  updateRoom = (data) ->
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

  webSocket = new WebSocket("#{window.location.origin.replace('http', 'ws:')}/_ws")

  webSocket.onopen = (event) ->
    pathparts = window.location.pathname.split("/")
    url = pathparts[pathparts.length - 1]
    webSocket.send JSON.stringify({login: url})

  webSocket.onmessage = (event) ->
    message = JSON.parse(event.data)
    switch message[0]
      when "room" then updateRoom(message[1])
      when "clear scroll" then clearScroll()
      when "focus" then setFocus(message[1]).select()
      when "disable" then disableField(message[1])
      when "update" then $(message[1]).html(message[2])
      when "set field" then $(message[1]).val(message[2])
      when "update prompt" then $("#prompt").text(message[1])
      when "redirect" then window.location = "#{window.location.origin}#{message[1]}"
      when "up" then command_history("up")
      else addToScroll("#scroll", message[1])

  webSocket.onclose = (event) ->
    $("#scroll").append("<p>Disconnected from server.</p>")
    disableField("#command")
    $('#scroll').scrollTop($('#scroll')[0].scrollHeight)

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

  history_marker = null

  command_history = (direction) ->
    history = $('.prompt:disabled')
    return if history.length == 0
    if history_marker == null
      history_marker = history.length

    if direction == "up"
      history_marker = Math.max(0, history_marker - 1)
    else if direction == "down"
      history_marker = Math.min(history.length - 1, history_marker + 1)

    $("#command").val(history[history_marker].value)
    setFocus("#command").select()

  $(document).on 'keydown', "input", (event) ->
    if event.which is 9 and !event.shiftKey # prevent tab, but not shift-tab
      event.preventDefault()

  $(document).on 'keyup', "input", (event) ->
    event.preventDefault()
    if event.which is 13 or (event.which is 9 and !event.shiftKey) # enter key or (non-shift) tab
      history_marker = null
      command = $(event.target).val()
      params = {}
      params[event.target.id] = command
      webSocket.send JSON.stringify(params)
    else if event.which is 38
      command_history("up")
    else if event.which is 40
      command_history("down")
