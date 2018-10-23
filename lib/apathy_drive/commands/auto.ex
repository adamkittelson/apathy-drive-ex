defmodule ApathyDrive.Commands.Auto do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile}

  def keywords, do: ["auto"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(
      character,
      "<p><span class='white'>You have the following automation settings:</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-magenta'>Setting   Enabled?</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-cyan'>Heal        #{emoji(character.auto_heal)}</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-cyan'>Flee        #{emoji(character.auto_flee)}</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-cyan'>Bless       #{emoji(character.auto_bless)}</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-cyan'>Curse       #{emoji(character.auto_curse)}</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-cyan'>Nuke        #{emoji(character.auto_nuke)}</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-cyan'>Roam        #{emoji(character.auto_roam)}</span></p>"
    )

    room
  end

  def execute(%Room{} = room, %Character{ref: ref}, ["heal"]) do
    room =
      Room.update_mobile(room, ref, fn character ->
        Map.put(character, :auto_heal, !character.auto_heal)
      end)

    execute(room, room.mobiles[ref], [])
  end

  def execute(%Room{} = room, %Character{ref: ref}, ["flee"]) do
    room =
      Room.update_mobile(room, ref, fn character ->
        Map.put(character, :auto_flee, !character.auto_flee)
      end)

    execute(room, room.mobiles[ref], [])
  end

  def execute(%Room{} = room, %Character{ref: ref}, ["bless"]) do
    room =
      Room.update_mobile(room, ref, fn character ->
        Map.put(character, :auto_bless, !character.auto_bless)
      end)

    execute(room, room.mobiles[ref], [])
  end

  def execute(%Room{} = room, %Character{ref: ref}, ["curse"]) do
    room =
      Room.update_mobile(room, ref, fn character ->
        Map.put(character, :auto_curse, !character.auto_curse)
      end)

    execute(room, room.mobiles[ref], [])
  end

  def execute(%Room{} = room, %Character{ref: ref}, ["nuke"]) do
    room =
      Room.update_mobile(room, ref, fn character ->
        Map.put(character, :auto_nuke, !character.auto_nuke)
      end)

    execute(room, room.mobiles[ref], [])
  end

  def execute(%Room{} = room, %Character{ref: ref}, ["roam"]) do
    room =
      Room.update_mobile(room, ref, fn character ->
        Map.put(character, :auto_roam, !character.auto_roam)
      end)

    execute(room, room.mobiles[ref], [])
  end

  def execute(%Room{} = room, %Character{} = character, _args) do
    Mobile.send_scroll(character, "<p>What?</p>")
    room
  end

  def emoji(true), do: "✅"
  def emoji(false), do: "❌"
end
