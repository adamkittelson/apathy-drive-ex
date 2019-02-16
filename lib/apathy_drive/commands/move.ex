defmodule ApathyDrive.Commands.Move do
  alias ApathyDrive.{Character, Currency, Companion, Doors, Mobile, Repo, Room, RoomServer}
  require Logger

  def execute(%Room{} = room, %{} = character, command) when is_binary(command) do
    direction = Room.direction(command)
    room_exit = Room.get_exit(room, direction)

    execute(room, character, room_exit)
  end

  def execute(%Room{} = room, %{} = mob, %{"kind" => kind} = re) when kind in ["Door", "Gate"] do
    if Doors.open?(room, re) do
      execute(room, mob, Map.put(re, "kind", "Normal"))
    else
      Mobile.send_scroll(
        mob,
        "<p><span class='red'>The #{String.downcase(kind)} is closed!</span></p>"
      )

      room
    end
  end

  def execute(%Room{} = room, %{} = mob, %{"kind" => "Block Guard"} = re) do
    execute(room, mob, Map.put(re, "kind", "Normal"))
  end

  def execute(
        %Room{} = room,
        character,
        %{"kind" => "Hidden", "passable_while_hidden" => true} = room_exit
      ) do
    execute(room, character, Map.put(room_exit, "kind", "Normal"))
  end

  def execute(
        %Room{} = room,
        %{} = character,
        %{"kind" => "Hidden", "passable_while_hidden" => false} = room_exit
      ) do
    if Doors.open?(room, room_exit) do
      execute(room, character, Map.put(room_exit, "kind", "Normal"))
    else
      Mobile.send_scroll(character, "<p>There is no exit in that direction.</p>")
      room
    end
  end

  def execute(%Room{} = room, %{} = character, nil) do
    Mobile.send_scroll(character, "<p>There is no exit in that direction.</p>")
    room
  end

  def execute(%Room{} = room, %{} = character, %{"kind" => "Command"}) do
    Mobile.send_scroll(character, "<p>There is no exit in that direction.</p>")
    room
  end

  def execute(%Room{} = room, %{} = character, %{"kind" => "RemoteAction"}) do
    Mobile.send_scroll(character, "<p>There is no exit in that direction.</p>")
    room
  end

  def execute(%Room{} = room, %{} = character, %{"kind" => "Level"} = room_exit) do
    cond do
      room_exit["min"] && character.level < room_exit["min"] ->
        Mobile.send_scroll(character, "<p>#{room_exit["failure_message"]}</p>")
        room

      room_exit["max"] && character.level > room_exit["max"] ->
        Mobile.send_scroll(character, "<p>#{room_exit["failure_message"]}</p>")
        room

      :else ->
        execute(room, character, Map.put(room_exit, "kind", "Normal"))
    end
  end

  def execute(%Room{} = room, %{} = character, %{"kind" => "Alignment"} = room_exit) do
    if character.alignment in room_exit["allowed"] do
      execute(room, room.mobiles[character.ref], Map.put(room_exit, "kind", "Normal"))
    else
      Mobile.send_scroll(character, "<p>A strange power holds you back!</p>")
      room
    end
  end

  def execute(%Room{} = room, %Character{} = character, %{"kind" => "Toll"} = room_exit) do
    amount_in_gold = room_exit["amount_in_gold"]
    amount_in_copper = amount_in_gold * 100

    if Currency.wealth(character) < amount_in_copper do
      message = "<p>You do not have enough to cover the toll of #{amount_in_gold} gold crowns.<p>"

      Mobile.send_scroll(character, message)
      room
    else
      message = "<p>You just paid #{amount_in_gold} gold crowns in toll charges.</p>"

      Mobile.send_scroll(character, message)

      room =
        Room.update_mobile(room, character.ref, fn char ->
          char_currency = Currency.subtract(char, amount_in_copper)

          char
          |> Ecto.Changeset.change(%{
            runic: char_currency.runic,
            platinum: char_currency.platinum,
            gold: char_currency.gold,
            silver: char_currency.silver,
            copper: char_currency.copper
          })
          |> Repo.update!()
          |> Character.load_items()
          |> Repo.save!()
        end)

      execute(room, room.mobiles[character.ref], Map.put(room_exit, "kind", "Normal"))
    end
  end

  def execute(%Room{} = room, %{} = mobile, %{"kind" => "Toll"} = room_exit) do
    execute(room, mobile, Map.put(room_exit, "kind", "Normal"))
  end

  def execute(
        %Room{} = room,
        %{} = character,
        %{"kind" => "Normal", "destination" => destination_id} = room_exit
      ) do
    if !Mobile.held(character) and !Mobile.confused(character, room) do
      Room.display_exit_message(room, %{
        mobile: character,
        message: Mobile.exit_message(character),
        to: destination_id
      })

      if Mobile.stealth_at_level(character, character.level) > 0 do
        Mobile.send_scroll(character, "<p>Sneaking...</p>")
      end

      destination_id
      |> RoomServer.find()
      |> RoomServer.mobile_entered(character)

      put_in(room.mobiles, Map.delete(room.mobiles, character.ref))
      |> party_move(character, room_exit)
    else
      room
    end
  end

  def execute(
        %Room{} = room,
        %{} = character,
        %{"kind" => "Action", "destination" => destination_id} = room_exit
      ) do
    if !Mobile.held(character) and !Mobile.confused(character, room) do
      if Mobile.stealth_at_level(character, character.level) > 0 do
        Mobile.send_scroll(character, "<p>Sneaking...</p>")
      end

      Mobile.send_scroll(
        character,
        "<p><span class='yellow'>#{room_exit["mover_message"]}</span></p>"
      )

      destination_id
      |> RoomServer.find()
      |> RoomServer.mobile_entered(
        character,
        "<span class='yellow'>#{room_exit["to_message"]}</span>"
      )

      Room.display_exit_message(room, %{
        mobile: character,
        message: room_exit["from_message"],
        to: destination_id
      })

      put_in(room.mobiles, Map.delete(room.mobiles, character.ref))
      |> party_move(character, room_exit)
    else
      room
    end
  end

  def execute(%Room{} = room, %{} = character, %{"kind" => kind} = room_exit) do
    Logger.error("unimplemented exit type '#{inspect(kind)}': #{inspect(room_exit)}")

    Mobile.send_scroll(
      character,
      "<p>unimplemented exit type '#{inspect(kind)}': #{inspect(room_exit)}</p>"
    )

    execute(room, character, Map.put(room_exit, "kind", "Normal"))
  end

  def party_move(
        room,
        %{leader: ref, ref: ref} = _character,
        %{"direction" => direction} = room_exit
      ) do
    room.mobiles
    |> Map.values()
    |> Enum.reduce(room, fn
      %Character{leader: ^ref} = party_member, updated_room ->
        Mobile.send_scroll(party_member, "<p> -- Following your Party leader #{direction} --</p>")
        execute(updated_room, party_member, room_exit)

      %Companion{leader: ^ref} = party_member, updated_room ->
        execute(updated_room, party_member, room_exit)

      _, updated_room ->
        updated_room
    end)
  end

  def party_move(room, _character, _room_exit) do
    room
  end
end
