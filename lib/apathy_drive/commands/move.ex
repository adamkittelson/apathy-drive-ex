defmodule ApathyDrive.Commands.Move do
  alias ApathyDrive.{
    Ability,
    Character,
    Currency,
    Doors,
    Mobile,
    Repo,
    Room,
    RoomServer
  }

  require Logger

  def execute(%Room{} = room, %{} = character, command) when is_binary(command) do
    direction = Room.direction(command)
    room_exit = Room.get_exit(room, direction)

    execute(room, character, room_exit)
  end

  def execute(%Room{} = room, %{} = mob, %{"kind" => kind} = re)
      when kind in ["Door", "Gate", "Key"] do
    if Doors.open?(room, re) do
      execute(room, mob, Map.put(re, "kind", "Normal"))
    else
      name = if kind == "Gate", do: "gate", else: "door"

      Mobile.send_scroll(
        mob,
        "<p><span class='red'>There is a closed #{name} in that direction!</span></p>"
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

  def execute(
        %Room{} = room,
        %{} = character,
        %{"kind" => "Item", "item" => item_id} = room_exit
      ) do
    if Enum.find(character.inventory ++ character.equipment, &(&1.id == item_id)) do
      execute(room, character, Map.put(room_exit, "kind", "Action"))
    else
      Mobile.send_scroll(character, "<p>#{room_exit["failure_message"]}</p>")
      room
    end
  end

  def execute(
        %Room{} = room,
        %{} = character,
        %{"kind" => "Alignment", "max" => max, "min" => min} = room_exit
      ) do
    if character.evil_points <= max and character.evil_points >= min do
      execute(room, room.mobiles[character.ref], Map.put(room_exit, "kind", "Normal"))
    else
      Mobile.send_scroll(character, "<p>A strange power holds you back!</p>")
      room
    end
  end

  def execute(
        %Room{} = room,
        %{} = character,
        %{"kind" => "Class", "min" => min, "max" => max} = room_exit
      ) do
    if class = Enum.find(character.classes, &(&1.class_id == room_exit["class_id"])) do
      cond do
        min && class.level < min ->
          Mobile.send_scroll(character, "<p>An invisible barrier blocks your entry!</p>")

          Room.send_scroll(
            room,
            "#{Mobile.colored_name(character)} is stopped by an invisible force!",
            [character]
          )

          room

        max && class.level > max ->
          Mobile.send_scroll(character, "<p>An invisible barrier blocks your entry!</p>")

          Room.send_scroll(
            room,
            "#{Mobile.colored_name(character)} is stopped by an invisible force!",
            [character]
          )

          room

        :else ->
          execute(
            room,
            room.mobiles[character.ref],
            Map.put(room_exit, "kind", "Normal")
          )
      end
    else
      Mobile.send_scroll(character, "<p>An invisible barrier blocks your entry!</p>")

      Room.send_scroll(
        room,
        "#{Mobile.colored_name(character)} is stopped by an invisible force!",
        [character]
      )

      room
    end
  end

  def execute(%Room{} = room, %{} = character, %{"kind" => "Class"} = room_exit) do
    classes = Enum.map(character.classes, & &1.class_id)

    if room_exit["class_id"] in classes do
      execute(room, room.mobiles[character.ref], Map.put(room_exit, "kind", "Normal"))
    else
      Mobile.send_scroll(character, "<p>An invisible barrier blocks your entry!</p>")

      Room.send_scroll(
        room,
        "#{Mobile.colored_name(character)} is stopped by an invisible force!",
        [character]
      )

      room
    end
  end

  def execute(
        %Room{} = room,
        %Character{} = character,
        %{"kind" => "Toll"} = room_exit
      ) do
    character =
      if character.sneaking do
        character
        |> Mobile.send_scroll("<p>Sneaking...</p>")
        |> Character.add_attribute_experience(%{
          agility: 0.75,
          charm: 0.25
        })
      else
        character
        |> Map.put(:resting, false)
      end

    if Mobile.exhausted(character) do
      room = put_in(room.mobiles[character.ref], character)
      {:error, :too_tired, room}
    else
      amount_in_gold = room_exit["amount_in_gold"]
      amount_in_copper = amount_in_gold * 100

      if Currency.wealth(character) < amount_in_copper do
        message =
          "<p>You do not have enough to cover the toll of #{amount_in_gold} gold crowns.<p>"

        Mobile.send_scroll(character, message)
        room
      else
        message = "<p>You just paid #{amount_in_gold} gold crowns in toll charges.</p>"

        Mobile.send_scroll(character, message)

        room =
          Room.update_mobile(room, character.ref, fn _room, char ->
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

        execute(
          room,
          room.mobiles[character.ref],
          Map.put(room_exit, "kind", "Normal")
        )
      end
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
    if !Mobile.held(character) and !Mobile.confused(character, room) and
         !ApathyDrive.Scripts.Asylum.enforce_asylum(room, character) do
      room =
        Room.display_exit_message(room, %{
          mobile: character,
          message: Mobile.exit_message(character),
          to: destination_id
        })

      character =
        if character.sneaking do
          character
          |> Mobile.send_scroll("<p>Sneaking...</p>")
          |> Character.add_attribute_experience(%{
            agility: 0.75,
            charm: 0.25
          })
        else
          character
          |> Map.put(:resting, false)
        end

      if Mobile.exhausted(character) do
        character = Map.put(character, :casting, {:move, room_exit})
        room = put_in(room.mobiles[character.ref], character)
        {:error, :too_tired, room}
      else
        character =
          if room.area_id != room_exit["area"] do
            ApathyDrive.KillCount.clear_kill_counts(character)
          else
            character
          end

        destination_id
        |> RoomServer.find()
        |> RoomServer.mobile_entered(character)

        put_in(room.mobiles, Map.delete(room.mobiles, character.ref))
        |> party_move(character, room_exit)
        |> case do
          %Room{} = room ->
            Room.update_moblist(room)
            room

          {:error, :too_tired, room} ->
            Room.update_moblist(room)
            room
        end
      end
    else
      room
    end
  end

  def execute(
        %Room{} = room,
        %{} = character,
        %{"kind" => "Action", "destination" => destination_id} = room_exit
      ) do
    if !Mobile.held(character) and !Mobile.confused(character, room) and
         !ApathyDrive.Scripts.Asylum.enforce_asylum(room, character) do
      character =
        if character.sneaking do
          character
          |> Mobile.send_scroll("<p>Sneaking...</p>")
          |> Character.add_attribute_experience(%{
            agility: 0.75,
            charm: 0.25
          })
        else
          character
          |> Map.put(:resting, false)
        end

      if Mobile.exhausted(character) do
        room = put_in(room.mobiles[character.ref], character)
        {:error, :too_tired, room}
      else
        Mobile.send_scroll(
          character,
          "<p><span class='yellow'>#{room_exit["mover_message"]}</span></p>"
        )

        character =
          if room.area_id != room_exit["area"] do
            ApathyDrive.KillCount.clear_kill_counts(character)
          else
            character
          end

        destination_id
        |> RoomServer.find()
        |> RoomServer.mobile_entered(
          character,
          "<span class='yellow'>#{room_exit["to_message"]}</span>"
        )

        room =
          Room.display_exit_message(room, %{
            mobile: character,
            message: room_exit["from_message"],
            to: destination_id
          })

        room =
          put_in(room.mobiles, Map.delete(room.mobiles, character.ref))
          |> party_move(character, room_exit)

        Room.update_moblist(room)
        room
      end
    else
      room
    end
  end

  def execute(
        %Room{} = room,
        %{} = character,
        %{"kind" => "Trap", "destination" => destination_id} = room_exit
      ) do
    if ApathyDrive.Doors.all_remote_actions_triggered?(room, room_exit) do
      execute(room, character, Map.put(room_exit, "kind", "Normal"))
    else
      if !Mobile.held(character) and !Mobile.confused(character, room) and
           !ApathyDrive.Scripts.Asylum.enforce_asylum(room, character) do
        character =
          if character.sneaking do
            character
            |> Mobile.send_scroll("<p>Sneaking...</p>")
            |> Character.add_attribute_experience(%{
              agility: 0.75,
              charm: 0.25
            })
          else
            character
            |> Map.put(:resting, false)
          end

        if Mobile.exhausted(character) do
          room = put_in(room.mobiles[character.ref], character)
          {:error, :too_tired, room}
        else
          Mobile.send_scroll(
            character,
            "<p><span class='yellow'>#{room_exit["mover_message"]}</span></p>"
          )

          room =
            Room.update_mobile(room, character.ref, fn _room, character ->
              ability = %ApathyDrive.Ability{
                traits: %{
                  "Damage" => [
                    %{
                      kind: "raw",
                      min: trunc(room_exit["max_damage"] * 0.75),
                      max: room_exit["max_damage"],
                      damage_type: "Unaspected",
                      damage_type_id: 3
                    }
                  ]
                },
                targets: "self",
                energy: 0,
                kind: "attack",
                ignores_round_cooldown?: true,
                difficulty: nil
              }

              room
              |> Ability.execute(character.ref, ability, [character.ref])
            end)

          character = room.mobiles[character.ref]

          character =
            if room.area_id != room_exit["area"] do
              ApathyDrive.KillCount.clear_kill_counts(character)
            else
              character
            end

          destination_id
          |> RoomServer.find()
          |> RoomServer.mobile_entered(
            character,
            "<span class='yellow'>#{room_exit["to_message"]}</span>"
          )

          room =
            Room.display_exit_message(room, %{
              mobile: character,
              message: room_exit["from_message"],
              to: destination_id
            })

          room =
            put_in(room.mobiles, Map.delete(room.mobiles, character.ref))
            |> party_move(character, room_exit)

          Room.update_moblist(room)
          room
        end
      else
        room
      end
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

      _, updated_room ->
        updated_room
    end)
  end

  def party_move(room, _character, _room_exit) do
    room
  end

  def energy_cost(%Character{} = character) do
    current_encumbrance = Character.encumbrance(character)
    max_encumbrance = Character.max_encumbrance(character)

    encumbrance_percent = current_encumbrance / max_encumbrance

    energy =
      cond do
        encumbrance_percent < 0.17 ->
          # none
          50

        encumbrance_percent < 0.34 ->
          # light
          100

        encumbrance_percent < 0.67 ->
          # medium
          200

        :else ->
          300
      end

    if character.sneaking do
      energy * 2
    else
      energy
    end
  end

  def energy_cost(%{} = _mobile), do: 0
end
