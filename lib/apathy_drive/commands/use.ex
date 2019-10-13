defmodule ApathyDrive.Commands.Use do
  use ApathyDrive.Command

  alias ApathyDrive.{
    Ability,
    Character,
    Doors,
    ElementalLores,
    Item,
    ItemInstance,
    Match,
    Mobile,
    Repo,
    Room
  }

  def keywords, do: ["use", "light"]

  @directions [
    "n",
    "north",
    "ne",
    "northeast",
    "e",
    "east",
    "se",
    "southeast",
    "s",
    "south",
    "sw",
    "southwest",
    "w",
    "west",
    "nw",
    "northwest",
    "u",
    "up",
    "d",
    "down"
  ]

  def execute(%Room{} = room, %Character{} = character, [lore, "lore"]) do
    if (lore = ElementalLores.lore(lore)) && Mobile.has_ability?(character, "Elemental") do
      if lore.level <= character.level do
        Mobile.send_scroll(character, "<p>You are now using the lore of #{lore.name}.</p>")

        put_in(room.mobiles[character.ref].lore, lore)
      else
        Mobile.send_scroll(
          character,
          "<p>You must be at least level #{lore.level} to use #{lore.name} lore!</p>"
        )

        room
      end
    else
      Mobile.send_scroll(character, "<p>You don't know of a #{lore} lore!</p>")
      room
    end
  end

  def execute(%Room{} = room, %Character{} = character, [item_name, target]) do
    character.inventory
    |> Match.one(:name_contains, item_name)
    |> case do
      nil ->
        Mobile.send_scroll(
          character,
          "<p><span class='red'>Syntax: USE {Item to use} [{target}]</red></p>"
        )

        room

      %Item{type: "Key", id: id} = item ->
        if target in @directions do
          case Room.get_exit(room, target) do
            %{"key" => ^id, "kind" => kind} = room_exit ->
              name = if kind == "Gate", do: "gate", else: "door"

              if Doors.open?(room, room_exit) do
                Mobile.send_scroll(character, "<p>The #{name} is already open.</p>")
                room
              else
                ApathyDrive.Commands.Open.mirror_open!(room_exit, room.id)
                Mobile.send_scroll(character, "<p>You successfully unlocked the #{name}.</p>")
                Mobile.send_scroll(character, "<p>You opened the #{name}.</p>")

                Room.send_scroll(
                  room,
                  "<p>You see #{Mobile.colored_name(character)} open the #{name} #{
                    ApathyDrive.Exit.direction_description(room_exit["direction"])
                  }.</p>",
                  [character]
                )

                room
                |> Room.open!(room_exit["direction"])
                |> deduct_uses(character.ref, item)
              end

            %{"kind" => kind} = room_exit when kind in ["Door", "Gate", "Key"] ->
              name = if kind == "Gate", do: "gate", else: "door"

              if Doors.open?(room, room_exit) do
                Mobile.send_scroll(character, "<p>The #{name} is already open.</p>")
                room
              else
                Mobile.send_scroll(
                  character,
                  "<p>The #{item.name} doesn't seem to fit that lock.</p>"
                )

                room
              end

            _ ->
              Mobile.send_scroll(
                character,
                "<p>There is no lock there.</p>"
              )

              room
          end
        else
          Mobile.send_scroll(
            character,
            "<p><span class='red'>Syntax: USE {Item to use} [{target}]</red></p>"
          )

          room
        end

      %Item{} ->
        Mobile.send_scroll(character, "<p>You may not use that item!</p>")
        room
    end
  end

  def execute(%Room{} = room, %Character{} = character, [item_name]) do
    character.inventory
    |> Match.one(:name_contains, item_name)
    |> case do
      nil ->
        Mobile.send_scroll(
          character,
          "<p><span class='red'>Syntax: USE {Item to use} [{target}]</red></p>"
        )

        room

      %Item{type: "Light", instance_id: instance_id} = item ->
        ItemInstance
        |> Repo.get(instance_id)
        |> Ecto.Changeset.change(%{
          equipped: true
        })
        |> Repo.update!()

        room =
          if current_light = equipped_light_source(character) do
            ApathyDrive.Commands.Remove.execute(room, character, current_light.keywords)
          else
            room
          end

        Mobile.send_scroll(character, "<p>You lit the #{Item.colored_name(item)}.</p>")

        Room.update_mobile(room, character.ref, fn _room, char ->
          Character.load_items(char)
        end)

      %Item{type: "Container"} = item ->
        ability = item.traits["OnUse"]

        room
        |> Ability.execute(character.ref, ability, [character.ref])
        |> deduct_uses(character.ref, item)

      %Item{} ->
        Mobile.send_scroll(character, "<p>You may not use that item!</p>")
        room
    end
  end

  def execute(%Room{} = room, %Character{} = character, _args) do
    Mobile.send_scroll(
      character,
      "<p><span class='red'>Syntax: USE {Item to use} [{target}]</red></p>"
    )

    room
  end

  def use_light_source(%Room{} = room, mobile_ref) do
    if light = equipped_light_source(room.mobiles[mobile_ref]) do
      deduct_uses(room, mobile_ref, light)
    else
      room
    end
  end

  def deduct_uses(room, character_ref, item) do
    Room.update_mobile(room, character_ref, fn room, character ->
      if item.uses do
        if item.uses > 1 do
          ItemInstance
          |> Repo.get(item.instance_id)
          |> Ecto.Changeset.change(%{
            uses: item.uses - 1
          })
          |> Repo.update!()

          Character.load_items(character)
        else
          ItemInstance
          |> Repo.get(item.instance_id)
          |> Repo.delete!()

          Mobile.send_scroll(character, "<p>#{item.destruct_message}</p>")
          Character.load_items(character)
        end
      else
        room
      end
    end)
  end

  def equipped_light_source(%{} = mobile) do
    Enum.find(mobile.equipment, &(&1.type == "Light"))
  end
end
