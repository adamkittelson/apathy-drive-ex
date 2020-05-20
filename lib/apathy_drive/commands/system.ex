defmodule ApathyDrive.Commands.System do
  use ApathyDrive.Command
  import ApathyDrive.Scripts
  alias ApathyDrive.{Ability, Character, Item, Match, Mobile, Monster, Repo, Room}
  alias ApathyDrive.Commands.System
  require Ecto.Query

  def keywords, do: ["system", "sys"]

  def execute(%Room{} = room, %Character{admin: true} = character, args) do
    system(room, character, args)
  end

  def execute(%Room{} = room, %Character{} = character, _args) do
    Mobile.send_scroll(character, "<p>You do not have permission to do that.</p>")
    room
  end

  def system(%Room{} = room, character, ["ouch"]) do
    Room.update_mobile(room, character.ref, fn _room, character ->
      ability = %ApathyDrive.Ability{
        traits: %{"Damage" => 0.1},
        targets: "self",
        energy: 0,
        kind: "attack",
        user_message: "The gods smite you in atonement for your sins!",
        ignores_round_cooldown?: true,
        difficulty: nil
      }

      room
      |> Ability.execute(character.ref, ability, [character.ref])
    end)
  end

  def system(%Room{} = room, character, ["train"]) do
    ApathyDrive.Commands.Train.execute(room, character, true)
  end

  def system(%Room{} = room, character, ["add_admin", name]) do
    {:ok, message} = ApathyDrive.System.add_admin(name)

    Mobile.send_scroll(character, "<p>#{message}</p>")

    room
  end

  def system(%Room{} = room, character, ["give_item" | item_id_or_name]) do
    item_id_or_name = Enum.join(item_id_or_name, " ")

    item_id_or_name
    |> Integer.parse()
    |> case do
      {item_id, ""} ->
        give_item(room, character.ref, item_id)

      _ ->
        Item
        |> Ecto.Query.where(name: ^item_id_or_name)
        |> Repo.all()
        |> Match.all(:match_name, item_id_or_name)
        |> case do
          %Item{id: item_id} ->
            give_item(room, character.ref, item_id)

          nil ->
            Mobile.send_scroll(character, "<p>Item not found.</p>")
            room

          items ->
            Mobile.send_scroll(
              character,
              "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
            )

            Enum.each(items, fn match ->
              Mobile.send_scroll(
                character,
                "<p>-- #{Item.colored_name(match, character: character)} (#{match.id})</p>"
              )
            end)

            room
        end
    end
  end

  def system(%Room{} = room, character, ["summon" | monster_id_or_name]) do
    monster_id_or_name = Enum.join(monster_id_or_name, " ")

    monster_id_or_name
    |> Integer.parse()
    |> case do
      {monster_id, ""} ->
        summon(room, monster_id)

      _ ->
        Monster
        |> Ecto.Query.where(name: ^monster_id_or_name)
        |> Repo.all()
        |> Match.all(:match_name, monster_id_or_name)
        |> case do
          %Monster{id: monster_id} ->
            summon(room, monster_id)

          nil ->
            Mobile.send_scroll(character, "<p>Monster not found.</p>")
            room

          items ->
            Mobile.send_scroll(
              character,
              "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
            )

            Enum.each(items, fn match ->
              Mobile.send_scroll(
                character,
                "<p>-- #{Item.colored_name(match, character: character)} (#{match.id})</p>"
              )
            end)

            room
        end
    end
  end

  def system(%Room{} = room, character, ["kill" | monster]) do
    monster = Enum.join(monster, " ")

    room.mobiles
    |> Map.values()
    |> Match.all(:match_keyword, monster)
    |> case do
      %{} = mobile ->
        Mobile.send_scroll(
          character,
          "<p><span class='red'>You slowly draw a finger across your throat, and then point at #{
            mobile.name
          }.</span></p>"
        )

        Room.send_scroll(
          room,
          "<p><span class='red'>#{character.name()} slowly draws a finger across their throat, and then points at #{
            mobile.name
          }.</span></p>",
          [character]
        )

        Mobile.die(mobile, room)

      nil ->
        Mobile.send_scroll(character, "<p>Victim not found.</p>")
        room

      items ->
        Mobile.send_scroll(
          character,
          "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
        )

        Enum.each(items, fn match ->
          Mobile.send_scroll(
            character,
            "<p>-- #{Mobile.colored_name(match)} (#{match.id})</p>"
          )
        end)

        room
    end
  end

  def system(%Room{} = room, character, ["edit" | args]) do
    System.Edit.execute(room, character, args)
  end

  def system(%Room{} = room, character, ["skill" | args]) do
    System.Skill.execute(room, character, args)
  end

  def system(%Room{} = room, character, ["area" | args]) do
    System.Area.execute(room, character, args)
  end

  def system(%Room{} = room, character, ["room" | args]) do
    System.Room.execute(room, character, args)
  end

  def system(%Room{} = room, character, ["ability" | args]) do
    System.Ability.execute(room, character, args)
  end

  def system(%Room{} = room, character, ["trait" | args]) do
    System.Trait.execute(room, character, args)
  end

  def system(%Room{} = room, character, ["item" | args]) do
    System.Item.execute(room, character, args)
  end

  def system(%Room{} = room, %Character{editing: %module{} = editing} = character, args) do
    character =
      character
      |> Map.put(:editing, Repo.get(module, editing.id))

    module =
      module
      |> Module.split()
      |> List.last()

    module = Module.safe_concat(ApathyDrive.Commands.System, module)

    module.execute(room, character, args)
  end

  def system(%Room{} = room, character, args) do
    System.Misc.execute(room, character, args)
  end
end
