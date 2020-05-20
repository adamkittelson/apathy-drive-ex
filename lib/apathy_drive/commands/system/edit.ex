defmodule ApathyDrive.Commands.System.Edit do
  alias ApathyDrive.{Ability, Item, Mobile, Room, ShopItem, Skill, Trait}
  alias ApathyDrive.Commands.Help

  def execute(%Room{} = room, character, ["ability" | ability_name]) do
    ability_name = Enum.join(ability_name, " ")

    case Ability.match_by_name(ability_name, true) do
      nil ->
        Mobile.send_scroll(character, "<p>\"#{ability_name}\" does not match any abilities.</p>")
        room

      %Ability{} = ability ->
        Help.execute(room, character, [ability.name])
        Mobile.send_scroll(character, "<p>You are now editing #{ability.name}.</p>")

        Room.update_mobile(room, character.ref, fn _room, character ->
          character
          |> Map.put(:editing, ability)
          |> Mobile.update_prompt(room)
        end)

      matches ->
        Mobile.send_scroll(
          character,
          "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
        )

        Enum.each(matches, fn match ->
          Mobile.send_scroll(character, "<p>-- #{match.name}</p>")
        end)

        room
    end
  end

  def execute(%Room{} = room, character, ["item" | item_name]) do
    item_name = Enum.join(item_name, " ")

    case Item.match_by_name(item_name) do
      nil ->
        Mobile.send_scroll(character, "<p>\"#{item_name}\" does not match any items.</p>")
        room

      %Item{} = item ->
        shop_item =
          %ShopItem{item: item}
          |> Item.from_assoc()

        ApathyDrive.Commands.Look.look_at_item(character, shop_item)
        Mobile.send_scroll(character, "<p>You are now editing #{item.name}.</p>")

        Room.update_mobile(room, character.ref, fn _room, character ->
          character
          |> Map.put(:editing, item)
          |> Mobile.update_prompt(room)
        end)

      matches ->
        Mobile.send_scroll(
          character,
          "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
        )

        Enum.each(matches, fn match ->
          Mobile.send_scroll(character, "<p>-- #{match.name}</p>")
        end)

        room
    end
  end

  def execute(%Room{} = room, character, ["trait" | trait_name]) do
    trait_name = Enum.join(trait_name, " ")

    case Trait.match_by_name(trait_name) do
      nil ->
        Mobile.send_scroll(character, "<p>\"#{trait_name}\" does not match any traits.</p>")
        room

      %Trait{} = trait ->
        Mobile.send_scroll(character, "<p>You are now editing #{trait.name}.</p>")

        Room.update_mobile(room, character.ref, fn _room, character ->
          character
          |> Map.put(:editing, trait)
          |> Mobile.update_prompt(room)
        end)

      matches ->
        Mobile.send_scroll(
          character,
          "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
        )

        Enum.each(matches, fn match ->
          Mobile.send_scroll(character, "<p>-- #{match.name}</p>")
        end)

        room
    end
  end

  def execute(%Room{} = room, character, ["skill" | skill_name]) do
    skill_name = Enum.join(skill_name, " ")

    case Skill.match_by_name(skill_name, true) do
      nil ->
        Mobile.send_scroll(character, "<p>\"#{skill_name}\" does not match any skills.</p>")
        room

      %Skill{} = skill ->
        Mobile.send_scroll(character, "<p>You are now editing #{skill.name}.</p>")

        Room.update_mobile(room, character.ref, fn _room, character ->
          character
          |> Map.put(:editing, skill)
          |> Mobile.update_prompt(room)
        end)

      matches ->
        Mobile.send_scroll(
          character,
          "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
        )

        Enum.each(matches, fn match ->
          Mobile.send_scroll(character, "<p>-- #{match.name}</p>")
        end)

        room
    end
  end

  def execute(%Room{} = room, character, _args) do
    Mobile.send_scroll(character, "<p>Invalid system command.</p>")

    room
  end
end
