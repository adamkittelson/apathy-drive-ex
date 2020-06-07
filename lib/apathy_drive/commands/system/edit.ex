defmodule ApathyDrive.Commands.System.Edit do
  alias ApathyDrive.{Ability, Item, Mobile, Room, ShopItem, Skill, Trait}
  alias ApathyDrive.Commands.Help

  def execute(%Room{} = room, character, args) do
    query = Enum.join(args, " ")

    cond do
      ability = Ability.match_by_name(query, true) ->
        edit_ability(room, character, ability)

      item = Item.match_by_name(query) ->
        edit_item(room, character, item)

      trait = Trait.match_by_name(query) ->
        edit_trait(room, character, trait)

      skill = Skill.match_by_name(query) ->
        edit_skill(room, character, skill)
    end
  end

  def edit_ability(room, character, %Ability{} = ability) do
    Help.execute(room, character, [ability.name])
    Mobile.send_scroll(character, "<p>You are now editing #{ability.name}.</p>")

    Room.update_mobile(room, character.ref, fn _room, character ->
      character
      |> Map.put(:editing, ability)
      |> Mobile.update_prompt(room)
    end)
  end

  def edit_ability(room, character, abilities) do
    Mobile.send_scroll(
      character,
      "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
    )

    Enum.each(abilities, fn match ->
      Mobile.send_scroll(character, "<p>-- #{match.name}</p>")
    end)

    room
  end

  def edit_item(room, character, %Item{} = item) do
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
  end

  def edit_item(room, character, items) do
    Mobile.send_scroll(
      character,
      "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
    )

    Enum.each(items, fn match ->
      Mobile.send_scroll(character, "<p>-- #{match.name}</p>")
    end)

    room
  end

  def edit_trait(room, character, %Trait{} = trait) do
    Mobile.send_scroll(character, "<p>You are now editing #{trait.name}.</p>")

    Room.update_mobile(room, character.ref, fn _room, character ->
      character
      |> Map.put(:editing, trait)
      |> Mobile.update_prompt(room)
    end)
  end

  def edit_trait(room, character, traits) do
    Mobile.send_scroll(
      character,
      "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
    )

    Enum.each(traits, fn match ->
      Mobile.send_scroll(character, "<p>-- #{match.name}</p>")
    end)

    room
  end

  def edit_skill(room, character, %Skill{} = skill) do
    Mobile.send_scroll(character, "<p>You are now editing #{skill.name}.</p>")

    Room.update_mobile(room, character.ref, fn _room, character ->
      character
      |> Map.put(:editing, skill)
      |> Mobile.update_prompt(room)
    end)
  end

  def edit_skill(room, character, skills) do
    Mobile.send_scroll(
      character,
      "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
    )

    Enum.each(skills, fn match ->
      Mobile.send_scroll(character, "<p>-- #{match.name}</p>")
    end)

    room
  end
end
