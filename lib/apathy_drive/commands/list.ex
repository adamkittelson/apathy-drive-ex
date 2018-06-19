defmodule ApathyDrive.Commands.List do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Level, Mobile, Item}

  def keywords, do: ["list"]

  def execute(%Room{} = room, %Character{} = character, ["skills"]) do
    if Room.trainer?(room) do
      list_skills(room, character)
    else
      Mobile.send_scroll(character, "<p><span class='red'>You cannot LIST SKILLS if you are not at a trainer!</span></p>")
    end
    room
  end

  def execute(%Room{} = room, %Character{} = character, _arguments) do
    room
    |> Room.items_for_sale
    |> list(character)

    room
  end

  def list([], character) do
    Mobile.send_scroll(character, "<p><span class='red'>You cannot LIST if you are not in a shop!</span></p>")
  end

  def list(items, character) do
    character
    |> Mobile.send_scroll("<p><span class='dark-green'>Item</span>                          <span class='dark-cyan'>Price</span></p>")
    |> Mobile.send_scroll("<p><span class='dark-cyan'>───────────────────────────────────────────────────────────────────────────</span></p>")

    items
    |> Enum.each(fn(%Item{name: _name} = item) ->
        item
        |> Map.put(:level, character.level)
        |> Item.from_shop
        |> Item.price
        |> case do
            "priceless" ->
              :noop
            price when price > 0 ->
              Mobile.send_scroll(character, "<p>#{Item.colored_name(item, pad_trailing: 30)}<span class='dark-cyan'>#{price} gold</span></p>")
            _ ->
              Mobile.send_scroll(character, "<p>#{Item.colored_name(item, pad_trailing: 30)}<span class='dark-cyan'>FREE</span></p>")
           end
       end)
  end

  def list_skills(%Room{skills: skills} = room, character) do
    padding =
      skills
      |> Enum.map(& String.length(&1.name))
      |> Enum.max

    Mobile.send_scroll(character, "<p><span class='dark-magenta'>#{String.pad_leading("Skill", padding)}</span> <span class='dark-green'>|</span> <span class='dark-magenta'>Level</span> <span class='dark-green'>|</span> <span class='dark-magenta'>Training Cost</span></p>")

    skills
    |> Enum.each(fn skill ->
         padded_skill_name = String.pad_leading(skill.name, padding)

         character_skill = character.skills[skill.name]

        {exp, level} =
          if character_skill do
            {character_skill.experience, character_skill.level}
          else
            {0, 0}
          end

         padded_level = String.pad_leading("#{level}", 5)

         tnl = Level.exp_to_next_skill_level(level, exp, skill.training_cost_multiplier)

         tnl =
          if tnl > character.experience do
            "<span class='dark-red'>#{tnl}</span>"
          else
            "<span class='green'>#{tnl}</span>"
          end


         Mobile.send_scroll(character, "<p><span class='dark-cyan'>#{padded_skill_name}</span> <span class='dark-green'>|</span> <span class='dark-cyan'>#{padded_level}</span> <span class='dark-green'>|</span> #{tnl}</p>")
       end)

    room
  end
end
