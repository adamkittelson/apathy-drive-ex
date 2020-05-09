defmodule ApathyDrive.Commands.Read do
  use ApathyDrive.Command

  alias ApathyDrive.{
    Ability,
    Character,
    Item,
    ItemInstance,
    Match,
    Repo,
    Room
  }

  def keywords, do: ["read"]

  def execute(%Room{} = room, %Character{} = character, []) do
    message = "<p><span class='red'>Syntax: READ {scroll}</span></p>"
    Mobile.send_scroll(character, message)

    room
  end

  def execute(%Room{} = room, %Character{} = character, args) do
    scroll = Enum.join(args, " ")

    character.inventory
    |> Enum.filter(&(&1.type == "Scroll"))
    |> Match.one(:name_contains, scroll)
    |> case do
      %Item{} = item ->
        ability = Systems.Effect.effect_bonus(item, "Learn")

        cond do
          Item.too_powerful_for_character?(character, item) ->
            message = "<p>You do not meet the requirements to read #{item.name}.</p>"
            Mobile.send_scroll(character, message)

            room

          !Ability.appropriate_alignment?(ability, character) ->
            message = "<p>You don't have the disposition to learn #{ability.name}.</p>"

            Mobile.send_scroll(character, message)
            room

          :else ->
            learn_ability(room, character, item, ability)
        end

      nil ->
        message = "<p><span class='red'>You cannot read that!</span></p>"
        Mobile.send_scroll(character, message)

        room
    end
  end

  defp learn_ability(room, character, scroll, ability) do
    Room.update_mobile(room, character.ref, fn _room, character ->
      effect = %{
        "Grant" => [ability.id],
        "StatusMessage" => "You know how to cast #{ability.name}!",
        "RemoveMessage" => "You forget how to cast #{ability.name}."
      }

      Mobile.send_scroll(character, "<p>As you read the #{scroll.name} it crumbles to dust!</p>")

      Mobile.send_scroll(
        character,
        "<p><span class='blue'>You know how to cast #{ability.name}!</span></p>"
      )

      ItemInstance
      |> Repo.get!(scroll.instance_id)
      |> Repo.delete!()

      character
      |> Systems.Effect.add(effect, :timer.hours(1))
      |> Character.load_abilities()
      |> Character.load_items()
    end)
  end
end
