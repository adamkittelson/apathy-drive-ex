defmodule ApathyDrive.Commands.Help do
  use ApathyDrive.Command

  alias ApathyDrive.{
    Ability,
    AbilityDamageType,
    AbilityTrait,
    Class,
    ClassAbility,
    Commands.Inventory,
    Mobile,
    Repo,
    Room
  }

  require Ecto.Query

  def keywords, do: ["help"]

  def execute(%Room{} = room, character, args) do
    query = Enum.join(args, " ")

    case topic(query) do
      [] ->
        Mobile.send_scroll(character, "<p>Sorry! No help is available for that topic.</p>")

      [topic] ->
        help(character, topic)

      list ->
        Mobile.send_scroll(
          character,
          "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
        )

        Enum.each(list, fn match ->
          Mobile.send_scroll(character, "<p>-- #{match.name}</p>")
        end)
    end

    room
  end

  def help(character, %Ability{} = ability) do
    Mobile.send_scroll(
      character,
      "<p><span class='dark-cyan'>+------------------------------------------------------------------+</span></p>"
    )

    Mobile.send_scroll(character, "<p><span class='dark-cyan'>#{ability.name}</span></p>")

    Mobile.send_scroll(
      character,
      "<p>    #{ability.description}</p>"
    )

    Mobile.send_scroll(
      character,
      "\n\n<p><span class='dark-green'>Command:</span> <span class='dark-cyan'>#{ability.command}</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>Kind:</span> <span class='dark-cyan'>#{ability.kind}</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>Targets:</span> <span class='dark-cyan'>#{ability.targets}</span></p>"
    )

    if ability.mana && ability.mana > 0 do
      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Mana Cost:</span> <span class='dark-cyan'>#{ability.mana}</span></p>"
      )
    end

    if ability.duration && ability.duration > 0 do
      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Duration:</span> <span class='dark-cyan'>#{ability.duration} seconds</span></p>"
      )
    end

    if ability.energy && ability.energy > 0 do
      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Energy:</span> <span class='dark-cyan'>#{ability.energy}</span></p>"
      )
    end

    chance =
      max(
        0,
        min(100, Mobile.spellcasting_at_level(character, character.level, ability)) +
          ability.difficulty
      )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>Success Chance:</span> <span class='dark-cyan'>#{chance}%</span></p>"
    )

    if map_size(ability.attributes) > 0 do
      Mobile.send_scroll(
        character,
        "\n\n<p><span class='dark-green'>Required Attributes:</span></p>"
      )

      Enum.each(ability.attributes, fn {attribute, value} ->
        attribute = attribute |> to_string |> String.capitalize()

        Mobile.send_scroll(
          character,
          "<p>  <span class='dark-green'>#{attribute}:</span> <span class='dark-cyan'>#{value}</span></p>"
        )
      end)
    end

    classes =
      ClassAbility
      |> Ecto.Query.where(ability_id: ^ability.id)
      |> Repo.all()
      |> Enum.map(fn ca ->
        Repo.get(Class, ca.class_id).name
      end)

    if Enum.any?(classes) do
      Mobile.send_scroll(
        character,
        "\n\n<p><span class='dark-green'>Learnable By: </span><span class='dark-cyan'>#{
          ApathyDrive.Commands.Inventory.to_sentence(classes)
        }</span></p>"
      )
    end

    traits = AbilityTrait.load_traits(ability.id)

    traits =
      case AbilityDamageType.load_damage(ability.id) do
        [] ->
          traits

        damage ->
          Map.put(traits, "Damage", damage)
      end

    Mobile.send_scroll(character, "\n\n<p><span class='dark-green'>Effects:</span></p>")

    traits
    |> Enum.map(&massage_trait/1)
    |> List.flatten()
    |> Enum.reject(&is_nil/1)
    |> Enum.each(fn
      {name, nil} ->
        Mobile.send_scroll(character, "<p>  <span class='dark-green'>#{name}</span></p>")

      {name, value} ->
        Mobile.send_scroll(
          character,
          "<p>  <span class='dark-green'>#{name}:</span> <span class='dark-cyan'>#{value}</span></p>"
        )
    end)

    Mobile.send_scroll(
      character,
      "<p><span class='dark-cyan'>+------------------------------------------------------------------+</span></p>"
    )
  end

  def topic(query) do
    [Ability.match_by_name(query, true)]
    |> Enum.reject(&is_nil/1)
    |> List.flatten()
  end

  defp massage_trait({"RemoveSpells", ids}) do
    spells =
      ids
      |> Enum.map(fn id ->
        Repo.get!(Ability, id)
      end)
      |> Enum.filter(&(&1 in Ability.learnable_abilities()))
      |> Enum.map(& &1.name)

    if Enum.any?(spells) do
      {"Removes the following spells", Inventory.to_sentence(spells)}
    end
  end

  defp massage_trait({"Heal", %{"max" => max, "min" => min}}) do
    {"Restores", "#{min}-#{max} HP"}
  end

  defp massage_trait({"Damage", damages}) do
    Enum.map(damages, fn %{damage_type: type, kind: kind, max: max, min: min} ->
      {"Damage", "#{min}-#{max} #{kind} damage (#{String.downcase(type)})"}
    end)
  end

  defp massage_trait({"Dodge", amount}) do
    {"Modifies Dodge Skill By", amount}
  end

  defp massage_trait({"AffectsLiving", _}), do: {"Only affects living targets", nil}
  defp massage_trait({"StatusMessage", _}), do: nil
  defp massage_trait({"RemoveMessage", _}), do: nil
  defp massage_trait({name, value}), do: {name, inspect(value)}
end
