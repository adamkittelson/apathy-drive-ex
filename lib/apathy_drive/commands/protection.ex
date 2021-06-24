defmodule ApathyDrive.Commands.Protection do
  use ApathyDrive.Command

  @damage_types %{
    "Cold" => "magical",
    "Electricity" => "magical",
    "Fire" => "magical",
    "Physical" => "physical",
    "Poison" => "magical"
  }

  @protection_levels %{
    0 => {"grey", "none"},
    1 => {"dark-red", "very poor"},
    3 => {"red", "poor"},
    6 => {"dark-blue", "low"},
    9 => {"blue", "below average"},
    13 => {"dark-cyan", "average"},
    18 => {"cyan", "above average"},
    24 => {"dark-magenta", "good"},
    31 => {"magenta", "very good"},
    39 => {"dark-green", "extremely good"},
    48 => {"green", "superb"},
    58 => {"dark-yellow", "excellent"},
    69 => {"yellow", "awesome"},
    81 => {"dark-red", "god awesome"},
    94 => {"red", "IMPREGNABLE!"}
  }

  def keywords, do: ["protection", "prot"]

  def damage_types, do: @damage_types

  def execute(%Room{} = room, character, _args) do
    show_protection(character)
    room
  end

  def percent_for_ac_mr(amount, level) do
    1 - amount / (level * 50 + amount)
  end

  def protection_amount(character, damage_type) do
    resist =
      if @damage_types[damage_type] == "physical" do
        Mobile.physical_resistance_at_level(character, character.level)
      else
        Mobile.magical_resistance_at_level(character, character.level)
      end

    resist_percent = percent_for_ac_mr(resist, character.level)

    modifier = Mobile.ability_value(character, "Resist#{damage_type}")

    modifier =
      if damage_type != "Physical" do
        modifier + Mobile.ability_value(character, "ElementalResist")
      else
        modifier
      end

    if modifier >= 100 do
      modifier / 100
    else
      total = 1 - resist_percent * (1 - modifier / 100)

      max(0, total)
    end
  end

  defp show_protection(character) do
    title = "Average Protection by Damage Type"

    Mobile.send_scroll(
      character,
      "<p><span class='dark-blue'>+-------------------------------------------+</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-blue'>|</span> <span class='yellow'>#{String.pad_trailing(title, 42)}</span><span class='dark-blue'>|</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-blue'>+-----------------+-------------------------+</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-blue'>|</span> <span class='yellow'>Damage Type</span>     <span class='dark-blue'>|</span> <span class='yellow'>Protection Level</span>        <span class='dark-blue'>|</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-blue'>+-----------------+-------------------------+</span></p>"
    )

    @damage_types
    |> Map.keys()
    |> Enum.each(fn damage_type ->
      amount = protection_amount(character, damage_type)

      {color, protection} = protection_level(amount)

      protection = protection <> " (#{trunc(amount * 100)}%)"

      Mobile.send_scroll(
        character,
        "<p><span class='dark-blue'>|</span> <span class='yellow'>#{damage_type |> String.pad_trailing(16)}</span><span class='dark-blue'>|</span> <span class='#{color}'>#{String.pad_trailing(protection, 24)}</span><span class='dark-blue'>|</span></p>"
      )
    end)

    Mobile.send_scroll(
      character,
      "<p><span class='dark-blue'>+-----------------+-------------------------+</span></p>"
    )
  end

  defp protection_level(protection_amount) do
    key =
      @protection_levels
      |> Map.keys()
      |> Enum.reverse()
      |> Enum.find(fn number ->
        number <= 100 * protection_amount
      end)

    @protection_levels[key]
  end
end
