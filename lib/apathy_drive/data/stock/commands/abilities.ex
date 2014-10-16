defmodule Commands.Abilities do
  use Systems.Command

  def keywords, do: ["abilities", "spells"]

  def execute(spirit, nil, _arguments) do
    send_message(spirit, "scroll", "<p><span class='white'>Your abilities are:</span></p>")
    send_message(spirit, "scroll", "<p><span class='blue'>---------------------------------------------------------------------------</span></p>")
    ability_names = Components.Abilities.names(spirit)
    chunks = get_chunks(ability_names)
    Enum.each chunks, &display_abilities(spirit, &1)
  end

  def execute(spirit, monster, _arguments) do
    send_message(spirit, "scroll", "<p><span class='white'>Your abilities are:</span></p>")
    send_message(spirit, "scroll", "<p><span class='blue'>---------------------------------------------------------------------------</span></p>")
    ability_names = Components.Abilities.names(monster)
    chunks = get_chunks(ability_names)
    Enum.each chunks, &display_abilities(spirit, &1)
  end

  defp display_abilities(entity, [ability1, ability2]) do
    send_message(entity, "scroll", "<p>#{abilitytext(ability1)} #{abilitytext(ability2)}</p>")
  end

  defp display_abilities(entity, [ability]) do
    send_message(entity, "scroll", "<p>#{abilitytext(ability)}</p>")
  end

  defp abilitytext(ability) do
    String.ljust("#{ability}", 36)
  end

  defp get_chunks([]), do: []
  defp get_chunks(abilities) do
    chunks = Enum.chunk(abilities, 2)
    last_ability = abilities |> List.last
    if List.flatten(chunks) |> Enum.member?(last_ability) do
      chunks
    else
      [[last_ability] | chunks |> Enum.reverse] |> Enum.reverse
    end
  end

  defp abilities(entity) do
    Systems.Ability.abilities(entity)
  end

end
