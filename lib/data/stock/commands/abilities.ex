defmodule Commands.Abilities do
  use Systems.Command

  def keywords, do: ["abilities", "spells"]

  def execute(entity, _arguments) do
    Components.Player.send_message(entity, ["scroll", "<p><span class='white'>Your abilities are:</span></p>"])
    Components.Player.send_message(entity, ["scroll", "<p><span class='blue'>---------------------------------------------------------------------------</span></p>"])
    ability_names = Systems.Ability.abilities(entity) |> Enum.map &(Components.Name.value(&1))
    chunks = get_chunks(ability_names)
    Enum.each chunks, &display_abilities(entity, &1)
  end

  defp display_abilities(entity, [ability1, ability2]) do
    Components.Player.send_message(entity, ["scroll", "<p>#{abilitytext(ability1)} #{abilitytext(ability2)}</p>"])
  end

  defp display_abilities(entity, [ability]) do
    Components.Player.send_message(entity, ["scroll", "<p>#{abilitytext(ability)}</p>"])
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
end
