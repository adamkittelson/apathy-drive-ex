defmodule ApathyDrive.Commands.Reputations do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile, Reputation}

  def keywords, do: ["reputations"]

  def execute(%Room{} = room, %Character{} = character, _arguments) do
    Mobile.send_scroll(character, "<p><span class='dark-magenta'>Reputation   Area</span></p>")
    display_reputations(room, character)
    room
  end

  def display_reputations(%Room{} = room, %Character{} = character) do
    reputation(character, room.area_id)

    room.allies
    |> Map.merge(room.enemies)
    |> Map.keys
    |> Enum.each(& reputation(character, &1))
  end

  def reputation(character, area_id) do
    if rep = get_in(character, [:reputations, area_id, :reputation]) do
      reputation = Reputation.word_for_value(rep)
      color = Reputation.color(reputation)
      area = character.reputations[area_id].name
      Mobile.send_scroll(character, "<p><span class='#{color}'>#{String.pad_trailing(reputation, 12)}</span> #{area}")
    end
  end

end
