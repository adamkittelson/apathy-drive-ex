defmodule ApathyDrive.Commands.Reputations do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile}

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
      Mobile.send_scroll(character, "<p>#{reputation(rep)} #{character.reputations[area_id].name}")
    end
  end
  def reputation(reputation) when reputation >= 1000, do: "<span class='white'>#{String.ljust("Saint", 12)}</span>"
  def reputation(reputation) when reputation >= 500, do: "<span class='white'>#{String.ljust("Good", 12)}</span>"
  def reputation(reputation) when reputation >= 200, do: "<span class='dark-cyan'>#{String.ljust("Neutral", 12)}</span>"
  def reputation(reputation) when reputation >= 0, do: "<span class='dark-grey'>#{String.ljust("Seedy", 12)}</span>"
  def reputation(reputation) when reputation > -200, do: "<span class='dark-red'>#{String.ljust("Outlaw", 12)}</span>"
  def reputation(reputation) when reputation > -500, do: "<span class='dark-yellow'>#{String.ljust("Criminal", 12)}</span>"
  def reputation(reputation) when reputation > -1000, do: "<span class='yellow'>#{String.ljust("Villian", 12)}</span>"
  def reputation(_), do: "<span class='red'>#{String.ljust("FIEND", 12)}</span>"

end
