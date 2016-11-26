defmodule ApathyDrive.Party do
  alias ApathyDrive.Room

  def invitees(%Room{} = room, %{} = member) do
    leader(room, member).invitees
    |> Enum.uniq
  end

  def members(%Room{} = room, %{leader: ref}) do
    room.mobiles
    |> Map.values
    |> Enum.filter(& Map.get(&1, :leader) == ref)
  end

  def leader(%Room{} = room, %{leader: ref}) do
    room.mobiles[ref]
  end
end
