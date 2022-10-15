defmodule ApathyDrive.Party do
  alias ApathyDrive.{Mobile, Room}

  def invitees(%Room{} = room, %{} = member) do
    leader(room, member).invitees
    |> Enum.uniq()
  end

  def members(%Room{}, %{leader: nil} = mobile), do: [mobile]

  def members(%Room{} = room, %{leader: ref}) do
    room.mobiles
    |> Map.values()
    |> Enum.filter(&(Map.get(&1, :leader) == ref))
  end

  def exhausted(%Room{} = room, character, energy \\ nil) do
    room
    |> members(character)
    |> Enum.any?(&Mobile.exhausted(&1, energy))
  end

  def size(%Room{} = room, member) do
    room
    |> members(member)
    |> Enum.count()
  end

  def refs(%Room{} = room, member) do
    room
    |> members(member)
    |> Enum.map(& &1.ref)
  end

  def leader(%Room{} = room, %{leader: ref}) do
    room.mobiles[ref]
  end

  def charm_at_level(%Room{} = room, member, level) do
    room
    |> members(member)
    |> Enum.reduce(0, &(&2 + Mobile.attribute_value(&1, :charm)))
  end
end
