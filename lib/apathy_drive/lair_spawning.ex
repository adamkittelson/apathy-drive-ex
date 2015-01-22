defmodule ApathyDrive.LairSpawning do

  def spawn_lair(room) do
    monster_ids = eligible_monsters(room)
    if Enum.any?(monster_ids) do
      monster_id = select_lair_monster(monster_ids)
      Phoenix.Channel.broadcast "rooms:#{room.id}", "scroll", %{:html => "<p>spawning monster #{monster_id} <p>"}

      #           |> Systems.Monster.spawn_monster(room)
      #
      # Components.Lair.spawn_monster(room, monster)
      # spawn_lair(room)
    end
  end

  def select_lair_monster(monster_ids) do
    :random.seed(:os.timestamp)

    monster_ids
    |> Enum.shuffle
    |> List.first
  end

  def eligible_monsters(room) do
    # todo check monster limit here
    room.lair_monsters
  end

end
