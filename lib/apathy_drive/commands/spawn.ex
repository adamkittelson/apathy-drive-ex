defmodule Commands.Spawn do
  use ApathyDrive.Command

  def keywords, do: ["spawn"]

  def execute(%Spirit{} = spirit, [mt_id | _]) do
    if mt = MonsterTemplate.find(mt_id) do
      room = Spirit.find_room(spirit)

      spawned_monster = MonsterTemplate.create_monster(mt, room)
                        |> MonsterTemplate.spawn

      # execute in a different process because display_enter_message
      # does a broadcast_from
      Task.start fn ->
        Monster.display_enter_message(room, spawned_monster)
      end
      spirit
    end
  end

  def execute(%Monster{} = monster, [mt_id | _]) do
    if mt = MonsterTemplate.find(mt_id) do
      room = Monster.find_room(monster)

      spawned_monster = MonsterTemplate.create_monster(mt, room)
                        |> MonsterTemplate.spawn

      # execute in a different process because display_enter_message
      # does a broadcast_from
      Task.start fn ->
        Monster.display_enter_message(room, spawned_monster)
      end
      monster
    end
  end
end
