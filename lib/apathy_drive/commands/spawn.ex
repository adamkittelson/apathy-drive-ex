defmodule Commands.Spawn do
  use ApathyDrive.Command

  def keywords, do: ["spawn"]

  def execute(%Spirit{} = spirit, arguments) do
    if mt = arguments |> hd |> MonsterTemplate.find do
      room = Spirit.find_room(spirit)

      spawned_monster = MonsterTemplate.spawn_monster(mt, room)

      # execute in a different process because display_enter_message
      # does a broadcast_from
      Task.start fn ->
        Monster.display_enter_message(room, spawned_monster)
      end
      spirit
    end
  end

  def execute(%Monster{} = monster, arguments) do
    if mt = arguments |> hd |> MonsterTemplate.find do
      room = Monster.find_room(monster)

      spawned_monster = MonsterTemplate.spawn_monster(mt, room)

      # execute in a different process because display_enter_message
      # does a broadcast_from
      Task.start fn ->
        Monster.display_enter_message(room, spawned_monster)
      end
      monster
    end
  end
end
