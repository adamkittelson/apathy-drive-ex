defmodule Components.Monsters do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.Monsters, :value)
  end

  def value(entity, new_value) do
    ApathyDrive.Entity.notify(entity, {:set_monsters, new_value})
  end

  def add_monster(entity, monster) do
    ApathyDrive.Entity.notify(entity, {:add_monster, monster})
  end

  def serialize(entity) do
    nil
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, monsters) do
    exits = Enum.map monsters, fn (monster_id) ->
      if is_integer(monster_id) do
        Monsters.find_by_id(monster_id)
      else
        monster_id
      end
    end
    {:ok, monsters, monsters}
  end

  def handle_event({:set_monsters, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event({:add_monster, monster}, value) do
    {:ok, [monster | value] |> Enum.uniq }
  end

  def handle_event({:remove_monster, monster}, value) do
    {:ok, List.delete(value, monster) }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end