defmodule Components.LairMonsters do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.LairMonsters, :value)
  end

  def value(entity, new_value) do
    ApathyDrive.Entity.notify(entity, {:set_lair_monsters, new_value})
  end

  def serialize(entity) do
    {"LairMonsters", value(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, value) do
    value = Enum.map value, fn (monster_id) ->
      if is_integer(monster_id) do
        Monsters.find_by_id(monster_id)
      else
        monster_id
      end
    end
    {:ok, value, value}
  end

  def handle_event({:set_lair_monsters, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
