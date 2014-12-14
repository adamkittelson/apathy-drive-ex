defmodule Components.Monsters do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Monsters, :value)
  end

  def get_monsters(entity) do
    monsters = GenEvent.call(entity, Components.Monsters, :get_monsters)
    Enum.each(monsters, &Parent.set(&1, entity))
    monsters
  end

  def get_monster(monster) when is_pid(monster), do: monster
  def get_monster(monster) when is_number(monster) do
    Monsters.find_by_id(monster)
  end
  def get_monster(monster) do
    mt = MonsterTemplates.find_by_id(monster)
    if mt do
      Systems.Monster.spawn_monster(mt)
    end
  end

  def monster_ids(entity) do
    value(entity)
    |> Enum.map(fn(monster) ->
         cond do
           is_pid(monster) and Process.alive?(monster) ->
             cond do
               Entity.has_component?(monster, Components.ID) ->
                 Components.ID.value(monster)
               true ->
                 Components.Name.value(monster)
             end
           is_integer(monster) ->
             if Monsters.find_by_id(monster) do
               monster
             end
           is_binary(monster) ->
              if MonsterTemplates.find_by_id(monster) do
                monster
              end
         end
       end)
    |> Enum.filter(&(&1 != nil))
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_monsters, new_value})
  end

  def add_monster(entity, monster) do
    Parent.set(monster, entity)
    GenEvent.notify(entity, {:add_monster, monster})
    Entities.save!(entity)
  end

  def remove_monster(entity, monster) do
    Parent.set(monster, nil)
    GenEvent.notify(entity, {:remove_monster, monster})
    Entities.save!(entity)
  end

  def serialize(entity) do
    %{"Monsters" => monster_ids(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, monsters) do
    {:ok, monsters, monsters}
  end

  def handle_call(:get_monsters, monsters) do
    monsters = monsters
               |> Enum.map(&get_monster/1)
               |> Enum.filter(&(&1 != nil))

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