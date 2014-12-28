defmodule Components.Monsters do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Monsters, :value)
  end

  def get_monsters(entity) do
    GenEvent.call(entity, Components.Monsters, :get_monsters)
    |> Enum.reject(&(&1 == nil))
  end

  def get_monster(monster, entity \\ nil)
  def get_monster(monster, _entity) when is_pid(monster), do: monster
  def get_monster(monster, entity) when is_number(monster) do
    monster = Monsters.find_by_id(monster)
    Parent.set(monster, entity)
    monster
  end
  def get_monster(monster, room) do
    mt = MonsterTemplates.find_by_id(monster)
    if mt do
      monster = Systems.Monster.spawn_monster(mt)
      if room do
        alignment = Components.Alignment.value(monster)
        light     = Components.Light.value(room)

        cond do
          alignment > 0 and light < 0 ->
            new_alignment = min(abs(light), 300)
            Components.Alignment.value(monster, new_alignment)
          alignment < 0 and light > 0 ->
            new_alignment = max(light, -200)
            Components.Alignment.value(monster, new_alignment)
          true ->
            nil
        end
      end
      Parent.set(monster, room)
      monster
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
    alignment = Components.Alignment.value(monster)
    light     = Components.Light.value(entity)

    cond do
      alignment > 0 and light < 0 ->
        new_alignment = min(abs(light), 300)
        Components.Alignment.value(monster, new_alignment)
      alignment < 0 and light > 0 ->
        new_alignment = max(light, -200)
        Components.Alignment.value(monster, new_alignment)
      true ->
        nil
    end

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
    monsters = entity
               |> monster_ids
               |> Enum.filter(&(is_integer(&1)))

    %{"Monsters" => monsters}
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