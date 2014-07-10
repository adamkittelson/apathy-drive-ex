defmodule Components.Characters do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Characters, :value)
  end

  def get_characters(entity) do
    value(entity) |> Enum.map(&Characters.find_by_id(&1))
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_characters, new_value})
  end

  def add_character(entity, character) do
    Parent.set(character, entity)
    GenEvent.notify(entity, {:add_character, Components.ID.value(character)})
  end

  def remove_character(entity, character) do
    Parent.set(character, nil)
    GenEvent.notify(entity, {:remove_character, Components.ID.value(character)})
  end

  def serialize(entity) do
    %{"Characters" => value(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, characters) do
    {:ok, characters, characters}
  end

  def handle_event({:set_characters, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event({:add_character, character}, value) do
    {:ok, [character | value] |> Enum.uniq }
  end

  def handle_event({:remove_character, character}, value) do
    {:ok, List.delete(value, character) }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end