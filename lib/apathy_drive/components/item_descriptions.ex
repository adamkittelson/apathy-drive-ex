defmodule Components.ItemDescriptions do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.ItemDescriptions, :value)
  end

  def get_item_descriptions(entity) do
    GenEvent.call(entity, Components.ItemDescriptions, :get_item_descriptions)
  end

  def serialize(entity) do
    item_descriptions = entity
                        |> get_item_descriptions
                        |> Enum.reduce(%{}, fn(pid, item_descriptions) ->
                             Map.put(item_descriptions, Components.Name.value(pid), Components.Description.value(pid))
                           end)
    %{"ItemDescriptions" => item_descriptions}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, item_descriptions) do
    {:ok, item_descriptions, item_descriptions}
  end

  def handle_call(:get_item_descriptions, %{} = item_descriptions) do
    item_descriptions = item_descriptions
                        |> Map.keys
                        |> Enum.map fn(item_name) ->
                             {:ok, entity} = Entity.init
                             Entity.add_component(entity, Components.Name, item_name)
                             Entity.add_component(entity, Components.Keywords, item_name |> String.split)
                             Entity.add_component(entity, Components.Description, item_descriptions[item_name])
                             entity
                           end
    {:ok, item_descriptions, item_descriptions}
  end

  def handle_call(:get_item_descriptions, item_descriptions) do
    {:ok, item_descriptions, item_descriptions}
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end