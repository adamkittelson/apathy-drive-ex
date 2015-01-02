defmodule Entity do
  use Systems.Reload
  alias Poison, as: JSON

  ### Public API
  def init do
    GenEvent.start
  end

  def add_component(pid, component, args) do
    GenEvent.add_handler(pid, component, args)
    Components.add(component, pid)
  end

  def remove_component(pid, component) do
    GenEvent.remove_handler(pid, component, [])
    Components.remove(component, pid)
  end

  def list_components(entity) do
    GenEvent.which_handlers(entity)
  end

  def has_component?(entity, component) do
    Enum.member?(list_components(entity), component)
  end

  def has_components?(entity, components) do
    Enum.all?(components, &(has_component?(entity, &1)))
  end

  def serialize_components(entity) do
    Enum.reduce(list_components(entity), %{}, fn(component, components) ->
      component = component.serialize(entity)
      if component == nil do
        components
      else
        Map.merge(components, component)
      end
    end)
    |> JSON.encode!
    |> IO.iodata_to_binary
  end

  def add_to_type_collection(entity) do
    {:ok, tm} = TimerManager.start_link
    Entity.add_component(entity, Components.TimerManager, tm)

    if Enum.member?(list_components(entity), Components.Types) do
      Enum.each(Components.Types.get_types(entity), fn(type) ->
        module = type |> Inflex.pluralize |> Inflex.camelize
        if :"Elixir.#{module}" == Characters do
          Characters.add(entity, id:  Components.ID.value(entity))
          Characters.add(entity, url: Components.URL.value(entity))
        else
          :"Elixir.#{module}".add(entity)
        end
      end)
    end
  end

  def remove_from_type_collection(entity) do
    if Enum.member?(list_components(entity), Components.Types) do
      Enum.each(Components.Types.get_types(entity), fn(type) ->
        module = type |> Inflex.pluralize |> Inflex.camelize
        :"Elixir.#{module}".remove(entity)
      end)
    end
  end

end
