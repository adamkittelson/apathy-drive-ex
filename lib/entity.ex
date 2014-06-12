defmodule Entity do
  use Systems.Reload

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

  def serialize_components(entity) do
    Enum.reduce(list_components(entity), %{}, fn(component, components) ->
      component = component.serialize(entity)
      if component == nil do
        components
      else
        Map.merge(components, component)
      end
    end) |> Jazz.encode!
  end

  def add_to_type_collection(entity) do
    if Enum.member?(list_components(entity), Components.Types) do
      Enum.each(Components.Types.get_types(entity), fn(type) ->
        module = type |> Inflex.pluralize |> Inflex.camelize
        :"Elixir.#{module}".add(entity)
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
