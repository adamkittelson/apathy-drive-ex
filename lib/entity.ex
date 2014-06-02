defmodule Entity do
  use Systems.Reload

  ### Public API
  def init do
    :gen_event.start()
  end

  def add_component(pid, component, args) do
    :gen_event.add_handler(pid, component, args)
    Components.add(component, pid)
  end

  def remove_component(pid, component) do
    :gen_event.delete_handler(pid, component, [])
    Components.remove(component, pid)
  end

  def notify(pid, event) do
    :gen_event.notify(pid, event)
  end

  def list_components(entity) do
    :gen_event.which_handlers(entity)
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
    if Enum.member?(:gen_event.which_handlers(entity), Components.Types) do
      Enum.each(Components.Types.get_types(entity), fn(type) ->
        module = type |> Inflex.pluralize |> Inflex.camelize
        :"Elixir.#{module}".add(entity)
      end)
    end
  end

  def remove_from_type_collection(entity) do
    if Enum.member?(:gen_event.which_handlers(entity), Components.Types) do
      Enum.each(Components.Types.get_types(entity), fn(type) ->
        module = type |> Inflex.pluralize |> Inflex.camelize
        :"Elixir.#{module}".remove(entity)
      end)
    end
  end

end
