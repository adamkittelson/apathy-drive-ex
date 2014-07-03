defmodule Components.Help do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    if module_help(entity) do
      module_help(entity).help
    else
      GenEvent.call(entity, Components.Help, :value)
    end
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_help, new_value})
  end

  def serialize(entity) do
    %{"Help" => value(entity)}
  end

  defp module_help(entity) do
    if Entity.has_component?(entity, Components.Module) && Components.Module.value(entity).module_info[:exports] |> Keyword.keys |> Enum.member?(:help) do
      Components.Module.value(entity)
    else
      false
    end
  end

  ### GenEvent API
  def init(state) do
    {:ok, state}
  end

  def handle_call(:value, state) do
    {:ok, state, state}
  end

  def handle_event({:set_help, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end

end
