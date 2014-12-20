defmodule Components.Module do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Module, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_module, new_value})
  end

  def serialize(entity) do
     %{"Module" => value(entity) |> Atom.to_string}
  end

  ### GenEvent API
  def init(value) when is_binary(value) do
    {:ok, String.to_atom(value)}
  end

  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_event({:set_module, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
