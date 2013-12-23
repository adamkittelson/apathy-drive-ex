defmodule ApathyDrive.NameComponent do
  use GenEvent.Behaviour

  ### Public API
  def get_name(entity) do
    :gen_event.call(entity, ApathyDrive.NameComponent, :get_name)
  end

  ### GenEvent API
  def init(name) do
    {:ok, name}
  end

  def handle_call(:get_name, name) do
    {:ok, name, name}
  end
end
