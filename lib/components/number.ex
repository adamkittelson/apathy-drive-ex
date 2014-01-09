defmodule Components.Number do
  use GenEvent.Behaviour

  ### Public API
  def get_number(entity) do
    :gen_event.call(entity, Components.Number, :get_number)
  end

  ### GenEvent API
  def init(number) do
    {:ok, number}
  end

  def handle_call(:get_number, number) do
    {:ok, number, number}
  end
end
