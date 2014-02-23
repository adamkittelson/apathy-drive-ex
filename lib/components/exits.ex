defmodule Components.Exits do
  use GenEvent.Behaviour

  ### Public API
  def get_exits(entity) do
    :gen_event.call(entity, Components.Exits, :get_exits)
  end

  def serialize(entity) do
    exit_ids = Enum.map get_exits(entity), fn (exit_pid) ->
      Components.ID.value(exit_pid)
    end
    {"Exits", exit_ids}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:get_exits, exits) do
    exits = Enum.map exits, fn (exit_id) ->
      if is_integer(exit_id) do
        :global.whereis_name(:"#{exit_id}")
      else
        exit_id
      end
    end
    {:ok, exits, exits}
  end
end