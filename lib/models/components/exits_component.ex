defmodule ApathyDrive.ExitsComponent do
  use GenEvent.Behaviour

  ### Public API
  def get_exits(entity) do
    :gen_event.call(entity, ApathyDrive.ExitsComponent, :get_exits)
  end

  ### GenEvent API
  def init(exit_ids) do
    {:ok, exit_ids}
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