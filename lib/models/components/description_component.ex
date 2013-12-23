defmodule ApathyDrive.DescriptionComponent do
  use GenEvent.Behaviour

  ### Public API
  def get_description(entity) do
    :gen_event.call(entity, ApathyDrive.DescriptionComponent, :get_description)
  end

  ### GenEvent API
  def init(description) do
    {:ok, description}
  end

  def handle_call(:get_description, description) do
    {:ok, description, description}
  end
end
