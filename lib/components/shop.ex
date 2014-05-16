defmodule Components.Shop do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.Shop, :value)
  end

  def value(entity, new_value) do
    ApathyDrive.Entity.notify(entity, {:set_shop, new_value})
  end

  def items(entity) do
    Enum.map(value(entity), fn(item_hash) ->
      Components.find_by(Components.ID, item_hash["item"])
    end)
  end

  def serialize(entity) do
    %{"Shop" => value(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, shop) do
    {:ok, shop, shop}
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
