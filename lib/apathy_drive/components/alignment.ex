defmodule Components.Alignment do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(monster) do
    GenEvent.call(monster, Components.Alignment, :value)
  end

  def get_alignment(monster) do
    cond do
      good?(monster) ->
        "good"
      evil?(monster) ->
        "evil"
      neutral?(monster) ->
        "neutral"
    end
  end

  def good?(monster) do
    value(monster) < -50
  end

  def evil?(monster) do
    value(monster) > 50
  end

  def neutral?(monster) do
    !good?(monster) and !evil?(monster)
  end

  def add_evil_points(monster, amount_to_add) do
    GenEvent.notify(monster, {:add_evil_points, amount_to_add})
  end

  def serialize(entity) do
    %{"Alignment" => value(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_event({:add_evil_points, amount}, value) do
    value = (value + amount)
            |> min(300)
            |> max(-200)

    {:ok, value}
  end

  def handle_event(_, value) do
    {:ok, value}
  end
end
