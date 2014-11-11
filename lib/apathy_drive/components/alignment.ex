defmodule Components.Alignment do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(monster) do
    GenEvent.call(monster, Components.Alignment, :value)
  end

  def alter_alignment(monster, "good") do
    add_evil_points(monster, 2)
  end

  def alter_alignment(monster, "neutral") do
    add_evil_points(monster, 1)
  end

  def alter_alignment(monster, "evil") do
    add_evil_points(monster, -0.1)
  end

  def get_alignment(monster) do
    GenEvent.call(monster, Components.Alignment, :get_alignment)
  end

  def good?(monster) do
    GenEvent.call(monster, Components.Alignment, :good?)
  end

  def evil?(monster) do
    GenEvent.call(monster, Components.Alignment, :evil?)
  end

  def neutral?(monster) do
    GenEvent.call(monster, Components.Alignment, :neutral?)
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

  def handle_call(:good?, value) do
    {:ok, value < -50, value}
  end

  def handle_call(:neutral?, value) do
    {:ok, value >= -50 and value <= 50, value}
  end

  def handle_call(:evil?, value) do
    {:ok, value > 50, value}
  end

  def handle_call(:get_alignment, value) do
    alignment = cond do
      value < -50 ->
        "good"
      value > 50 ->
        "evil"
      value >= -50 and value <= 50 ->
        "neutral"
    end

    {:ok, alignment, value}
  end

  def handle_event({:add_evil_points, amount}, value) do
    value = (value + amount)
            |> min(300)
            |> max(-200)
            |> Float.round(1)

    {:ok, value}
  end

  def handle_event(_, value) do
    {:ok, value}
  end
end
