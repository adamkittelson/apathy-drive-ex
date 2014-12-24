defmodule Components.Brain do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Brain, :get_brain)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_brain, new_value})
  end

  def think(monster) do
    :random.seed(:os.timestamp)

    sleep = 30000 + :random.uniform(300000 - 30000)

    :random.uniform
    :timer.sleep(sleep)
    Systems.AI.think(monster)
    think(monster)
  end

  def kill(monster) do
    GenEvent.notify(monster, :kill_brain)
  end

  def serialize(_entity) do
    nil
  end

  ### GenEvent API
  def init(name) do
    {:ok, name}
  end

  def handle_call(:get_brain, brain) do
    {:ok, brain, brain}
  end

  def handle_event({:set_brain, new_brain}, _brain) do
    {:ok, new_brain }
  end

  def handle_event(:kill_brain, brain) do
    Process.exit(brain, :killed)
    {:ok, brain }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
