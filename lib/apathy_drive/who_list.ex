defmodule ApathyDrive.WhoList do
  def start_link do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  def log_on(%Spirit{name: name, alignment: alignment, pid: pid, school: school}) do
    :ok = Agent.cast(__MODULE__, fn(state) ->
      Map.put(state, pid, %{alignment: alignment, name: name, possessing: "", school: school})
    end)
  end

  def log_on(%Monster{pid: pid, name: monster_name, spirit: %Spirit{name: name, alignment: alignment, school: school}}) do
    :ok = Agent.cast(__MODULE__, fn(state) ->
      Map.put(state, pid, %{alignment: alignment, name: name, possessing: monster_name, school: school})
    end)
  end

  def log_off(pid) do
    :ok = Agent.cast(__MODULE__, fn(state) ->
      Map.delete(state, pid)
    end)
  end

  def list do
    Agent.get_and_update(__MODULE__, fn(state) ->
      state = cleanup(state)
      list = state
             |> Map.values
             |> Enum.sort_by(&(&1.name))

      {list, state}
    end)
  end

  defp cleanup(state) do
    state
    |> Map.keys
    |> Enum.filter(&Process.alive?/1)
    |> Enum.reduce(%{}, fn(pid, map) ->
         Map.put(map, pid, state[pid])
       end)
  end
end
