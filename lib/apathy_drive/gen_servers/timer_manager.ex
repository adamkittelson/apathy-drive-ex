defmodule TimerManager do
  use Systems.Reload
  use GenServer

  # Public API
  def call_after(timer_manager, time, function) do
    :erlang.start_timer(time, timer_manager, function)
  end

  def call_every(timer_manager, time, function) do
    ref = :erlang.start_timer(time, timer_manager, {time, function})
    GenServer.cast(timer_manager, {:add_ref, ref})
    ref
  end

  def cancel(timer_manager, ref) do
    GenServer.cast(timer_manager, {:cancel, ref})
  end

  defp execute_function(function) do
    try do
      function.()
    catch
      kind, error ->
        IO.puts Exception.format(kind, error)
        # {fun, arity} = env.function
        # IO.puts """
        # ** BlockTimer apply error, originating from:
        #      #{env.file}:#{env.line} in #{fun}/#{arity}
        #    error:
        #      #{Exception.format(kind, error)}
        # """
    end
  end

  # GenServer API
  def start do
    GenServer.start(__MODULE__, HashDict.new)
  end

  def init(value) do
    {:ok, value}
  end

  def handle_cast({:add_ref, ref}, refs) do
    {:noreply, HashDict.put(refs, ref, ref) }
  end

  def handle_cast({:cancel, ref}, refs) do
    :erlang.cancel_timer(ref)
    if cur_ref = HashDict.get(refs, ref) do
      :erlang.cancel_timer(cur_ref)
    end
    {:noreply, HashDict.delete(refs, ref) }
  end

  def handle_info({:timeout, ref, {time, function}}, refs) do
    key = refs
          |> HashDict.keys
          |> Enum.find(&(HashDict.get(refs, &1) == ref))

    if key do
      new_ref = :erlang.start_timer(time, self, {time, function})

      execute_function(function)

      {:noreply, HashDict.put(refs, key, new_ref)}
    else
      {:noreply, refs}
    end
  end

  def handle_info({:timeout, ref, function}, refs) do
    execute_function(function)
    {:noreply, refs}
  end

end
