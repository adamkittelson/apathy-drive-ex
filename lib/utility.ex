defmodule Utility do
  use Systems.Reload

  defmacro delay(time_in_seconds, body) do
    quote do
      function = fn -> unquote body[:do] end

      {:ok, timer} = :timer.apply_after(seconds_to_ms(unquote(time_in_seconds)), Utility, :call, [function])
      timer
    end
  end

  defmacro every(time_in_seconds, body) do
    quote do
      function = fn -> unquote body[:do] end

      {:ok, timer} = :timer.apply_interval(seconds_to_ms(unquote(time_in_seconds)), Utility, :call, [function])
      timer
    end
  end

  def remove_timer(timer) do
    :timer.cancel(timer)
  end

  def call(function) do
    function.()
  end

  def seconds_to_ms(seconds) do
    Float.floor(seconds * 1000)
  end

end
