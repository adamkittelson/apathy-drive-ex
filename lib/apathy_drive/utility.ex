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

  def send_message(entity, event, message \\ nil) do
    if Entity.has_component?(entity, Components.Socket) do
      socket = Components.Socket.value(entity)
      if socket do
        Phoenix.Channel.reply socket, event, message
      end
    end
  end

end
