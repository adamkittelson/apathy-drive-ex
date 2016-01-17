defmodule ApathyDrive.PubSub do

  def broadcast!(topic, message) do
    Phoenix.PubSub.broadcast!(:pub_sub, topic, message)
  end

  def broadcast(topic, message) do
    Phoenix.PubSub.broadcast(:pub_sub, topic, message)
  end

  def broadcast_from!(from_pid, topic, message) do
    Phoenix.PubSub.broadcast_from!(:pub_sub, from_pid, topic, message)
  end

  def broadcast_from(from_pid, topic, message) do
    Phoenix.PubSub.broadcast_from(:pub_sub, from_pid, topic, message)
  end

  def broadcast_except(exceptions, topic, message) do
    topic
    |> subscribers
    |> Enum.each(fn(pid) ->
         if Enum.member?(exceptions, pid) do
           :noop
         else
           send(pid, message)
         end
       end)
    :ok
  end

  def subscribe(pid, topic, opts \\ []) do
    Phoenix.PubSub.subscribe(:pub_sub, pid, topic, opts)
  end

  def subscribers(topic, exceptions \\ []) do
    Phoenix.PubSub.Local.subscribers(:pub_sub, topic, 0) -- exceptions
  end

  def unsubscribe(pid, topic) do
    Phoenix.PubSub.unsubscribe(:pub_sub, pid, topic)
  end

end
