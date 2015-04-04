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

  def subscribe(pid, topic, opts \\ []) do
    :pg2.create(topic)
    unless subscribers(topic) |> Enum.member?(pid) do
      :pg2.join(topic, pid)
    end
    Phoenix.PubSub.subscribe(:pub_sub, pid, topic, opts)
  end

  def subscribers(topic) do
    case :pg2.get_members(topic) do
      {:error, {:no_such_group, _}} ->
        []
      subscribers ->
        subscribers
    end
  end

  def unsubscribe(pid, topic) do
    :pg2.leave(topic, pid)
    Phoenix.PubSub.unsubscribe(:pub_sub, pid, topic)
  end

end
