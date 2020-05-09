defmodule ApathyDrive.PubSub do
  def broadcast!(topic, message) do
    Phoenix.PubSub.broadcast!(ApathyDrive.PubSub, topic, message)
  end

  def broadcast(topic, message) do
    Phoenix.PubSub.broadcast(ApathyDrive.PubSub, topic, message)
  end

  def broadcast_from!(from_pid, topic, message) do
    Phoenix.PubSub.broadcast_from!(ApathyDrive.PubSub, from_pid, topic, message)
  end

  def broadcast_from(from_pid, topic, message) do
    Phoenix.PubSub.broadcast_from(ApathyDrive.PubSub, from_pid, topic, message)
  end

  def subscribe(topic, opts \\ []) do
    Phoenix.PubSub.subscribe(ApathyDrive.PubSub, topic, opts)
  end

  def unsubscribe(topic) do
    Phoenix.PubSub.unsubscribe(ApathyDrive.PubSub, topic)
  end
end
