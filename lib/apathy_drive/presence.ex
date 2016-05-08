defmodule ApathyDrive.Presence do
  use Phoenix.Presence, otp_app: :my_app,
                        pubsub_server: :pub_sub

  def metas(topic) do
    topic
    |> list()
    |> Map.values
    |> Enum.map(&Map.values/1)
    |> List.flatten
  end

  def pluck(topic, key) do
    topic
    |> metas()
    |> Enum.map(&(&1[key]))
  end
end