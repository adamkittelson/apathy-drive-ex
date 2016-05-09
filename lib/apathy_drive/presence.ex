defmodule ApathyDrive.Presence do
  use Phoenix.Presence, otp_app: :my_app,
                        pubsub_server: :pub_sub

  def metas(topic) when is_binary(topic) do
    topic
    |> list()
    |> metas()
  end

  def metas(%{} = map) do
    map
    |> Map.values
    |> Enum.map(&Map.get(&1, :metas))
    |> List.flatten
  end

end
