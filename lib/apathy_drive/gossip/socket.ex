defmodule ApathyDrive.Gossip.Socket do
  use WebSockex
  require Logger
  alias ApathyDrive.Gossip

  def start_link() do
    WebSockex.start_link(config(:url), __MODULE__, [], name: __MODULE__)
  end

  def handle_connect(_conn, state) do
    Logger.debug("Gossip connected to #{config(:url)}")
    send(self(), :authorize)
    {:ok, state}
  end

  def handle_info(:authorize, state) do
    message =
      Poison.encode!(%{
        "event" => "authenticate",
        "payload" => %{
          "client_id" => config(:client_id),
          "client_secret" => config(:secret_id),
          "supports" => ["channels"],
          "channels" => ["gossip"],
          "user_agent" => "Apathy Drive v#{to_string(Application.spec(:apathy_drive, :vsn))}"
        }
      })

    {:reply, {:text, message}, state}
  end

  def handle_frame({:text, msg}, state) do
    msg
    |> Poison.decode!()
    |> handle_message()
    |> case do
      :ok ->
        {:ok, state}

      {:ok, response} ->
        {:reply, {:text, response}, state}
    end
  end

  def handle_frame(_, state) do
    {:ok, state}
  end

  def handle_cast({:broadcast, channel, name, message}, state) do
    message =
      Poison.encode!(%{
        "event" => "messages/new",
        "payload" => %{
          "channel" => channel,
          "name" => name,
          "message" => message
        }
      })

    {:reply, {:text, message}, state}
  end

  defp handle_message(%{"event" => "messages/broadcast", "payload" => payload}) do
    Gossip.receive_message(payload)
  end

  defp handle_message(%{"event" => "authenticate", "status" => status}) do
    Logger.debug("Gossip Authentication: #{status}")
  end

  defp handle_message(%{"event" => "heartbeat"}) do
    Gossip.respond_to_heartbeat()
  end

  defp handle_message(msg) do
    Logger.warn("unrecognized Gossip message: #{inspect(msg)}")
  end

  defp config(key) do
    Application.get_all_env(:apathy_drive)[:gossip][key]
  end
end
