defmodule ApathyDriveWeb.AdminChannel do
  use ApathyDrive.Web, :channel
  alias ApathyDrive.{Character, DamageType}

  def join("admin", %{"character" => token}, socket) do
    case Phoenix.Token.verify(socket, "character", token, max_age: 1209600) do
      {:ok, character_id} ->
        case Repo.get!(Character, character_id) do
          nil ->
            {:error, %{reason: "unauthorized"}}
          %Character{admin: true} ->
            send(self(), :after_join)
            {:ok, socket}
          %Character{name: nil} -> # Character has been reset, probably due to a game wipe
            {:error, %{reason: "unauthorized"}}
        end
      {:error, _} ->
        {:error, %{reason: "unauthorized"}}
    end
  end

  def handle_info(:after_join, socket) do
    damage_types =
      DamageType.data_for_admin_index
      |> Repo.all

    push(socket, "damage_types", %{damage_types: damage_types})

    {:noreply, socket}
  end

  # Channels can be used in a request/response fashion
  # by sending replies to requests from the client
  def handle_in("update_name", %{"id" => id, "name" => name}, socket) do
    %DamageType{id: String.to_integer(id)}
    |> Ecto.Changeset.change(%{name: name})
    |> Repo.update!
    {:reply, :ok, socket}
  end

  # It is also common to receive messages from the client and
  # broadcast to everyone in the current topic (mud:lobby).
  def handle_in("shout", payload, socket) do
    broadcast socket, "shout", payload
    {:noreply, socket}
  end

  # This is invoked every time a notification is being broadcast
  # to the client. The default implementation is just to push it
  # downstream but one could filter or change the event.
  def handle_out(event, payload, socket) do
    push socket, event, payload
    {:noreply, socket}
  end

end
