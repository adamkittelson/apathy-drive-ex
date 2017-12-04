defmodule ApathyDriveWeb.Admin.AbilitiesChannel do
  use ApathyDrive.Web, :channel
  alias ApathyDrive.{Ability, Character}

  def join("admin:abilities", %{"character" => token}, socket) do
    case ApathyDriveWeb.AdminChannelHelper.authorize(socket, token) do
      {:ok, %Character{} = _character} ->
        send(self(), :after_join)
        {:ok, socket}
      {:error, error} ->
        {:error, error}
    end
  end

  def handle_info(:after_join, socket) do
    page =
      Ability.data_for_admin_index
      |> Ecto.Query.order_by(asc: :id)
      |> Repo.paginate(%{"page" => 1})

    push(socket, "abilities", %{valid_targets: Ability.valid_targets, kinds: Ability.kinds, page: page})

    {:noreply, socket}
  end

  # Channels can be used in a request/response fashion
  # by sending replies to requests from the client
  def handle_in("update", form_data, socket) do
    Ability
    |> Repo.get!(form_data["id"])
    |> Ability.changeset(form_data)
    |> Repo.update!
    {:reply, :ok, socket}
  end

  def handle_in("fetch_page", %{"page_number" => page} = params, socket) do
    query =
      cond do
        params["order_by"] && params["descending"] ->
          order_by = String.to_existing_atom(params["order_by"])

          Ability.data_for_admin_index
          |> Ecto.Query.order_by(desc: ^order_by)
        params["order_by"] ->
          order_by = String.to_existing_atom(params["order_by"])

          Ability.data_for_admin_index
          |> Ecto.Query.order_by(asc: ^order_by)
        :else ->
          Ability.data_for_admin_index
          |> Ecto.Query.order_by(asc: :id)
      end

    query = if params["query"], do: Ecto.Query.where(query, [a], ilike(a.name, ^"%#{params["query"]}%")), else: query

    page = Repo.paginate(query, %{"page" => page})

    push(socket, "abilities", %{valid_targets: Ability.valid_targets, kinds: Ability.kinds, page: page})

    {:noreply, socket}
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
