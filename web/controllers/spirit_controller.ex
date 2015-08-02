defmodule ApathyDrive.SpiritController do
  use ApathyDrive.Web, :controller
  import Comeonin.Bcrypt
  import Ecto.Changeset
  import Systems.Text

  plug :scrub_params, "spirit" when action in [:create, :update]

  def create(conn, %{"spirit" => spirit_params}) do
    changeset = Spirit.sign_up_changeset(%Spirit{}, spirit_params)

    if changeset.valid? do
      hashed_password =
        changeset
        |> get_field(:password)
        |> hashpwsalt

      spirit =
        changeset
        |> put_change(:password, hashed_password)
        |> put_change(:room_id, Room.start_room_id)
        |> Repo.insert!

      conn =
        conn
        |> put_session(:current_spirit, spirit.id)

      redirect(conn, to: game_path(conn, :game))
    else
      render(conn, ApathyDrive.SessionView, "new.html", changeset: changeset)
    end
  end

  def edit(conn, _params) do
    spirit = Repo.get(Spirit, get_session(conn, :current_spirit))
    changeset = Spirit.changeset(spirit)
    render conn, "edit.html", changeset: changeset
  end

  def update(conn, %{"spirit" => spirit_params}) do
    spirit = Repo.get(Spirit, get_session(conn, :current_spirit))

    alignment = case spirit_params["faction"] do
      "Demon" ->
        "evil"
      "Angel" ->
        "good"
      "Elemental" ->
        "neutral"
      _ ->
        nil
    end

    spirit_params =
      spirit_params
      |> Map.put("name", capitalize_first(spirit_params["name"] || ""))
      |> Map.put("alignment", alignment)

    changeset = Spirit.changeset(spirit, spirit_params)

    if changeset.valid? do
      spirit =
        changeset
        |> Repo.update!

      case :global.whereis_name(:"spirit_#{spirit.id}") do
        :undefined ->
          conn
          |> redirect(to: game_path(conn, :game))
        pid ->
          send(pid, {:reroll, name: spirit.name, faction: spirit.faction, alignment: spirit.alignment})

          conn
          |> redirect(to: game_path(conn, :game))
      end
    else
      render conn, "edit.html", changeset: changeset
    end
  end


end
