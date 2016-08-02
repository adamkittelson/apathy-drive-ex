defmodule ApathyDrive.SpiritController do
  use ApathyDrive.Web, :controller
  import Comeonin.Bcrypt
  import Ecto.Changeset
  import ApathyDrive.Text

  plug :scrub_params, "spirit" when action in [:create, :update]

  def create(conn, %{"spirit" => spirit_params}) do
    changeset = Spirit.sign_up_changeset(%Spirit{}, spirit_params)

    hashed_password =
      changeset
      |> get_field(:password)
      |> hashpwsalt

    changeset =
      changeset
      |> put_change(:password, hashed_password)

    case Repo.insert(changeset) do
      {:ok, spirit} ->
        conn =
          conn
          |> put_session(:current_spirit, spirit.id)
          |> redirect(to: game_path(conn, :game))
      {:error, changeset} ->
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

    spirit_params =
      spirit_params
      |> Map.put("name", capitalize_first(spirit_params["name"] || ""))

    changeset =
      spirit
      |> Spirit.changeset(spirit_params)

    case Repo.update(changeset) do
      {:ok, spirit} ->
        spirit
        |> Map.put(:room_id, ApathyDrive.Class.start_room(changeset.changes.class_id))
        |> Repo.save!

        conn
        |> redirect(to: game_path(conn, :game))
      {:error, changeset} ->
        render conn, "edit.html", changeset: changeset
    end
  end

end
