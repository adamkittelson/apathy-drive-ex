defmodule ApathyDriveWeb.CharacterController do
  use ApathyDriveWeb, :controller
  alias ApathyDrive.{Character, Mailer}
  import Ecto.Changeset
  import ApathyDrive.Text

  plug(:scrub_params, "character" when action in [:create, :update])

  def create(conn, %{"character" => character_params}) do
    changeset = Character.sign_up_changeset(%Character{}, character_params)

    hashed_password =
      changeset
      |> get_field(:password)
      |> to_string
      |> Bcrypt.hash_pwd_salt()

    changeset =
      changeset
      |> put_change(:password, hashed_password)
      |> put_change(:room_id, ApathyDrive.Room.start_room_id())
      |> put_change(:name, capitalize_first(character_params["name"] || ""))

    case Repo.insert(changeset) do
      {:ok, character} ->
        Mailer.send_welcome_email(character)

        conn
        |> put_session(:character, character.id)
        |> redirect(to: Routes.game_path(conn, :game))

      {:error, changeset} ->
        render(
          conn,
          ApathyDriveWeb.SessionView,
          "new.html",
          changeset: changeset,
          tab: "signup_tab"
        )
    end
  end

  def edit(conn, _params) do
    spirit = Repo.get(Character, get_session(conn, :character))
    changeset = Character.changeset(spirit)
    render(conn, "edit.html", changeset: changeset)
  end

  def update(conn, %{"character" => character_params}) do
    character = Repo.get(Character, get_session(conn, :character))

    character_params =
      character_params
      |> Map.put("name", capitalize_first(character_params["name"] || ""))

    changeset =
      character
      |> Character.changeset(character_params)

    case Repo.update(changeset) do
      {:ok, character} ->
        character
        |> Map.put(:room_id, ApathyDrive.Room.start_room_id())
        |> Repo.save!()

        conn
        |> redirect(to: Routes.game_path(conn, :game))

      {:error, changeset} ->
        render(conn, "edit.html", changeset: changeset)
    end
  end
end
