defmodule ApathyDrive.PlayerController do
  use ApathyDrive.Web, :controller
  import Comeonin.Bcrypt
  import Ecto.Changeset

  alias ApathyDrive.Player

  plug :scrub_params, "player" when action in [:create]

  def create(conn, %{"player" => player_params}) do
    changeset = Player.sign_up_changeset(%Player{}, player_params)

    if changeset.valid? do
      hashed_password =
        changeset
        |> get_field(:password)
        |> hashpwsalt

      player =
        changeset
        |> put_change(:password, hashed_password)
        |> Repo.insert!

      conn =
        conn
        |> put_session(:current_player, player.id)

      redirect(conn, to: character_path(conn, :index))
    else
      render(conn, ApathyDrive.SessionView, "new.html", changeset: changeset)
    end
  end

end
