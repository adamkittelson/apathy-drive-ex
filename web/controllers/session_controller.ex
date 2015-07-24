defmodule ApathyDrive.SessionController do
  use ApathyDrive.Web, :controller

  alias ApathyDrive.Session
  alias ApathyDrive.Player

  plug :scrub_params, "session" when action in [:create]

  def new(conn, _params) do
    changeset = Player.sign_up_changeset(%Player{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"session" => %{"email" => nil}}) do
    email_or_password_incorrect(conn)
  end

  def create(conn, %{"session" => %{"password" => nil}}) do
    email_or_password_incorrect(conn)
  end

  def create(conn, %{"session" => %{"email" => email, "password" => password}}) do
    if player = Player.sign_in(email, password) do
      conn =
        conn
        |> put_session(:current_player, player.id)

      redirect(conn, to: character_path(conn, :index))
    else
      email_or_password_incorrect(conn)
    end
  end

  def delete(conn, %{"id" => id}) do
    player = Repo.get!(Player, id)
    Repo.delete!(player)

    conn
    |> put_flash(:info, "Player deleted successfully.")
    |> redirect(to: player_path(conn, :index))
  end

  defp email_or_password_incorrect(conn) do
    changeset = Player.sign_up_changeset(%Player{})

    conn
    |> put_flash(:sign_in, "email or password incorrect")
    |> render("new.html", changeset: changeset)
  end
end
