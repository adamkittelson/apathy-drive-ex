defmodule ApathyDrive.SessionController do
  use ApathyDrive.Web, :controller
  alias ApathyDrive.Character

  plug :scrub_params, "session" when action in [:create]

  def new(conn, _params) do
    changeset = Character.sign_up_changeset(%Character{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"session" => %{"email" => nil}}) do
    email_or_password_incorrect(conn)
  end

  def create(conn, %{"session" => %{"password" => nil}}) do
    email_or_password_incorrect(conn)
  end

  def create(conn, %{"session" => %{"email" => email, "password" => password}}) do
    if character = Character.sign_in(email, password) do
      conn =
        conn
        |> put_session(:character, character.id)

      redirect(conn, to: game_path(conn, :game, %{}))
    else
      email_or_password_incorrect(conn)
    end
  end

  def delete(conn, _params) do
    conn
    |> put_session(:character, nil)
    |> redirect(to: "/")
  end

  defp email_or_password_incorrect(conn) do
    changeset = Character.sign_up_changeset(%Character{})

    conn
    |> put_flash(:sign_in, "email or password incorrect")
    |> render("new.html", changeset: changeset)
  end
end
