defmodule ApathyDrive.SessionController do
  use ApathyDrive.Web, :controller

  plug :scrub_params, "session" when action in [:create]

  def new(conn, _params) do
    changeset = Spirit.sign_up_changeset(%Spirit{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"session" => %{"email" => nil}}) do
    email_or_password_incorrect(conn)
  end

  def create(conn, %{"session" => %{"password" => nil}}) do
    email_or_password_incorrect(conn)
  end

  def create(conn, %{"session" => %{"email" => email, "password" => password}}) do
    if spirit = Spirit.sign_in(email, password) do
      conn =
        conn
        |> put_session(:current_spirit, spirit.id)

      redirect(conn, to: game_path(conn, :game, %{"spirit_id" => spirit.id}))
    else
      email_or_password_incorrect(conn)
    end
  end

  defp email_or_password_incorrect(conn) do
    changeset = Spirit.sign_up_changeset(%Spirit{})

    conn
    |> put_flash(:sign_in, "email or password incorrect")
    |> render("new.html", changeset: changeset)
  end
end
