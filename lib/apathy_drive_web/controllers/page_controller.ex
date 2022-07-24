defmodule ApathyDriveWeb.PageController do
  use ApathyDriveWeb, :controller
  alias ApathyDrive.{Character, Repo}

  def index(conn, _params) do
    render(conn, "index.html")
  end

  def game(conn, _params) do
    case get_session(conn, :character) do
      nil ->
        conn
        |> put_session(:character, nil)
        |> redirect(to: "/")

      spirit_id ->
        case Repo.get(Character, spirit_id) do
          %Character{id: id, name: nil} ->
            conn
            |> put_session(:character, id)
            |> redirect(to: Routes.character_path(conn, :edit))

          %Character{chat_tab: tab} ->
            conn
            |> put_layout({ApathyDriveWeb.LayoutView, :game})
            |> render("game.html", chat_tab: tab)

          nil ->
            conn
            |> put_session(:character, nil)
            |> redirect(to: "/")
        end
    end
  end

  def welcome(conn, %{"token" => token}) do
    {:ok, character_id} = Phoenix.Token.verify(ApathyDriveWeb.Endpoint, "welcome email", token)

    case Repo.get(Character, character_id) do
      %Character{welcome_token: ^token} = character ->
        character
        |> Ecto.Changeset.change(%{
          welcome_token: nil,
          email_verified: true
        })
        |> Repo.update!()

        conn
        |> put_session(:character, character.id)
        |> redirect(to: Routes.game_path(conn, :game, %{}))

      _ ->
        conn
        |> put_session(:character, nil)
        |> redirect(to: "/")
    end
  end
end
