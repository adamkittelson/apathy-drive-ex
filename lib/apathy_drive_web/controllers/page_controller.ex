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

          %Character{} ->
            conn
            |> put_layout({ApathyDriveWeb.LayoutView, :game})
            |> render("game.html", [])

          nil ->
            conn
            |> put_session(:character, nil)
            |> redirect(to: "/")
        end
    end
  end
end
