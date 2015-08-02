defmodule ApathyDrive.PageController do
  use ApathyDrive.Web, :controller
  alias ApathyDrive.Repo

  def index(conn, _params) do
    render conn, "index.html"
  end

  def game(conn, _params) do
    case get_session(conn, :current_spirit) do
      nil ->
        conn
        |> put_session(:current_spirit, nil)
        |> redirect(to: "/")
      spirit_id ->
        case Repo.get(Spirit, spirit_id) do
          %Spirit{id: id, name: nil} ->
            conn
            |> put_session(:current_spirit, id)
            |> redirect(to: spirit_path(conn, :edit))
          %Spirit{} ->
            render conn, "game.html", []
          nil ->
            conn
            |> put_session(:current_spirit, nil)
            |> redirect(to: "/")
        end
    end
  end

end
