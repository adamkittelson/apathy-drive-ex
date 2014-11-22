defmodule ApathyDrive.PageController do
  use Phoenix.Controller

  plug :action

  def index(conn, _params) do
    render conn, "index.html"
  end

  def game(conn, %{"id" => _id}) do
    render conn, "game.html", []
  end

  def game(conn, _params) do
    url = Systems.Login.create
    redirect conn, to: ApathyDrive.Router.Helpers.game_path(:game, url)
  end
end
