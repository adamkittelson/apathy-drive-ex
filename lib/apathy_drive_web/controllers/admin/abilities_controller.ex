defmodule ApathyDriveWeb.Admin.AbilitiesController do
  use ApathyDriveWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
