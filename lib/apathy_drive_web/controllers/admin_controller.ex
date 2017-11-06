defmodule ApathyDriveWeb.AdminController do
  use ApathyDrive.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end

end
