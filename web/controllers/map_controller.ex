defmodule ApathyDrive.MapController do
  use ApathyDrive.Web, :controller

  def show(conn, %{}) do
    conn
    |> put_layout(false)
    |> render("show.html")
  end
end
