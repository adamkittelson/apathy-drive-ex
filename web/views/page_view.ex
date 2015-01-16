defmodule ApathyDrive.PageView do
  use ApathyDrive.View

  def csrf_token(conn), do: Plug.Conn.get_session(conn, :csrf_token)

end
