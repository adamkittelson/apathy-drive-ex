defmodule ApathyDrive.Plugs.HTTPSRedirect do
  def init(opts) do
    opts
  end

  @doc """
  Call this plug.
  """
  def call(conn, _opts) do

    conn
    |> redirect_to_https
  end

  defp redirect_to_https(%Plug.Conn{port: 80} = conn) do
    conn
    |> Plug.Conn.put_resp_header("location", get_current_url_as_proxied_https(conn))
    |> Plug.Conn.resp(302, "")
    |> Plug.Conn.halt
  end

  defp redirect_to_https(conn) do
    conn
  end

  defp get_current_url_as_proxied_https(conn) do
    rewrite_url_as_https(conn)
  end

  defp get_forwarded_host(conn) do
    Plug.Conn.get_req_header(conn, "host")
    |> List.first
  end

  defp rewrite_url_as_https(conn = %Plug.Conn{query_string: query_string}) do
    https_url_with_path(get_forwarded_host(conn), Plug.Conn.full_path(conn), query_string)
  end

  defp https_url_with_path(host, path, ""), do: "https://#{host}#{path}"
  defp https_url_with_path(host, path, query), do: "https://#{host}#{path}?#{query}"

end