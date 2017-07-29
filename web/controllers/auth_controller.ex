defmodule ApathyDrive.AuthController do
  use ApathyDrive.Web, :controller

  alias OAuth2.AccessToken

  @doc """
  This action is reached via `/auth` and redirects to the OAuth2 provider
  based on the chosen strategy. The strategy in this example has already
  been stored in `conn.private.oauth2_strategy` in the router's pipeline.
  """
  def index(conn, _params) do
    redirect conn, external: Facebook.authorize_url!([])
  end

  def delete(conn, _params) do
    conn
    |> put_flash(:info, "You have been logged out!")
    |> configure_session(drop: true)
    |> redirect(to: "/")
  end

  @doc """
  This action is reached via `/auth/callback` is the the callback URL that
  the OAuth2 provider will redirect the user back to with a `code` that will
  be used to request an access token. The access token will then be used to
  access protected resources on behalf of the user.
  """
  def callback(conn, %{"code" => code}) do
    # Exchange an auth code for an access token
    token = Facebook.get_token!([code: code], [])

    # Request the user's data with the access token
    user = AccessToken.get!(token, "/me")

    character = ApathyDrive.Character.find_or_create_by_external_id(user.body["id"])

    conn =
      conn
      |> put_session(:character, character.id)

    redirect(conn, to: game_path(conn, :game))
  end

end
