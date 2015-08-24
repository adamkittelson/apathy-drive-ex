defmodule ApathyDrive.Router do
  use Phoenix.Router

  pipeline :browser do
    if Mix.env == :prod do
      plug ApathyDrive.Plugs.HTTPSRedirect
    end
    plug :accepts, ~w(html)
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :assign_current_spirit
    plug :put_secure_browser_headers
  end

  pipeline :auth do
    plug :put_oauth_strategy
  end

  pipeline :admin do
    plug :require_admin
  end

  scope "/", ApathyDrive do
    pipe_through :browser # Use the default browser stack

    get "/", PageController, :index
    get  "/game", PageController, :game, as: :game
    get "/angels", FactionController, :angels
    get "/demons", FactionController, :demons
    get "/elementals", FactionController, :elementals
    resources "/sessions", SessionController
    resources "/spirits", SpiritController, only: [:create, :edit, :update],
                                            singleton: true
  end

  scope "/system", ApathyDrive do
    pipe_through [:browser, :admin]

    resources "/rooms", RoomController
    resources "/monsters", MonsterController
    resources "/classes", ClassController
  end

  scope "/auth", alias: ApathyDrive do
    pipe_through [:browser, :auth]
    get "/", AuthController, :index
    get "/callback", AuthController, :callback
  end

  # Fetch the current user from the session and add it to `conn.assigns`. This
  # will allow you to have access to the current user in your views with
  # `@current_user`.
  defp assign_current_spirit(conn, _) do
    spirit_id = conn
                |> get_session(:current_spirit)

    if spirit_id && (spirit = ApathyDrive.Repo.get(Spirit, spirit_id)) do
      conn
      |> assign(:current_spirit, spirit_id)
      |> assign(:admin?, spirit.admin)
    else
      conn
      |> put_session(:current_spirit, nil)
    end
  end

  defp require_admin(conn, _) do
    case conn.assigns[:admin?] do
      true ->
        conn
      _ ->
        conn
        |> redirect(to: "/")
        |> halt
    end
  end

  # Fetch the configured strategy from the router's config and store the
  # initialized strategy into `conn.private.oauth2_strategy`.
  defp put_oauth_strategy(conn, _) do
    {strategy, opts} = ApathyDrive.Endpoint.config(:oauth2)
    put_private(conn, :oauth2_strategy, apply(strategy, :new, [opts]))
  end

end
