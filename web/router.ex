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
    plug :assign_current_player
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
    get "/create", PageController, :edit_spirit, as: :spirit
    put "/create", PageController, :update_spirit, as: :spirit
    get "/angels", FactionController, :angels
    get "/demons", FactionController, :demons
    get "/elementals", FactionController, :elementals
    resources "/characters", CharacterController
    resources "/players", PlayerController, only: [:create]
    resources "/sessions", SessionController
  end

  scope "/system", ApathyDrive do
    pipe_through [:browser, :admin]

    resources "/rooms", RoomController
    resources "/monsters", MonsterController
    resources "/races", RaceController
    resources "/classes", ClassController
  end

  scope "/auth", alias: ApathyDrive do
    pipe_through [:browser, :auth]
    get "/", AuthController, :index
    get "/callback", AuthController, :callback
  end

  socket "/ws", ApathyDrive do
    channel "mud", MUD
    channel "index", Index
  end

  # Fetch the current user from the session and add it to `conn.assigns`. This
  # will allow you to have access to the current user in your views with
  # `@current_user`.
  defp assign_current_player(conn, _) do
    player_id = conn
                |> get_session(:current_player)

    if player_id && (player = ApathyDrive.Repo.get(ApathyDrive.Player, player_id)) do
      conn
      |> assign(:current_player, player_id)
      |> assign(:admin?, player.admin)
    else
      conn
      |> put_session(:current_player, nil)
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

