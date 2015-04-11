defmodule ApathyDrive.Router do
  use Phoenix.Router

  pipeline :browser do
    plug :accepts, ~w(html)
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :assign_current_spirit
  end

  pipeline :auth do
    plug :put_oauth_strategy
  end

  scope "/", ApathyDrive do
    pipe_through :browser # Use the default browser stack

    get "/", PageController, :index
    get  "/game", PageController, :game, as: :game
    get "/create", PageController, :edit_spirit, as: :spirit
    put "/create", PageController, :update_spirit, as: :spirit
  end

  scope "/auth", alias: ApathyDrive do
    pipe_through [:browser, :auth]
    get "/", AuthController, :index
    get "/callback", AuthController, :callback
  end

  socket "/ws", ApathyDrive do
    channel "mud", MUD
  end

  # Fetch the current user from the session and add it to `conn.assigns`. This
  # will allow you to have access to the current user in your views with
  # `@current_user`.
  defp assign_current_spirit(conn, _) do
    spirit_id = get_session(conn, :current_spirit)

    if spirit_id && ApathyDrive.Repo.get(Spirit, spirit_id) do
      assign(conn, :current_spirit, spirit_id)
    else
      conn
      |> assign(:current_spirit, nil)
      |> put_session(:current_spirit, nil)
    end
  end

  # Fetch the configured strategy from the router's config and store the
  # initialized strategy into `conn.private.oauth2_strategy`.
  defp put_oauth_strategy(conn, _) do
    {strategy, opts} = ApathyDrive.Endpoint.config(:oauth2)
    put_private(conn, :oauth2_strategy, apply(strategy, :new, [opts]))
  end

end

