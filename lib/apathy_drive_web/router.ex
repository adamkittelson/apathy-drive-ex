defmodule ApathyDriveWeb.Router do
  use ApathyDriveWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, {ApathyDriveWeb.LayoutView, :app}
    plug :protect_from_forgery
    plug :assign_character
    plug :put_secure_browser_headers
  end

  pipeline :admin do
    plug(:require_admin)
    plug(:put_layout, {ApathyDriveWeb.LayoutView, :admin})
  end

  scope "/", ApathyDriveWeb do
    # Use the default browser stack
    pipe_through(:browser)

    get("/", PageController, :index)
    get("/game", PageController, :game, as: :game)
    resources("/sessions", SessionController)

    resources(
      "/characters",
      CharacterController,
      only: [:create, :edit, :update],
      singleton: true
    )
  end

  scope "/admin", ApathyDriveWeb do
    pipe_through(:browser)
    pipe_through(:admin)
  end

  # Fetch the current user from the session and add it to `conn.assigns`. This
  # will allow you to have access to the current user in your views with
  # `@current_user`.
  defp assign_character(conn, _) do
    character_id =
      conn
      |> get_session(:character)

    if character_id do
      character = ApathyDrive.Repo.get(ApathyDrive.Character, character_id)

      if character do
        conn
        |> assign(:character, character_id)
        |> assign(:admin?, !!character.admin)
      else
        conn
        |> put_session(:character, nil)
      end
    else
      conn
      |> put_session(:character, nil)
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
end
