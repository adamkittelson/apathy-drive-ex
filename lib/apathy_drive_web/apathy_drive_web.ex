defmodule ApathyDriveWeb do
  @moduledoc """
  A module that keeps using definitions for controllers,
  views and so on.
  This can be used in your application as:
      use <%= application_module %>.Web, :controller
      use <%= application_module %>.Web, :view
  The definitions below will be executed for every view,
  controller, etc, so keep them short and clean, focused
  on imports, uses and aliases.
  Do NOT define functions inside the quoted expressions
  below.
  """

  def controller do
    quote do
      use Phoenix.Controller, namespace: ApathyDriveWeb

      import Plug.Conn
      import ApathyDriveWeb.Gettext
      alias ApathyDriveWeb.Router.Helpers, as: Routes

      # Alias the data repository and import query/model functions
      alias ApathyDrive.Repo
      import Ecto
      import Ecto.Query
    end
  end

  def view do
    quote do
      use Phoenix.View,
        root: "lib/apathy_drive_web/templates",
        namespace: ApathyDriveWeb

      # Import convenience functions from controllers
      import Phoenix.Controller, only: [get_flash: 1, get_flash: 2, view_module: 1]

      # Include shared imports and aliases for views
      unquote(view_helpers())
    end
  end

  def live_view do
    quote do
      use Phoenix.LiveView,
        layout: {ApathyDriveWeb.LayoutView, "live.html"}

      unquote(view_helpers())
    end
  end

  def live_component do
    quote do
      use Phoenix.LiveComponent

      unquote(view_helpers())
    end
  end

  def model do
    quote do
      use Ecto.Schema
      import Ecto
      import Ecto.Changeset
      import Ecto.Query
      alias ApathyDrive.Repo
    end
  end

  def router do
    quote do
      use Phoenix.Router

      import Plug.Conn
      import Phoenix.Controller
      import Phoenix.LiveView.Router
    end
  end

  def channel do
    quote do
      import Ecto
      import Ecto.Changeset
      import Ecto.Query
      alias ApathyDrive.Repo
      use Phoenix.Channel
      import ApathyDriveWeb.Gettext
    end
  end

  defp view_helpers do
    quote do
      # Use all HTML functionality (forms, tags, etc)
      use Phoenix.HTML

      # Import LiveView helpers (live_render, live_component, live_patch, etc)
      import Phoenix.LiveView.Helpers

      # Import basic rendering functionality (render, render_layout, etc)
      import Phoenix.View

      require Ecto.Query

      import ApathyDriveWeb.ErrorHelpers
      import ApathyDriveWeb.Gettext
      alias ApathyDriveWeb.Router.Helpers, as: Routes
    end
  end

  @doc """
  When used, dispatch to the appropriate controller/view/etc.
  """
  defmacro __using__(which) when is_atom(which) do
    apply(__MODULE__, which, [])
  end
end
