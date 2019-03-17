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

  def view do
    quote do
      use Phoenix.View,
        root: "lib/apathy_drive_web/templates",
        namespace: ApathyDriveWeb

      # Import convenience functions from controllers
      import Phoenix.Controller, only: [get_csrf_token: 0, get_flash: 2, view_module: 1]

      # Import URL helpers from the router
      alias ApathyDriveWeb.Router.Helpers, as: Routes
      import ApathyDriveWeb.ErrorHelpers
      import ApathyDriveWeb.Gettext

      # Use all HTML functionality (forms, tags, etc)
      use Phoenix.HTML
    end
  end

  def controller do
    quote do
      use Phoenix.Controller, namespace: ApathyDriveWeb

      # Alias the data repository and import query/model functions
      alias ApathyDrive.Repo
      import Ecto
      import Ecto.Query
      alias ApathyDriveWeb.Router.Helpers, as: Routes
      import ApathyDriveWeb.Gettext
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
    end
  end

  def channel do
    quote do
      use Phoenix.Channel
      # Alias the data repository and import query/model functions
      alias ApathyDrive.Repo
      import Ecto
      import Ecto.Query
      import ApathyDriveWeb.Gettext
    end
  end

  @doc """
  When used, dispatch to the appropriate controller/view/etc.
  """
  defmacro __using__(which) when is_atom(which) do
    apply(__MODULE__, which, [])
  end
end
