defmodule ApathyDrive.Web do
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
      use Phoenix.View, root: "web/templates"

      # Import convenience functions from controllers
      import Phoenix.Controller, only: [get_csrf_token: 0, get_flash: 2, view_module: 1]

      # Import URL helpers from the router
      import ApathyDrive.Router.Helpers

      # Use all HTML functionality (forms, tags, etc)
      use Phoenix.HTML

      def list_input(form, field, opts) when is_atom(field) and is_list(opts) do
        opts =
          opts
          |> Keyword.put_new(:type, :text)
          |> Keyword.put_new(:id, id_from(form, field))
          |> Keyword.put_new(:name, name_from(form, field))
          |> Keyword.put_new(:value, value_from(form, field))
        tag(:input, opts)
      end

      def jsonarea(form, field, opts \\ []) do
        opts =
          opts
          |> Keyword.put_new(:id, id_from(form, field))
          |> Keyword.put_new(:name, name_from(form, field))

        {value, opts} = Keyword.pop(opts, :value, value_from(form, field) || "")
        content_tag(:textarea, html_escape(["\n", value]), Keyword.merge(opts, [class: "json"]))
      end

      defp value_from(%{model: model, params: params}, field) do
        case Map.get(params, Atom.to_string(field)) || Map.get(model, field) do
          %{} = value ->
            Poison.encode!(value)
          value when is_list(value) ->
            Poison.encode!(value)
          value ->
            value
        end
      end
      defp value_from(name, _field) when is_atom(name),
        do: nil

      defp id_from(%{name: name}, field),
        do: "#{name}_#{field}"
      defp id_from(name, field) when is_atom(name),
        do: "#{name}_#{field}"

      defp name_from(%{name: name}, field),
        do: "#{name}[#{field}]"
      defp name_from(name, field) when is_atom(name),
        do: "#{name}[#{field}]"

    end
  end

  def controller do
    quote do
      use Phoenix.Controller

      # Alias the data repository and import query/model functions
      alias ApathyDrive.Repo
      import Ecto
      import Ecto.Query, only: [from: 1, from: 2]
      import ApathyDrive.Router.Helpers
    end
  end

  def model do
    quote do
      use Ecto.Schema
      import Ecto
      import Ecto.Changeset
      import Ecto.Query
      use Timex.Ecto.Timestamps
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
      import Ecto.Query, only: [from: 1, from: 2]
    end
  end

  @doc """
  When used, dispatch to the appropriate controller/view/etc.
  """
  defmacro __using__(which) when is_atom(which) do
    apply(__MODULE__, which, [])
  end
end
