defmodule ApathyDrive.Web do
  @moduledoc """
  A module that keeps using definitions for controllers,
  views and so on.

  This can be used in your application as:

      use MyApp.Web, :controller
      use MyApp.Web, :view

  Keep the definitions in this module short and clean,
  mostly focused on imports, uses and aliases.
  """

  def view do
    quote do
      use Phoenix.View, root: "web/templates"

      # Import URL helpers from the router
      import ApathyDrive.Router.Helpers

      # Import all HTML functions (forms, tags, etc)
      use Phoenix.HTML
      import Phoenix.Controller, only: [get_flash: 2]

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

      # Import URL helpers from the router
      import ApathyDrive.Router.Helpers
    end
  end

  def model do
    quote do
    end
  end

  @doc """
  When used, dispatch to the appropriate controller/view/etc.
  """
  defmacro __using__(which) when is_atom(which) do
    apply(__MODULE__, which, [])
  end
end
