defmodule ApathyDrive.PageController do
  use ApathyDrive.Web, :controller
  alias ApathyDrive.Repo
  import Systems.Text

  plug :scrub_params, "spirit" when action in [:update_spirit]
  plug :action

  def index(conn, _params) do
    render conn, "index.html"
  end

  def game(conn, _params) do
    case Repo.get(Spirit, get_session(conn, :current_spirit)) do
      %Spirit{id: id, name: nil} ->
        conn
        |> put_session(:current_spirit, id)
        |> redirect(to: "/create")
      %Spirit{id: id} ->
        render conn, "game.html", []
      nil ->
        conn
        |> put_session(:current_spirit, nil)
        |> redirect(to: "/")
    end
  end

  def edit_spirit(conn, params) do
    spirit = Repo.get(Spirit, get_session(conn, :current_spirit))
    changeset = Spirit.changeset(spirit)
    render conn, "edit.html", changeset: changeset
  end

  def update_spirit(conn, %{"spirit" => spirit_params}) do
    spirit = Repo.get(Spirit, get_session(conn, :current_spirit))

    spirit_params = Map.put(spirit_params, "name", capitalize_first(spirit_params["name"]))

    changeset = Spirit.changeset(spirit, spirit_params)

    if changeset.valid? do
      Repo.update(changeset)

      conn
      |> put_flash(:info, "Your spirit has been created!")
      |> redirect(to: "/")
    else
      render conn, "edit.html", changeset: changeset
    end
  end


end
