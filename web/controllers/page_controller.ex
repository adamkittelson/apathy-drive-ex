defmodule ApathyDrive.PageController do
  use ApathyDrive.Web, :controller
  alias ApathyDrive.Repo
  import Systems.Text

  plug :scrub_params, "spirit" when action in [:update_spirit]

  def index(conn, _params) do
    render conn, "index.html"
  end

  def game(conn, _params) do
    case get_session(conn, :current_spirit) do
      nil ->
        conn
        |> put_session(:current_spirit, nil)
        |> redirect(to: "/")
      spirit_id ->
        case Repo.get(Spirit, spirit_id) do
          %Spirit{id: id, name: nil} ->
            conn
            |> put_session(:current_spirit, id)
            |> redirect(to: "/create")
          %Spirit{} ->
            render conn, "game.html", []
          nil ->
            conn
            |> put_session(:current_spirit, nil)
            |> redirect(to: "/")
        end
    end
  end

  def edit_spirit(conn, _params) do
    spirit = Repo.get(Spirit, get_session(conn, :current_spirit))
    changeset = Spirit.changeset(spirit)
    render conn, "edit.html", changeset: changeset
  end

  def update_spirit(conn, %{"spirit" => spirit_params}) do
    spirit = Repo.get(Spirit, get_session(conn, :current_spirit))

    alignment = case spirit_params["faction"] do
      "Demon" ->
        "evil"
      "Angel" ->
        "good"
      "Elemental" ->
        "neutral"
      _ ->
        nil
    end

    spirit_params =
      spirit_params
      |> Map.put("name", capitalize_first(spirit_params["name"] || ""))
      |> Map.put("alignment", alignment)

    changeset = Spirit.changeset(spirit, spirit_params)

    if changeset.valid? do
      spirit =
        changeset
        |> Repo.update

      case :global.whereis_name(:"spirit_#{spirit.id}") do
        :undefined ->
          conn
          |> redirect(to: game_path(conn, :game))
        pid ->
          send(pid, {:reroll, name: spirit.name, faction: spirit.faction, alignment: spirit.alignment})

          conn
          |> redirect(to: game_path(conn, :game))
      end
    else
      render conn, "edit.html", changeset: changeset
    end
  end


end
