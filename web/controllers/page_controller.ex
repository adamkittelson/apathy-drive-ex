defmodule ApathyDrive.PageController do
  use ApathyDrive.Web, :controller
  alias ApathyDrive.Repo
  alias ApathyDrive.Character
  import Systems.Text

  plug :scrub_params, "spirit" when action in [:update_spirit]

  def index(conn, _params) do
    render conn, "index.html"
  end

  def game(conn, %{"character_id" => character_id}) do
    case Repo.get_by(Character, id: character_id, player_id: conn.assigns[:current_player]) do
      %Character{} = character ->
        IO.puts "found #{character.name}!"
        conn
        |> put_session(:current_character, character_id)
        |> redirect(to: game_path(conn, :game))
      nil ->
        IO.puts "no character for id: #{character_id}, player_id: #{conn.assigns[:current_player]}"
        redirect(conn, to: character_path(conn, :index))
    end
  end

  def game(conn, _params) do
    case get_session(conn, :current_character) do
      nil ->
        conn
        |> put_session(:current_character, nil)
        |> redirect(to: character_path(conn, :index))
      character_id ->
        case Repo.get_by(Character, id: character_id, player_id: conn.assigns[:current_player]) do
          # %Spirit{id: id, name: nil} ->
          #   conn
          #   |> put_session(:current_spirit, id)
          #   |> redirect(to: "/create")
          %Character{id: id} ->
            conn
            |> assign(:current_character, id)
            |> render "game.html", []
          nil ->
            conn
            |> put_session(:current_character, nil)
            |> redirect(to: character_path(conn, :index))
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
        |> Repo.update!

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
