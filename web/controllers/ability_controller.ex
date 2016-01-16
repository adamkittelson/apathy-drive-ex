defmodule ApathyDrive.AbilityController do
  use ApathyDrive.Web, :controller
  import Ecto.Query

  alias ApathyDrive.{Ability,ClassAbility,MonsterAbility}

  plug :scrub_params, "ability" when action in [:create, :update]

  def index(conn, %{"q" => query} = params) do

    if query == "" do
      page =
        ApathyDrive.Ability
        |> Repo.paginate(params)

      render(conn, "index.html",
        abilities: page.entries,
        page_number: page.page_number,
        page_size: page.page_size,
        total_pages: page.total_pages,
        q: params["q"])
    else
      page =
        ApathyDrive.Ability
        |> ApathyDrive.Repo.all
        |> Stream.filter(&(String.contains?(&1.properties["name"] |> to_string, query)))
        |> Enum.sort_by(&(&1.id))

      render(conn, "index.html",
        abilities: page,
        page_number: 1,
        page_size: length(page),
        total_pages: 1,
        q: params["q"])
    end
  end

  def index(conn, params) do
    page =
      Ability
      |> order_by([r], asc: r.id)
      |> Repo.paginate(params)

    render(conn, "index.html",
      abilities: page.entries,
      page_number: page.page_number,
      page_size: page.page_size,
      total_pages: page.total_pages)
  end

  def new(conn, _params) do
    changeset = Ability.changeset(%Ability{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"ability" => ability_params}) do
    changeset = Ability.changeset(%Ability{}, ability_params)

    case Repo.insert(changeset) do
      {:ok, _ability} ->
        conn
        |> put_flash(:info, "Ability created successfully.")
        |> redirect(to: ability_path(conn, :index))
      {:error, changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    ability = Repo.get!(Ability, id)

    class_abilities =
      id
      |> ClassAbility.ability_classes
      |> Repo.all
      |> ClassAbility.names

    monster_abilities =
      id
      |> MonsterAbility.ability_monsters
      |> Repo.all
      |> MonsterAbility.names

    render(conn, "show.html", ability: ability,
                              class_abilities: class_abilities,
                              monster_abilities: monster_abilities)
  end

  def edit(conn, %{"id" => id}) do
    ability = Repo.get!(Ability, id)
    changeset = Ability.changeset(ability)
    render(conn, "edit.html", ability: ability, changeset: changeset)
  end

  def update(conn, %{"id" => id, "ability" => ability_params}) do
    ability = Repo.get!(Ability, id)
    changeset = Ability.changeset(ability, ability_params)

    case Repo.update(changeset) do
      {:ok, ability} ->
        conn
        |> put_flash(:info, "Ability updated successfully.")
        |> redirect(to: ability_path(conn, :show, ability))
      {:error, changeset} ->
        render(conn, "edit.html", ability: ability, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    ability = Repo.get!(Ability, id)

    # Here we use delete! (with a bang) because we expect
    # it to always work (and if it does not, it will raise).
    Repo.delete!(ability)

    conn
    |> put_flash(:info, "Ability deleted successfully.")
    |> redirect(to: ability_path(conn, :index))
  end
end
