defmodule ApathyDrive.MonsterController do
  use ApathyDrive.Web, :controller
  import Ecto.Query

  alias ApathyDrive.Repo
  alias ApathyDrive.ItemDrop

  plug :scrub_params, "monster_template" when action in [:create, :update]

  def index(conn, %{"q" => query} = params) do
    query = "%#{query}%"

    page =
      MonsterTemplate
      |> where([r], ilike(r.name, ^query))
      |> order_by([r], asc: r.id)
      |> Repo.paginate(params)

    render(conn, "index.html",
      monsters: page.entries,
      page_number: page.page_number,
      page_size: page.page_size,
      total_pages: page.total_pages,
      q: params["q"])
  end

  def index(conn, params) do
    page =
      MonsterTemplate
      |> order_by([r], asc: r.id)
      |> Repo.paginate(params)

    render(conn, "index.html",
      monsters: page.entries,
      page_number: page.page_number,
      page_size: page.page_size,
      total_pages: page.total_pages)
  end

  def new(conn, _params) do
    changeset = MonsterTemplate.changeset(%MonsterTemplate{})
    render(conn, "new.html", changeset: changeset,
                             genders: MonsterTemplate.genders,
                             alignments: MonsterTemplate.alignments)
  end

  def create(conn, %{"monster_template" => monster_template_params}) do
    changeset = MonsterTemplate.changeset(%MonsterTemplate{}, monster_template_params)

    case Repo.insert(changeset) do
      {:ok, _monster_template} ->
        conn
        |> put_flash(:info, "MonsterTemplate created successfully.")
        |> redirect(to: monster_path(conn, :index))
      {:error, changeset} ->
        render(conn, "new.html", changeset: changeset,
                                 genders: MonsterTemplate.genders,
                                 alignments: MonsterTemplate.alignments)
    end
  end

  def show(conn, %{"id" => id}) do
    monster = Repo.get(MonsterTemplate, id)
    drops =
      id
      |> ItemDrop.monster_drops
      |> ItemDrop.names

    lairs =
      monster
      |> Ecto.assoc(:lairs)
      |> Ecto.Query.preload(:room)
      |> Repo.all

    monster_abilities =
      monster
      |> Ecto.assoc(:monster_abilities)
      |> Ecto.Query.preload(:ability)
      |> Repo.all

    render(conn, "show.html", monster: monster,
                              drops: drops,
                              lairs: lairs,
                              monster_abilities: monster_abilities)
  end

  def edit(conn, %{"id" => id}) do
    monster = Repo.get(MonsterTemplate, id)
    changeset = MonsterTemplate.changeset(monster)
    render(conn, "edit.html", monster: monster,
                              changeset: changeset,
                              genders: MonsterTemplate.genders,
                              alignments: MonsterTemplate.alignments)
  end

  def update(conn, %{"id" => id, "monster_template" => monster_params}) do
    mt = Repo.get(MonsterTemplate, id)

    changeset = MonsterTemplate.changeset(mt, monster_params)

    if changeset.valid? do
      Repo.update!(changeset)

      ApathyDrive.PubSub.broadcast!("monster_templates:#{id}", {:monster_template_updated, changeset})

      conn
      |> put_flash(:info, "Monster Template updated successfully.")
      |> redirect(to: monster_path(conn, :show, id))
    else
      render(conn, "edit.html", monster: mt,
                                changeset: changeset,
                                genders: MonsterTemplate.genders,
                                alignments: MonsterTemplate.alignments)
    end
  end

  def delete(conn, %{"id" => id}) do
    room = Repo.get(Room, id)
    Repo.delete!(room)

    conn
    |> put_flash(:info, "Room deleted successfully.")
    |> redirect(to: room_path(conn, :index))
  end
end
