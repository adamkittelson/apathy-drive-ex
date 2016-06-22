defmodule ApathyDrive.LairController do
  use ApathyDrive.Web, :controller

  alias ApathyDrive.{LairMonster, Room}

  plug :scrub_params, "lair_monster" when action in [:create, :update]

  def new(conn, params) do
    changeset = LairMonster.changeset(%LairMonster{}, params)
    render(conn, "new.html", changeset: changeset,
                             rooms: Room.datalist,
                             monsters: MonsterTemplate.datalist,
                             from: params["from"])
  end

  def create(conn, %{"lair_monster" => lair_params} = params) do
    changeset = LairMonster.changeset(%LairMonster{}, lair_params)

    case Repo.insert(changeset) do
      {:ok, _item} ->
        conn
        |> put_flash(:info, "Lair monster created successfully.")
        |> redirect(to: params["from"])
      {:error, changeset} ->
        render(conn, "new.html", changeset: changeset,
                                 rooms: Room.datalist,
                                 monsters: MonsterTemplate.datalist,
                                 from: params["from"])
    end
  end

  def delete(conn, %{"id" => id} = params) do
    lair_monster = Repo.get!(LairMonster, id)

    # Here we use delete! (with a bang) because we expect
    # it to always work (and if it does not, it will raise).
    Repo.delete!(lair_monster)

    conn
    |> put_flash(:info, "Lair Monster deleted successfully.")
    |> redirect(to: params["from"])
  end
end
