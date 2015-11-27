defmodule ApathyDrive.ItemDropController do
  use ApathyDrive.Web, :controller

  alias ApathyDrive.Item
  alias ApathyDrive.ItemDrop

  plug :scrub_params, "item_drop" when action in [:create, :update]

  def new(conn, params) do
    changeset = ItemDrop.changeset(%ItemDrop{}, params)
    render(conn, "new.html", changeset: changeset,
                             items: Item.datalist,
                             monsters: MonsterTemplate.datalist,
                             from: params["from"])
  end

  def create(conn, %{"item_drop" => item_params} = params) do
    changeset = ItemDrop.changeset(%ItemDrop{}, item_params)

    case Repo.insert(changeset) do
      {:ok, _item} ->
        conn
        |> put_flash(:info, "Item drop created successfully.")
        |> redirect(to: params["from"])
      {:error, changeset} ->
        render(conn, "new.html", changeset: changeset,
                                 items: Item.datalist,
                                 monsters: MonsterTemplate.datalist,
                                 from: params["from"])
    end
  end

  def delete(conn, %{"id" => id} = params) do
    item_drop = Repo.get!(ItemDrop, id)

    # Here we use delete! (with a bang) because we expect
    # it to always work (and if it does not, it will raise).
    Repo.delete!(item_drop)

    conn
    |> put_flash(:info, "Item Drop deleted successfully.")
    |> redirect(to: params["from"])
  end
end
