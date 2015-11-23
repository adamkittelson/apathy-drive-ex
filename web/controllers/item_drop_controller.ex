defmodule ApathyDrive.ItemDropController do
  use ApathyDrive.Web, :controller
  import Ecto.Query

  alias ApathyDrive.Item
  alias ApathyDrive.ItemDrop

  plug :scrub_params, "item" when action in [:create, :update]

  def new(conn, _params) do
    changeset = Item.changeset(%Item{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"item" => item_params}) do
    changeset = Item.changeset(%Item{}, item_params)

    case Repo.insert(changeset) do
      {:ok, _item} ->
        conn
        |> put_flash(:info, "Item created successfully.")
        |> redirect(to: item_path(conn, :index))
      {:error, changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def edit(conn, %{"id" => id}) do
    item = Repo.get!(Item, id)
    changeset = Item.changeset(item)
    render(conn, "edit.html", item: item, changeset: changeset)
  end

  def update(conn, %{"id" => id, "item" => item_params}) do
    item = Repo.get!(Item, id)
    changeset = Item.changeset(item, item_params)

    case Repo.update(changeset) do
      {:ok, item} ->
        conn
        |> put_flash(:info, "Item updated successfully.")
        |> redirect(to: item_path(conn, :show, item))
      {:error, changeset} ->
        render(conn, "edit.html", item: item, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id} = params) do
    IO.puts "DELETING!!!"
    item_drop = Repo.get!(ItemDrop, id)

    # Here we use delete! (with a bang) because we expect
    # it to always work (and if it does not, it will raise).
    Repo.delete!(item_drop)

    conn
    |> put_flash(:info, "Item Drop deleted successfully.")
    |> redirect(to: params["from"])
  end
end
