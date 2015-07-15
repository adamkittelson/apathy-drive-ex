defmodule ApathyDrive.RoomController do
  use ApathyDrive.Web, :controller
  import Ecto.Query

  plug :scrub_params, "room" when action in [:create, :update]

  def index(conn, %{"q" => query} = params) do
    query = "%#{query}%"

    page =
      Room
      |> where([r], ilike(r.name, ^query))
      |> order_by([r], asc: r.id)
      |> Repo.paginate(params)

    render(conn, "index.html",
      rooms: page.entries,
      page_number: page.page_number,
      page_size: page.page_size,
      total_pages: page.total_pages,
      q: params["q"])
  end

  def index(conn, params) do
    page =
      Room
      |> order_by([r], asc: r.id)
      |> Repo.paginate(params)

    render(conn, "index.html",
      rooms: page.entries,
      page_number: page.page_number,
      page_size: page.page_size,
      total_pages: page.total_pages)
  end

  def new(conn, _params) do
    changeset = Room.changeset(%Room{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"room" => room_params}) do
    room_params = update_in room_params["lair_monsters"], fn
      nil ->
        nil
      lair_monsters ->
        lair_monsters
        |> String.replace(~r/[^\d,]/, "")
        |> String.split(",")
        |> Enum.map(&String.to_integer/1)
    end

    changeset = Room.changeset(%Room{}, room_params)

    if changeset.valid? do
      Repo.insert!(changeset)

      conn
      |> put_flash(:info, "Room created successfully.")
      |> redirect(to: room_path(conn, :index))
    else
      render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    room = Repo.get(Room, id)
    render(conn, "show.html", room: room)
  end

  def edit(conn, %{"id" => id}) do
    room = Repo.get(Room, id)
    changeset = Room.changeset(room)
    render(conn, "edit.html", room: room, changeset: changeset)
  end

  def update(conn, %{"id" => id, "room" => room_params}) do
    room = Repo.get(Room, id)

    room_params = update_in room_params["lair_monsters"], fn
      nil ->
        nil
      lair_monsters ->
        lair_monsters
        |> String.replace(~r/[^\d,]/, "")
        |> String.split(",")
        |> Enum.map(&String.to_integer/1)
    end

    changeset = Room.changeset(room, room_params)

    if changeset.valid? do
      Repo.update!(changeset)

      ApathyDrive.PubSub.broadcast!("rooms:#{id}", {:room_updated, changeset})

      conn
      |> put_flash(:info, "Room updated successfully.")
      |> redirect(to: room_path(conn, :index))
    else
      render(conn, "edit.html", room: room, changeset: changeset)
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
