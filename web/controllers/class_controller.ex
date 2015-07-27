defmodule ApathyDrive.ClassController do
  use ApathyDrive.Web, :controller

  alias ApathyDrive.Class

  plug :scrub_params, "class" when action in [:create, :update]

  def index(conn, _params) do
    classes = Repo.all(Class)
    render(conn, "index.html", classes: classes)
  end

  def new(conn, _params) do
    changeset = Class.changeset(%Class{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"class" => class_params}) do
    changeset = Class.changeset(%Class{}, class_params)

    if changeset.valid? do
      Repo.insert!(changeset)

      conn
      |> put_flash(:info, "Class created successfully.")
      |> redirect(to: class_path(conn, :index))
    else
      render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    class = Repo.get!(Class, id)
    render(conn, "show.html", class: class)
  end

  def edit(conn, %{"id" => id}) do
    class = Repo.get!(Class, id)
    changeset = Class.changeset(class)
    render(conn, "edit.html", class: class, changeset: changeset)
  end

  def update(conn, %{"id" => id, "class" => class_params}) do
    class = Repo.get!(Class, id)
    changeset = Class.changeset(class, class_params)

    if changeset.valid? do
      Repo.update!(changeset)

      conn
      |> put_flash(:info, "Class updated successfully.")
      |> redirect(to: class_path(conn, :index))
    else
      render(conn, "edit.html", class: class, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    class = Repo.get!(Class, id)
    Repo.delete!(class)

    conn
    |> put_flash(:info, "Class deleted successfully.")
    |> redirect(to: class_path(conn, :index))
  end
end
