defmodule ApathyDriveWeb.Admin.ClassController do
  use ApathyDriveWeb, :controller

  alias ApathyDrive.Admin
  alias ApathyDrive.Class

  def index(conn, _params) do
    classes = Admin.list_classes()
    render(conn, "index.html", classes: classes)
  end

  def new(conn, _params) do
    changeset = Admin.change_class(%Class{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"class" => class_params}) do
    case Admin.create_class(class_params) do
      {:ok, class} ->
        conn
        |> put_flash(:info, "Class created successfully.")
        |> redirect(to: Routes.class_path(conn, :edit, class))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    class = Admin.get_class!(id)
    abilities = Admin.get_abilities_for_class(class)
    render(conn, "show.html", class: class, abilities: abilities)
  end

  def edit(conn, %{"id" => id}) do
    class = Admin.get_class!(id)
    changeset = Admin.change_class(class)
    render(conn, "edit.html", class: class, changeset: changeset)
  end

  def update(conn, %{"id" => id, "class" => class_params}) do
    class = Admin.get_class!(id)

    case Admin.update_class(class, class_params) do
      {:ok, class} ->
        conn
        |> put_flash(:info, "Class updated successfully.")
        |> redirect(to: Routes.class_path(conn, :edit, class))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", class: class, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    class = Admin.get_class!(id)
    {:ok, _class} = Admin.delete_class(class)

    conn
    |> put_flash(:info, "Class deleted successfully.")
    |> redirect(to: Routes.class_path(conn, :index))
  end
end
