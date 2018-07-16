defmodule ApathyDriveWeb.Admin.RaceController do
  use ApathyDriveWeb, :controller

  alias ApathyDrive.Admin
  alias ApathyDrive.Race

  def index(conn, _params) do
    races = Admin.list_races()
    render(conn, "index.html", races: races)
  end

  def new(conn, _params) do
    changeset = Admin.change_race(%Race{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"race" => race_params}) do
    case Admin.create_race(race_params) do
      {:ok, race} ->
        conn
        |> put_flash(:info, "Race created successfully.")
        |> redirect(to: race_path(conn, :show, race))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    race = Admin.get_race!(id)
    render(conn, "show.html", race: race)
  end

  def edit(conn, %{"id" => id}) do
    race = Admin.get_race!(id)
    changeset = Admin.change_race(race)
    render(conn, "edit.html", race: race, changeset: changeset)
  end

  def update(conn, %{"id" => id, "race" => race_params}) do
    race = Admin.get_race!(id)

    case Admin.update_race(race, race_params) do
      {:ok, race} ->
        conn
        |> put_flash(:info, "Race updated successfully.")
        |> redirect(to: race_path(conn, :show, race))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", race: race, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    race = Admin.get_race!(id)
    {:ok, _race} = Admin.delete_race(race)

    conn
    |> put_flash(:info, "Race deleted successfully.")
    |> redirect(to: race_path(conn, :index))
  end
end
