defmodule ApathyDrive.RaceController do
  use ApathyDrive.Web, :controller

  alias ApathyDrive.Race

  plug :scrub_params, "race" when action in [:create, :update]

  def index(conn, _params) do
    races = Repo.all(Race)
    render(conn, "index.html", races: races)
  end

  def new(conn, _params) do
    changeset = Race.changeset(%Race{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"race" => race_params}) do
    changeset = Race.changeset(%Race{}, race_params)

    if changeset.valid? do
      Repo.insert!(changeset)

      conn
      |> put_flash(:info, "Race created successfully.")
      |> redirect(to: race_path(conn, :index))
    else
      render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    race = Repo.get!(Race, id)
    render(conn, "show.html", race: race)
  end

  def edit(conn, %{"id" => id}) do
    race = Repo.get!(Race, id)
    changeset = Race.changeset(race)
    render(conn, "edit.html", race: race, changeset: changeset)
  end

  def update(conn, %{"id" => id, "race" => race_params}) do
    race = Repo.get!(Race, id)
    changeset = Race.changeset(race, race_params)

    if changeset.valid? do
      Repo.update!(changeset)

      conn
      |> put_flash(:info, "Race updated successfully.")
      |> redirect(to: race_path(conn, :index))
    else
      render(conn, "edit.html", race: race, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    race = Repo.get!(Race, id)
    Repo.delete!(race)

    conn
    |> put_flash(:info, "Race deleted successfully.")
    |> redirect(to: race_path(conn, :index))
  end
end
