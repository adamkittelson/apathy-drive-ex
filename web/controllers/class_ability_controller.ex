defmodule ApathyDrive.ClassAbilityController do
  use ApathyDrive.Web, :controller

  alias ApathyDrive.{Ability, ClassAbility, Class}

  plug :scrub_params, "class_ability" when action in [:create, :update]

  def new(conn, params) do
    changeset = ClassAbility.changeset(%ClassAbility{}, params)
    render(conn, "new.html", changeset: changeset,
                             classes: Class.datalist,
                             abilities: Ability.datalist,
                             from: params["from"])
  end

  def create(conn, %{"class_ability" => class_ability_params} = params) do
    changeset = ClassAbility.changeset(%ClassAbility{}, class_ability_params)

    case Repo.insert(changeset) do
      {:ok, _item} ->
        conn
        |> put_flash(:info, "Class Ability created successfully.")
        |> redirect(to: params["from"])
      {:error, changeset} ->
        render(conn, "new.html", changeset: changeset,
                                 classes: Class.datalist,
                                 abilities: Ability.datalist,
                                 from: params["from"])
    end
  end

  def delete(conn, %{"id" => id} = params) do
    class_ability = Repo.get!(ClassAbility, id)

    # Here we use delete! (with a bang) because we expect
    # it to always work (and if it does not, it will raise).
    Repo.delete!(class_ability)

    conn
    |> put_flash(:info, "Class Ability deleted successfully.")
    |> redirect(to: params["from"])
  end
end
