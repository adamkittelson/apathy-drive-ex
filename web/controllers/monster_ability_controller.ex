defmodule ApathyDrive.MonsterAbilityController do
  use ApathyDrive.Web, :controller

  alias ApathyDrive.{Ability, MonsterAbility, MonsterTemplate}

  plug :scrub_params, "class_ability" when action in [:create, :update]

  def new(conn, params) do
    changeset = MonsterAbility.changeset(%MonsterAbility{}, params)
    render(conn, "new.html", changeset: changeset,
                             monsters: MonsterTemplate.datalist,
                             abilities: Ability.datalist,
                             from: params["from"])
  end

  def create(conn, %{"monster_ability" => monster_ability_params} = params) do
    changeset = MonsterAbility.changeset(%MonsterAbility{}, monster_ability_params)

    case Repo.insert(changeset) do
      {:ok, _item} ->
        conn
        |> put_flash(:info, "Monster Ability created successfully.")
        |> redirect(to: params["from"])
      {:error, changeset} ->
        render(conn, "new.html", changeset: changeset,
                                 monsters: MonsterTemplate.datalist,
                                 abilities: Ability.datalist,
                                 from: params["from"])
    end
  end

  def delete(conn, %{"id" => id} = params) do
    monster_ability = Repo.get!(MonsterAbility, id)

    # Here we use delete! (with a bang) because we expect
    # it to always work (and if it does not, it will raise).
    Repo.delete!(monster_ability)

    conn
    |> put_flash(:info, "Monster Ability deleted successfully.")
    |> redirect(to: params["from"])
  end
end
