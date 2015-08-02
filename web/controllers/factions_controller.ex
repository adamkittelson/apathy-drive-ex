defmodule ApathyDrive.FactionController do
  use ApathyDrive.Web, :controller
  alias ApathyDrive.Repo
  import Ecto.Query

  def angels(conn, _params) do
    render conn, "angels.html", abilities: abilities("Angel")
  end

  def demons(conn, _params) do
    render conn, "demons.html", abilities: abilities("Demon")
  end

  def elementals(conn, _params) do
    render conn, "elementals.html", abilities: abilities("Elemental")
  end

  defp abilities(faction) do
    (from a in Ability,
      where: a.faction == ^faction,
      select: a)
    |> Repo.all
    |> Enum.reduce(%{}, fn(ability, abilities) ->
         abilities = Map.put_new(abilities, ability.level, [])
         update_in(abilities, [ability.level], &([ability | &1]))
       end)
  end

end
