defmodule ApathyDrive.System do
  require Logger

  alias ApathyDrive.Repo

  def drop_world! do
    ["areas", "class_abilities", "item_drops", "lair_monsters", "monster_abilities",
     "abilities", "classes", "items", "monster_templates", "rooms", "scripts"]
    |> Enum.each(&(Ecto.Adapters.SQL.query!(Repo, "DELETE FROM #{&1}", [])))
  end

  def add_admin(name) do
    case Repo.get_by(Spirit, name: name) do
      %Spirit{admin: true} ->
        Logger.info "#{name} is already an admin"
      %Spirit{} = spirit ->
        spirit
        |> Map.put(:admin, true)
        |> Repo.save!

        Logger.info "#{name} is now an admin"
      nil ->
        Logger.info "#{name} does not exist"
    end
  end

  def remove_admin(name) do
    case Repo.get_by(Spirit, name: name) do
      %Spirit{admin: true} = spirit ->
        spirit
        |> Map.put(:admin, false)
        |> Repo.save!
        |> IO.inspect

        Logger.info "#{name} is no longer an admin"
      %Spirit{} ->
        Logger.info "#{name} is not an admin"
      nil ->
        Logger.info "#{name} does not exist"
    end
  end
end
