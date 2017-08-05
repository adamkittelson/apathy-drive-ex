defmodule ApathyDrive.System do
  require Logger

  alias ApathyDrive.Repo

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

        Logger.info "#{name} is no longer an admin"
      %Spirit{} ->
        Logger.info "#{name} is not an admin"
      nil ->
        Logger.info "#{name} does not exist"
    end
  end
end
