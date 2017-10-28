defmodule ApathyDrive.System do
  require Logger

  alias ApathyDrive.{Character, Repo}

  def add_admin(name) do
    case Repo.get_by(Character, name: name) do
      %Character{admin: true} ->
        Logger.info "#{name} is already an admin"
      %Character{} = character ->
        character
        |> Map.put(:admin, true)
        |> Repo.save!

        Logger.info "#{name} is now an admin"
      nil ->
        Logger.info "#{name} does not exist"
    end
  end

  def remove_admin(name) do
    case Repo.get_by(Character, name: name) do
      %Character{admin: true} = character ->
        character
        |> Map.put(:admin, false)
        |> Repo.save!

        Logger.info "#{name} is no longer an admin"
      %Character{} ->
        Logger.info "#{name} is not an admin"
      nil ->
        Logger.info "#{name} does not exist"
    end
  end
end
