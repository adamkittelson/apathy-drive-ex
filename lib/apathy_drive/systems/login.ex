defmodule Systems.Login do

  def login(socket, id) do
    spirit = ApathyDrive.Repo.get(Spirit, id)

    if spirit do
      case :global.whereis_name(:"spirit_#{spirit.id}") do
        :undefined ->
          spirit = Map.put(spirit, :socket, socket)

          spirit = Spirit.login(spirit)

          Spirit.activate_hint(spirit, "movement")
          Spirit.activate_hint(spirit, "name")
          spirit
        spirit ->
          Spirit.socket(spirit, socket)
          spirit
      end
    end
  end

end
