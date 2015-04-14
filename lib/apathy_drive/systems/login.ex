defmodule Systems.Login do

  def login(socket, id) do
    spirit = ApathyDrive.Repo.get(Spirit, id)

    if spirit do
      case :global.whereis_name(:"spirit_#{spirit.id}") do
        :undefined ->
          spirit
          |> Map.put(:socket, socket)
          |> Spirit.login
        spirit ->
          spirit
          |> Spirit.socket(socket)

          Spirit.value(spirit)
      end
    end
  end

end
