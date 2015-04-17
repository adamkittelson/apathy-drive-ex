defmodule Systems.Login do

  def login(socket, socket_pid, id) do
    spirit = ApathyDrive.Repo.get(Spirit, id)

    if spirit do
      case :global.whereis_name(:"spirit_#{spirit.id}") do
        :undefined ->
          spirit
          |> Map.put(:socket, socket)
          |> Map.put(:socket_pid, socket_pid)
          |> Spirit.login
        spirit ->
          spirit
          |> Spirit.socket(socket)

          Spirit.value(spirit)
      end
    end
  end

end
