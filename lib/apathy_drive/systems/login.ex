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
        existing_spirit ->
          spirit =
            existing_spirit
            |> Spirit.value

          case spirit do
            %Spirit{socket_pid: old_socket_pid} ->
              send(old_socket_pid, :go_home)
              Spirit.update_socket(spirit.pid, socket, socket_pid)
            %Monster{spirit: %Spirit{socket_pid: old_socket_pid}} ->
              send(old_socket_pid, :go_home)
              Monster.update_socket(spirit.pid, socket, socket_pid)
          end
      end
    end
  end

end
