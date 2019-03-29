defmodule ApathyDriveWeb.AdminChannelHelper do
  alias ApathyDrive.{Character, Repo}

  def authorize(socket, token) do
    case Phoenix.Token.verify(socket, "character", token, max_age: 1_209_600) do
      {:ok, character_id} ->
        case Repo.get!(Character, character_id) do
          nil ->
            {:error, %{reason: "unauthorized"}}

          %Character{admin: true} = character ->
            {:ok, character}

          # Character has been reset, probably due to a game wipe
          %Character{name: nil} ->
            {:error, %{reason: "unauthorized"}}
        end

      {:error, _} ->
        {:error, %{reason: "unauthorized"}}
    end
  end
end
