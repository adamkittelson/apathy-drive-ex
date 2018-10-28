defmodule ApathyDrive.Commands.Tell do
  use ApathyDrive.Command
  alias ApathyDrive.{ChannelHistory, Directory, Mobile, Repo}

  def keywords, do: ["tell", "whisper"]

  def execute(%Room{} = room, %Character{} = character, [player | message]) do
    message =
      message
      |> Enum.join(" ")

    case Directory.find(player) do
      {:local, name, room, ref} ->
        room
        |> RoomServer.find()
        |> RoomServer.tell(character.name, nil, ref, message)

        Character.send_chat(
          character,
          "<p><span class='red'>You tell #{name}:</span> #{Character.sanitize(message)}"
        )

      {:remote, name, game} ->
        case Gossip.send_tell(character.name, game, name, message) do
          {:error, error} ->
            Mobile.send_scroll(
              character,
              "<p><span class='red'>#{name}@#{game}:</span> I'm unable to receive tells 😭 - #{
                Character.sanitize(error)
              }"
            )

          :ok ->
            message =
              "<p><span class='red'>You tell #{name}@#{game}:</span> #{
                Character.sanitize(message)
              }</p>"

            Repo.insert!(%ChannelHistory{
              character_id: character.id,
              message: message
            })

            Character.send_chat(character, message)
        end

      _ ->
        Mobile.send_scroll(character, "<p>Who?</p>")
    end

    room
  end

  def execute(%Room{} = room, %Character{} = character, _) do
    Mobile.send_scroll(character, "<p>Who?</p>")
    room
  end
end
