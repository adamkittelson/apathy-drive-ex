defmodule ApathyDrive.Commands.Join do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, CharacterClass, Party, Repo, Room, Trainer}

  def keywords, do: ["join", "follow"]

  def execute(%Room{} = room, %Character{} = character, []) do
    if Trainer.join_room?(room) do
      CharacterClass
      |> Repo.get_by(%{character_id: character.id, class_id: room.class_id})
      |> Repo.preload(:class)
      |> case do
        nil ->
          cc =
            %CharacterClass{character_id: character.id, class_id: room.class_id, level: 1}
            |> Repo.insert!()
            |> Repo.preload(:class)

          Mobile.send_scroll(character, "<p>You join the #{cc.class.name}'s guild!</p>")

          Room.update_mobile(room, character.ref, fn _room, character ->
            Character.change_class(character, room.class_id)
          end)

        %CharacterClass{class: class} ->
          Mobile.send_scroll(
            character,
            "<p>You are already a member of the #{class.name}'s guild!</p>"
          )

          room
      end
    else
      Mobile.send_scroll(character, "<p>What?</p>")
      room
    end
  end

  def execute(%Room{} = room, %Character{ref: ref} = character, arguments) do
    query = Enum.join(arguments)

    case Room.find_mobile_in_room(room, character, query) do
      %Character{invitees: invitees} = target ->
        cond do
          Party.size(room, target) > 5 ->
            Mobile.send_scroll(
              character,
              "<p>#{Mobile.colored_name(target)}'s party is already full.</p>"
            )

            room

          ref in invitees ->
            room =
              room
              |> Room.update_mobile(character.ref, fn _room, character ->
                Mobile.send_scroll(character, "<p>You are now following #{target.name}</p>")

                character
                |> put_in([:leader], target.ref)
              end)
              |> Room.update_mobile(target.ref, fn _room, target ->
                Mobile.send_scroll(target, "<p>#{character.name} started to follow you</p>")

                target
                |> update_in([:invitees], &List.delete(&1, ref))
              end)

            Room.update_moblist(room)
            room

          true ->
            Mobile.send_scroll(
              character,
              "<p><span class='red'>You must be invited first!</span></p>"
            )

            room
        end

      _other ->
        Mobile.send_scroll(character, "<p>You don't see #{query} here!</p>")
        room
    end
  end
end
