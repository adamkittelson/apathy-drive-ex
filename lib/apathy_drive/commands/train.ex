defmodule ApathyDrive.Commands.Train do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Directory, Repo, Trainer}

  def keywords, do: ["train"]

  def execute(%Room{} = room, %Character{} = character, _args) do
    Character.show_talent_tree(character, room)
    room
    # cond do
    #   !Trainer.trainer?(room) ->
    #     message = "<p>You must be in an appropriate training room to train!</p>"
    #     Mobile.send_scroll(character, message)
    #     room

    #   room.trainer.class_id && room.trainer.class_id != character.class_id ->
    #     change_class(room, character, room.trainer.class_id)

    #   :else ->
    #     message = "<p>You are already training in this class.</p>"
    #     Mobile.send_scroll(character, message)
    #     room
    # end
  end

  def change_class(room, character, class_id) do
    Room.update_mobile(room, character.ref, fn _room, character ->
      character =
        character
        |> Ecto.Changeset.change(%{
          class_id: class_id
        })
        |> Repo.update!()
        |> Character.load_class()
        |> Character.load_race()
        |> Character.load_items()
        |> Character.load_abilities()
        |> Character.set_title()
        |> Character.update_exp_bar()

      Directory.add_character(%{
        name: character.name,
        evil_points: character.evil_points,
        room: character.room_id,
        ref: character.ref,
        title: character.title
      })

      message = "<p>You are now a #{character.class.class.name}.</p>"

      Mobile.send_scroll(character, message)
    end)
  end
end
