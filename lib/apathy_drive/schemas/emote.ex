defmodule ApathyDrive.Emote do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Emote, Item, Match, Mobile, Room, Text}

  schema "emotes" do
    field(:command, :string)
    field(:user_message, :string)
    field(:target_message, :string)
    field(:spectator_message, :string)
    field(:targeted, :boolean)

    field(:keywords, :any, virtual: true)
  end

  def match_by_name(name, []) do
    Repo.get_by(__MODULE__, command: name, targeted: false)
  end

  def match_by_name(name, _target) do
    Repo.get_by(__MODULE__, command: name, targeted: true)
  end

  def execute(%Emote{targeted: false} = emote, room, character, _target) do
    Mobile.send_scroll(
      character,
      "<p><span style='color:lightskyblue'><i>#{emote.user_message}</i></span></p>"
    )

    Room.send_scroll(
      room,
      "<p><span style='color:lightskyblue'><i>#{
        Text.interpolate(emote.spectator_message, %{"user" => character})
      }</i></span></p>",
      [character]
    )
  end

  def execute(%Emote{targeted: true} = emote, room, character, %Item{} = item) do
    user_message =
      emote.user_message
      |> String.replace("{{target}}", "your {{target}}")
      |> Text.interpolate(%{"user" => character, "target" => item})

    Mobile.send_scroll(
      character,
      "<p><span style='color:lightskyblue'><i>#{user_message}</i></span></p>"
    )

    spectator_message =
      emote.spectator_message
      |> String.replace("{{target}}", "{{user:his/her/their}} {{target}}")
      |> Text.interpolate(%{"user" => character, "target" => item})

    Room.send_scroll(
      room,
      "<p><span style='color:lightskyblue'><i>#{spectator_message}</i></span></p>",
      [character]
    )
  end

  def execute(%Emote{targeted: true} = emote, room, character, %{} = target) do
    user_message =
      emote.user_message
      |> Text.interpolate(%{"user" => character, "target" => target})

    Mobile.send_scroll(
      character,
      "<p><span style='color:lightskyblue'><i>#{user_message}</i></span></p>"
    )

    target_message =
      emote.target_message
      |> Text.interpolate(%{"user" => character})

    Mobile.send_scroll(
      target,
      "<p><span style='color:lightskyblue'><i>#{target_message}</i></span></p>"
    )

    spectator_message =
      emote.spectator_message
      |> Text.interpolate(%{"user" => character, "target" => target})

    Room.send_scroll(
      room,
      "<p><span style='color:lightskyblue'><i>#{spectator_message}</i></span></p>",
      [character, target]
    )
  end

  def execute(%Emote{targeted: true}, _room, character, nil) do
    Mobile.send_scroll(character, "<p>You don't see that here!</p>")
  end

  def execute(%Emote{targeted: true} = emote, room, character, target) when is_binary(target) do
    target = find_item(character, target) || Room.find_mobile_in_room(room, character, target)

    execute(emote, room, character, target)
  end

  defp find_item(%{inventory: items, equipment: equipment}, item) do
    item =
      (items ++ equipment)
      |> Match.one(:keyword_starts_with, item)

    case item do
      nil ->
        nil

      %{} = item ->
        item
    end
  end
end
