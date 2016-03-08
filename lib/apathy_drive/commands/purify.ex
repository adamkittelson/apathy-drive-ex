defmodule Commands.Purify do
  use ApathyDrive.Command

  def keywords, do: ["purify"]

  def execute(mobile, _arguments) do
    room =
      mobile
      |> Mobile.room_id
      |> Room.find

    Room.purify(room)
  end
end
