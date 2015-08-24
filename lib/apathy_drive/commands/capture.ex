defmodule Commands.Capture do
  use ApathyDrive.Command

  def keywords, do: ["cap"]

  # def execute(%Spirit{} = spirit, _arguments) do
  #   Spirit.send_scroll(spirit, "<p>You must be possessing a monster to capture a lair.</p>")
  #   spirit
  # end
  # 
  # def execute(%Monster{room_id: room_id, spirit: %Spirit{faction: faction}} = monster, _arguments) do
  #   room_id
  #   |> Room.find
  #   |> send({:capture, monster: self, faction: faction})
  # 
  #   monster
  # end
  # 
end
