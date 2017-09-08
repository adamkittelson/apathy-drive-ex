defmodule ApathyDrive.Commands.System do
  use ApathyDrive.Command
  alias ApathyDrive.Room
  alias ApathyDrive.Commands.System

  def keywords, do: ["system", "sys"]

  def execute(%Room{} = room, %Character{admin: true} = character, args) do
    system(room, character, args)
  end

  def execute(%Room{} = room, %Character{} = character, _args) do
    Mobile.send_scroll(character, "<p>You do not have permission to do that.</p>")
    room
  end

  def system(%Room{} = room, character, ["skill" | args]) do
    System.Skill.execute(room, character, args)
  end

  def system(%Room{} = room, character, ["area" | args]) do
    System.Area.execute(room, character, args)
  end

  def system(%Room{} = room, character, ["room" | args]) do
    System.Room.execute(room, character, args)
  end

  def system(%Room{} = room, character, ["ability" | args]) do
    System.Ability.execute(room, character, args)
  end

  def system(%Room{} = room, character, args) do
    System.Misc.execute(room, character, args)
  end

end
