defmodule ApathyDrive.Commands.Skills do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile}

  def keywords, do: ["skills"]

  def execute(%Room{} = room, %Character{} = character, _arguments) do
    Mobile.send_scroll(character, "<p><span class='white'>You have the following skills:</span></p>")
    Mobile.send_scroll(character, "<p><span class='dark-magenta'>Level Skill</span></p>")
    display_skills(character)
    room
  end

  def display_skills(%Character{} = character) do
    character.skills
    |> Map.values
    |> Enum.each(fn(%{name: name, level: level}) ->
         level =
           level
           |> to_string
           |> String.pad_leading(5)

         Mobile.send_scroll(character, "<p><span class='dark-cyan'>#{level} #{name}</span></p>")
       end)
  end

end
