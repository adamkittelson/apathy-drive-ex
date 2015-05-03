defmodule Commands.Who do
  use ApathyDrive.Command

  def keywords, do: ["who"]

  def execute(%Spirit{} = spirit, _arguments) do
    Spirit.send_scroll(spirit, "<p><span class='dark-cyan'>Name                Class                 Possessing</span>")
    Spirit.send_scroll(spirit, "<p><span class='dark-green'>==============================================================</span></p>")

    ApathyDrive.WhoList.list
    |> Enum.each(fn(line) ->
      Spirit.send_scroll(spirit, format_line(line))
    end)
    spirit
  end

  def execute(%Monster{} = monster, _arguments) do
    Monster.send_scroll(monster, "<p><span class='dark-cyan'>Name                Class               Possessing</span>")
    Monster.send_scroll(monster, "<p><span class='dark-green'>============================================================</span></p>")

    ApathyDrive.WhoList.list
    |> Enum.each(fn(line) ->
         Monster.send_scroll(monster, format_line(line))
       end)
    monster
  end

  def format_line(%{name: name, possessing: "", school: class} = line) do
    color = Spirit.alignment_color(line)
    class = String.capitalize(class)

    "<p><span class='#{color}'>#{String.ljust(name, 20)}</span><span class='#{color}'>#{class}</span></p>"
  end

  def format_line(%{name: name, possessing: monster, school: class} = line) do
    color = Spirit.alignment_color(line)
    class =
      class
      |> String.capitalize
      |> String.ljust(20)

    "<p><span class='#{color}'>#{String.ljust(name, 20)}</span><span class='#{color}'>#{class}</span><span class='#{color}'>#{monster}</span></p>"
  end

end
