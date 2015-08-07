defmodule Commands.Who do
  use ApathyDrive.Command
  alias ApathyDrive.Mobile

  def keywords, do: ["who"]

  def execute(%Mobile{} = mobile, _arguments) do
    send(mobile.socket, {:scroll, "<p><span class='dark-cyan'>Name                Faction               Possessing</span></p>"})
    send(mobile.socket, {:scroll, "<p><span class='dark-green'>==============================================================</span></p>"})

    Task.start fn ->
      ApathyDrive.PubSub.subscribers("spirits:online")
      |> Enum.map(fn(pid) ->
           Mobile.data_for_who_list(pid)
         end)
      |> Enum.each(fn(line) ->
        send(mobile.socket, {:scroll, format_line(line)})
      end)
    end
  end

  def execute(%Spirit{} = spirit, _arguments) do
    Spirit.send_scroll(spirit, "<p><span class='dark-cyan'>Name                Faction               Possessing</span></p>")
    Spirit.send_scroll(spirit, "<p><span class='dark-green'>==============================================================</span></p>")

    ApathyDrive.WhoList.list
    |> Enum.each(fn(line) ->
      Spirit.send_scroll(spirit, format_line(line))
    end)
    spirit
  end

  def execute(%Monster{} = monster, _arguments) do
    Monster.send_scroll(monster, "<p><span class='dark-cyan'>Name                Faction             Possessing</span></p>")
    Monster.send_scroll(monster, "<p><span class='dark-green'>============================================================</span></p>")

    ApathyDrive.WhoList.list
    |> Enum.each(fn(line) ->
         Monster.send_scroll(monster, format_line(line))
       end)
    monster
  end

  def format_line(%{name: name, possessing: "", faction: faction} = line) do
    color = Mobile.alignment_color(line)

    "<p><span class='#{color}'>#{String.ljust(name, 20)}</span><span class='#{color}'>#{faction}</span></p>"
  end

  def format_line(%{name: name, possessing: monster, faction: faction} = line) do
    color = Mobile.alignment_color(line)
    faction = String.ljust(faction, 20)

    "<p><span class='#{color}'>#{String.ljust(name, 20)}</span><span class='#{color}'>#{faction}</span><span class='#{color}'>#{monster}</span></p>"
  end

end
