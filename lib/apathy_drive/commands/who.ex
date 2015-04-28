defmodule Commands.Who do
  use ApathyDrive.Command

  def keywords, do: ["who"]

  def execute(%Spirit{} = spirit, _arguments) do
    Spirit.send_scroll(spirit, "<p><span class='dark-cyan'>Name                Possessing</span>")
    Spirit.send_scroll(spirit, "<p><span class='dark-green'>================================</span></p>")

    spirit
    |> online_entities
    |> Enum.each(fn(line) ->
         Spirit.send_scroll(spirit, line)
       end)
    spirit
  end

  def execute(%Monster{} = monster, _arguments) do
    Monster.send_scroll(monster, "<p><span class='dark-cyan'>Name                Possessing</span>")
    Monster.send_scroll(monster, "<p><span class='dark-green'>================================</span></p>")

    monster
    |> online_entities
    |> Enum.each(fn(line) ->
         Monster.send_scroll(monster, line)
       end)
    monster
  end

  def online_entities(entity) do
    subscribers = (ApathyDrive.PubSub.subscribers("spirits:online") -- [self])
                  |> Enum.map(&(GenServer.call(&1, :value)))

    (subscribers ++ [entity])
    |> Enum.sort_by(fn %Spirit{name: name} ->
                         name
                       %Monster{spirit: %Spirit{name: name}} ->
                         name
                    end)
    |> Enum.map(fn %Spirit{name: name} = spirit ->
                    "<p><span class='#{Spirit.alignment_color(spirit)}'>#{name}</span></p>"
                   %Monster{name: monster_name, spirit: %Spirit{name: name} = spirit} ->
                    color = Spirit.alignment_color(spirit)
                    "<p><span class='#{color}'>#{String.ljust(name, 20)}</span><span class='#{color}'>#{monster_name}</span></p>"
                end)
  end

end
