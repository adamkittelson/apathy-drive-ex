defmodule Commands.Help do
  use Systems.Command

  def keywords, do: ["help"]

  def execute({:player, player}, arguments) do
    keyword = Enum.join(arguments, " ")
    case Help.find(keyword) do
      [match] ->
        Players.send_message(player, ["scroll", Components.Help.value(match)])
      [] ->
        Players.send_message(player, ["scroll", "<p>Sorry, no help is available for \"#{keyword}\".</p>"])
      matches ->
        match_names = matches |> Enum.map &(Components.Name.value(&1))
        Players.send_message(player, ["scroll", "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"])
        Enum.each match_names, fn(match_name) ->
          Players.send_message(player,  ["scroll", "<p>-- #{match_name}</p>"])
        end
    end
  end

  def execute(entity, arguments) do
    keyword = Enum.join(arguments, " ")
    case Help.find(keyword) do
      [match] ->
        Components.Player.send_message(entity, ["scroll", Components.Help.value(match)])
      [] ->
        Components.Player.send_message(entity, ["scroll", "<p>Sorry, no help is available for \"#{keyword}\".</p>"])
      matches ->
        match_names = matches |> Enum.map &(Components.Name.value(&1))
        Components.Player.send_message(entity, ["scroll", "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"])
        Enum.each match_names, fn(match_name) ->
          Components.Player.send_message(entity, ["scroll", "<p>-- #{match_name}</p>"])
        end
    end
  end
end
