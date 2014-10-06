defmodule Commands.Help do
  use Systems.Command

  def keywords, do: ["help"]

  def execute(spirit, _monster, arguments) do
    keyword = Enum.join(arguments, " ")
    case Help.find(keyword) do
      [match] ->
        send_message(spirit, "scroll", "<p>#{Components.Help.value(match)}</p>")
      [] ->
        send_message(spirit, "scroll", "<p>Sorry, no help is available for \"#{keyword}\".</p>")
      matches ->
        match_names = matches |> Enum.map &(Components.Name.value(&1))
        send_message(spirit, "scroll", "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>")
        Enum.each match_names, fn(match_name) ->
          send_message(spirit, "scroll", "<p>-- #{match_name}</p>")
        end
    end
  end
end
