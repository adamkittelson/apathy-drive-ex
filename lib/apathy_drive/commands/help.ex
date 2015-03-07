defmodule Commands.Help do
  use ApathyDrive.Command

  def keywords, do: ["help"]

  def execute(spirit, _monster, arguments) do
    keyword = Enum.join(arguments, " ")
    case Help.find(keyword) do
      [match] ->
        Spirit.send_scroll(spirit, "<p>#{Components.Help.value(match)}</p>")
      [] ->
        Spirit.send_scroll(spirit, "<p>Sorry, no help is available for \"#{keyword}\".</p>")
      matches ->
        match_names = matches |> Enum.map &(Components.Name.value(&1))
        Spirit.send_scroll(spirit, "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>")
        Enum.each match_names, fn(match_name) ->
          Spirit.send_scroll(spirit, "<p>-- #{match_name}</p>")
        end
    end
  end
end
