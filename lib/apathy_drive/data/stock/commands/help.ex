defmodule Commands.Help do
  use Systems.Command

  def keywords, do: ["help"]

  def execute(entity, arguments) do
    keyword = Enum.join(arguments, " ")
    case Help.find(keyword) do
      [match] ->
        send_message(entity, "scroll", "<p>#{Components.Help.value(match)}</p>")
      [] ->
        send_message(entity, "scroll", "<p>Sorry, no help is available for \"#{keyword}\".</p>")
      matches ->
        match_names = matches |> Enum.map &(Components.Name.value(&1))
        send_message(entity, "scroll", "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>")
        Enum.each match_names, fn(match_name) ->
          send_message(entity, "scroll", "<p>-- #{match_name}</p>")
        end
    end
  end
end
