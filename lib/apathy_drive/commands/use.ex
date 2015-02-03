defmodule Commands.Use do
  use ApathyDrive.Command

  def keywords, do: ["use", "cast"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(spirit, monster, arguments) do
    arguments = Enum.join(arguments, " ")
    if String.contains?(arguments, " at ") do
      [ability_name, target_name] = arguments |> String.split(" at ")
    else
      ability_name = arguments
      target_name = nil
    end

    case Systems.Match.all(Abilities.all, :name_contains, ability_name) do
      [match] ->
        Components.Module.value(match).execute(monster, target_name)
      [] ->
        send_message(spirit, "scroll", "<p>You don't know how to peform \"#{ability_name}\".</p>")
      matches ->
        match_names = matches |> Enum.map &(Components.Name.value(&1))
        send_message(spirit, "scroll", "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>")
        Enum.each match_names, fn(match_name) ->
          send_message(spirit, "scroll", "<p>-- #{match_name}</p>")
        end
    end
  end
end
