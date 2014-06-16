defmodule Commands.Use do
  use Systems.Command

  def keywords, do: ["use", "cast"]

  def execute(entity, arguments) do
    arguments = Enum.join(arguments, " ")
    if String.contains?(arguments, " at ") do
      [ability_name, target_name] = arguments |> String.split(" at ")
    else
      ability_name = arguments
      target_name = nil
    end

    case Systems.Match.all(Abilities.all, :name_contains, ability_name) do
      [match] ->
        Components.Module.value(match).execute(entity, target_name)
      [] ->
        Components.Player.send_message(entity, ["scroll", "<p>You don't know how to peform \"#{ability_name}\".</p>"])
      matches ->
        match_names = matches |> Enum.map &(Components.Name.value(&1))
        Components.Player.send_message(entity, ["scroll", "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"])
        Enum.each match_names, fn(match_name) ->
          Components.Player.send_message(entity, ["scroll", "<p>-- #{match_name}</p>"])
        end
    end
  end
end
