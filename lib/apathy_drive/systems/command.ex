defmodule Systems.Command do
  use Systems.Reload
  import Utility

  def execute(spirit, command, arguments) do
    character = Possession.possessed(spirit)
    Components.Idle.value(spirit, 0)
    display_prompt(character || spirit)

    case Systems.Match.first(Commands.all, :keyword_starts_with, command) do
      nil ->
        send_message(character || spirit, "scroll", "<p>What?</p>")
      match ->
        :"Elixir.Commands.#{Inflex.camelize(Components.Name.value(match))}".execute(character || spirit, arguments)
    end
  end

  def display_prompt(character) do
    send_message(character, "disable", "#prompt")
    send_message(character, "disable", "#command")
    if Entity.has_component?(character, Components.HP) do
      send_message(character, "scroll", "<p><span id='prompt'>[HP=#{Components.HP.value(character)}/MA=#{Components.Mana.value(character)}]:</span><input id='command' size='50' class='prompt'></input></p>")
    else
      send_message(character, "scroll", "<p><span id='prompt'>[#{Systems.Trainer.total_power(character)}]:</span><input id='command' size='50' class='prompt'></input></p>")
    end
    send_message(character, "focus", "#command")
    send_message(character, "up")
  end

  defmacro __using__(_opts) do
    quote do
      use Systems.Reload
      import Utility
      import BlockTimer

      def name do
        __MODULE__
        |> Atom.to_string
        |> String.split(".")
        |> List.last
        |> Inflex.underscore
      end
    end
  end

end
