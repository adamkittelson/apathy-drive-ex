defmodule Systems.Command do
  use Systems.Reload
  import Utility

  def execute(spirit, command, arguments) do
    monster = Possession.possessed(spirit)

    Components.Idle.value(spirit, 0)
    display_prompt(spirit, monster)

    case Systems.Match.first(Commands.all, :keyword_starts_with, command) do
      nil ->
        send_message(spirit, "scroll", "<p>What?</p>")
      match ->
        :"Elixir.Commands.#{Inflex.camelize(Components.Name.value(match))}".execute(spirit, monster, arguments)
    end
  end

  def display_prompt(spirit) do
    monster = Possession.possessed(spirit)
    display_prompt(spirit, monster)
  end

  def display_prompt(spirit, nil) do
    send_message(spirit, "disable", "#prompt")
    send_message(spirit, "disable", "#command")
    send_message(spirit, "scroll", "<p><span id='prompt'>[#{Systems.Trainer.total_power(spirit)}]:</span><input id='command' size='50' class='prompt'></input></p>")
    send_message(spirit, "focus", "#command")
    send_message(spirit, "up")
  end

  def display_prompt(spirit, monster) do
    send_message(spirit, "disable", "#prompt")
    send_message(spirit, "disable", "#command")
    send_message(spirit, "scroll", "<p><span id='prompt'>[HP=#{Components.HP.value(monster)}/MA=#{Components.Mana.value(monster)}]:</span><input id='command' size='50' class='prompt'></input></p>")
    send_message(spirit, "focus", "#command")
    send_message(spirit, "up")
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
