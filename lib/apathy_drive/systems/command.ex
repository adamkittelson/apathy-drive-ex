defmodule Systems.Command do
  use Systems.Reload
  import Utility

  def execute(spirit, command, arguments) do
    monster = Possession.possessed(spirit)

    Components.Idle.value(spirit, 0)
    display_prompt(spirit, monster)

    execute(spirit, monster, command, arguments)
  end

  def execute(spirit, nil, command, arguments) do
    execute_command(spirit, nil, command, arguments)
  end

  def execute(spirit, monster, command, arguments) do
    ability = monster
              |> Components.Abilities.value
              |> Enum.find(fn(ability) ->
                   Components.Module.value(ability).properties(monster)[:command] == String.downcase(command)
                 end)

    if ability do
      Components.Module.value(ability).execute(monster, Enum.join(arguments, " "))
    else
      execute_command(spirit, monster, command, arguments)
    end
  end

  def execute_command(spirit, monster, command, arguments) do
    case Systems.Match.one(Commands.all, :keyword_starts_with, command) do
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
    send_message(spirit, "scroll", "<p><span id='prompt'>[#{Systems.Trainer.spirit_power(spirit)}]:</span><input id='command' size='50' class='prompt'></input></p>")
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
      import Systems.Text

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
