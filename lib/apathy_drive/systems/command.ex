defmodule Systems.Command do
  use Systems.Reload
  import Utility

  def execute(spirit, command, arguments) do
    monster = Possession.possessed(spirit)

    Components.Idle.value(spirit, 0)
    Systems.Prompt.display(spirit, monster)

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
    command_exit = spirit
                   |> Parent.of
                   |> Room.exits
                   |> Enum.find(fn(ex) ->
                        ex["kind"] == "Command" and Enum.member?(ex["commands"], [command | arguments] |> Enum.join(" "))
                      end)

    remote_action_exit = spirit
                         |> Parent.of
                         |> Room.exits
                         |> Enum.find(fn(ex) ->
                              ex["kind"] == "RemoteAction" and Enum.member?(ex["commands"], [command | arguments] |> Enum.join(" "))
                            end)

    cond do
      command_exit ->
        Systems.Exits.Command.move_via_command(spirit, monster, Parent.of(spirit), command_exit)
      remote_action_exit ->
        Systems.Exits.RemoteAction.trigger_remote_action(spirit, monster, Parent.of(spirit), remote_action_exit)
      true ->
        case Systems.Match.one(Commands.all, :keyword_starts_with, command) do
          nil ->
            send_message(spirit, "scroll", "<p>What?</p>")
          match ->
            :"Elixir.Commands.#{Inflex.camelize(Components.Name.value(match))}".execute(spirit, monster, arguments)
        end
    end
  end

  def display_prompt(spirit) do
    monster = Possession.possessed(spirit)
    Systems.Prompt.display(spirit, monster)
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
