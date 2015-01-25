defmodule Systems.Command do
  use Systems.Reload
  import Utility

  def execute(spirit, command, arguments) when is_pid(spirit) do
    monster = Possession.possessed(spirit)

    Spirit.reset_idle(spirit)
    Systems.Prompt.display(spirit, monster)

    spirit
    |> Spirit.value
    |> execute(command, arguments)
  end

  def execute(%Spirit{} = spirit, command, arguments) do
    execute_command(spirit, command, arguments)
  end

  def execute(%Monster{} = monster, command, arguments) do
    ability = monster.abilities
              |> Enum.find(fn(ability) ->
                   ability.properties(monster)[:command] == String.downcase(command)
                 end)

    if ability do
      Components.Module.value(ability).execute(monster, Enum.join(arguments, " "))
    else
      execute_command(monster, command, arguments)
    end
  end

  def execute_command(%Spirit{} = spirit, command, arguments) do
    room = spirit.room_id
           |> Room.find
           |> Room.value

    command_exit = room.exits
                   |> Enum.find(fn(ex) ->
                        ex.kind == "Command" and Enum.member?(ex.commands, [command | arguments] |> Enum.join(" "))
                      end)

    remote_action_exit = room.exits
                         |> Enum.find(fn(ex) ->
                              ex.kind == "RemoteAction" and Enum.member?(ex.commands, [command | arguments] |> Enum.join(" "))
                            end)

    cond do
      command_exit ->
        ApathyDrive.Exits.Command.move_via_command(spirit, room, command_exit)
      remote_action_exit ->
        ApathyDrive.Exits.RemoteAction.trigger_remote_action(spirit, room, remote_action_exit)
      true ->
        case Systems.Match.one(Commands.all, :keyword_starts_with, command) do
          nil ->
            send_message(spirit.pid, "scroll", "<p>What?</p>")
          match ->
            :"Elixir.Commands.#{Inflex.camelize(Components.Name.value(match))}".execute(spirit, arguments)
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
