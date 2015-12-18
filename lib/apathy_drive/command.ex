defmodule ApathyDrive.Command do
  defstruct name: nil, keywords: nil, module: nil
  require Logger
  alias ApathyDrive.Mobile

  @directions ["n", "north", "ne", "northeast", "e", "east",
              "se", "southeast", "s", "south", "sw", "southwest",
               "w", "west", "nw", "northwest", "u", "up", "d", "down"]

  def all do
    :code.all_loaded
    |> Enum.map(fn{module, _} -> to_string(module) end)
    |> Enum.filter(&(String.starts_with?(&1, "Elixir.Commands.") and !String.ends_with?(&1, "Test")))
    |> Enum.map(&String.to_atom/1)
  end

  def execute(mobile, command, arguments) do
    send(mobile, :display_prompt)

    room =
      mobile
      |> Mobile.room_id
      |> Room.find

    full_command = Enum.join([command | arguments], " ")

    cond do
      command in @directions ->
        ApathyDrive.Exit.move(mobile, Room.direction(command))
      scripts = Room.command(room, full_command) ->
        cond do
          Mobile.confused(mobile) ->
            nil
          true ->
            scripts = Enum.map(scripts, &ApathyDrive.Script.find/1)

            Mobile.execute_script(mobile, scripts)
        end
      command_exit = Room.command_exit(room, full_command) ->
        cond do
          Mobile.confused(mobile) ->
            nil
          Mobile.held(mobile) ->
            nil
          true ->
            ApathyDrive.Exits.Command.move_via_command(room, mobile, command_exit)
        end
      cmd = Systems.Match.one(Enum.map(all, &(&1.to_struct)), :keyword_starts_with, command) ->
        cmd.module.execute(mobile, arguments)
      true ->
        Mobile.use_ability(mobile, command, arguments)
    end
  end

  # def execute(%Spirit{monster: nil} = spirit, command, arguments) do
  #   spirit
  #   |> Map.put(:idle, 0)
  #   |> Systems.Prompt.display
  #
  #   room = Spirit.find_room(spirit)
  #
  #   command_exit = room.exits
  #                  |> Enum.find(fn(ex) ->
  #                       ex["kind"] == "Command" and Enum.member?(ex["commands"], [command | arguments] |> Enum.join(" "))
  #                     end)
  #
  #   cond do
  #     command_exit ->
  #       ApathyDrive.Exits.Command.move_via_command(room, spirit, command_exit)
  #     true ->
  #       case Systems.Match.one(Enum.map(all, &(&1.to_struct)), :keyword_starts_with, command) do
  #         nil ->
  #           Spirit.send_scroll(spirit, "<p>What?</p>")
  #           spirit
  #         %ApathyDrive.Command{module: Commands.Reroll} ->
  #           if command == "reroll" do
  #             Commands.Reroll.execute(spirit, [])
  #           else
  #             Spirit.send_scroll(spirit, "<p>What?</p>")
  #             spirit
  #           end
  #         match ->
  #           match.module.execute(spirit, arguments)
  #       end
  #   end
  # end
  #
  # def execute(%Spirit{monster: monster} = spirit, command, arguments) do
  #   Monster.execute_command(monster, command, arguments)
  #
  #   spirit
  #   |> Map.put(:idle, 0)
  # end
  #
  # def execute(%Monster{} = monster, command, arguments) do
  #   Systems.Prompt.display(monster)
  #
  #   ability = monster.abilities
  #             |> Enum.filter(fn(%Ability{command: cmd}) ->
  #                  cmd == String.downcase(command)
  #                end)
  #             |> select_ability
  #
  #   if ability do
  #     Ability.execute(monster, ability, Enum.join(arguments, " "))
  #   else
  #     room = Monster.find_room(monster)
  #
  #     command_exit = room.exits
  #                    |> Enum.find(fn(ex) ->
  #                         ex["kind"] == "Command" and Enum.member?(ex["commands"], [command | arguments] |> Enum.join(" "))
  #                       end)
  #
  #     remote_action_exit = room.exits
  #                          |> Enum.find(fn(ex) ->
  #                               ex["kind"] == "RemoteAction" and Enum.member?(ex["commands"], [command | arguments] |> Enum.join(" "))
  #                             end)
  #
  #     cond do
  #       command_exit ->
  #         cond do
  #           Monster.confuse(monster) ->
  #             monster
  #           Monster.held(monster) ->
  #             monster
  #           true ->
  #             ApathyDrive.Exits.Command.move_via_command(room, monster, command_exit)
  #         end
  #       remote_action_exit ->
  #         if Monster.confuse(monster) do
  #           monster
  #         else
  #           ApathyDrive.Exits.RemoteAction.trigger_remote_action(room, monster, remote_action_exit)
  #           monster
  #         end
  #       true ->
  #         case Systems.Match.one(Enum.map(all, &(&1.to_struct)), :keyword_starts_with, command) do
  #           nil ->
  #             Monster.send_scroll(monster, "<p>What?</p>")
  #             monster
  #           match ->
  #             if Monster.confuse(monster) do
  #               monster
  #             else
  #               match.module.execute(monster, arguments)
  #             end
  #         end
  #     end
  #   end
  # end

  def select_ability([]),                     do: nil
  def select_ability([%Ability{} = ability]), do: ability
  def select_ability(abilities) do
    if Enum.all?(abilities, fn(ability) -> Map.has_key?(ability.properties, "attack_chance") end) do
      roll = :random.uniform(100)

      abilities
      |> Enum.sort_by(&(&1.properties["attack_chance"]))
      |> Enum.find(fn(%Ability{properties: %{"attack_chance" => chance}}) ->
           chance >= roll
         end)
    else
      abilities
      |> Enum.random
    end
  end


  defmacro __using__(_opts) do
    quote do
      import Systems.Text
      alias ApathyDrive.Mobile

      def name do
        __MODULE__
        |> Atom.to_string
        |> String.split(".")
        |> List.last
        |> Inflex.underscore
      end

      def to_struct do
        %ApathyDrive.Command{name: name, keywords: keywords, module: __MODULE__}
      end
    end
  end

end
