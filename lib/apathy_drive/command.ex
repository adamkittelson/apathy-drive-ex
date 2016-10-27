defmodule ApathyDrive.Command do
  defstruct name: nil, keywords: nil, module: nil
  require Logger
  alias ApathyDrive.{Ability, Commands, Monster, Match, Room, RoomServer}

  @callback execute(%Room{}, %Monster{}, list) :: %Room{}

  @directions ["n", "north", "ne", "northeast", "e", "east",
              "se", "southeast", "s", "south", "sw", "southwest",
               "w", "west", "nw", "northwest", "u", "up", "d", "down"]

  def all do
    [Commands.Abilities, Commands.Absorb, Commands.Ask, Commands.Attack,
     Commands.Bash, Commands.Buy, Commands.Class, Commands.Close,
     Commands.Cooldowns, Commands.Delve, Commands.Drop, Commands.Experience,
     Commands.Get, Commands.Gossip, Commands.Greet,
     Commands.Inventory, Commands.List, Commands.Lock, Commands.Look, Commands.Open,
     Commands.Remove, Commands.Return, Commands.Say, Commands.Score,
     Commands.Search, Commands.Sell, Commands.System, Commands.Wear, Commands.Who]
  end

  def execute(%Room{} = room, monster_ref, command, arguments) do
    full_command = Enum.join([command | arguments], " ")

    monster = room.mobiles[monster_ref]

    cond do
      command in @directions ->
        Commands.Move.execute(room, monster, command)
      command_exit = Room.command_exit(room, full_command) ->
        Commands.Move.execute(room, monster, Map.put(command_exit, "kind", "Action"))
      remote_action_exit = Room.remote_action_exit(room, full_command) ->
        Room.initiate_remote_action(room, monster, remote_action_exit)
      scripts = Room.command(room, full_command) ->
        execute_room_command(room, monster, scripts)
      cmd = Match.one(Enum.map(all, &(&1.to_struct)), :keyword_starts_with, command) ->
        cmd.module.execute(room, monster, arguments)
      true ->
        ability =
          monster.abilities
          |> Enum.find(fn(ability) ->
               ability["command"] == String.downcase(command)
             end)

        if ability do
          Ability.execute(room, monster.ref, ability, Enum.join(arguments, " "))
        else
          Monster.send_scroll(monster, "<p>What?</p>")
          room
        end
    end
  end

  defp execute_room_command(room, monster, scripts) do
    if Monster.confused(room, monster) do
      room
    else
      scripts = Enum.map(scripts, &ApathyDrive.Script.find/1)
      ApathyDrive.Script.execute(room, monster, scripts)
    end
  end

  defmacro __using__(_opts) do
    quote do
      import ApathyDrive.Text
      alias ApathyDrive.{Monster, Room, RoomServer}

      @behaviour ApathyDrive.Command

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
