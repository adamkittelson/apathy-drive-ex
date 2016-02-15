defmodule ApathyDrive.Command do
  defstruct name: nil, keywords: nil, module: nil
  require Logger
  alias ApathyDrive.{Mobile, Ability}

  @directions ["n", "north", "ne", "northeast", "e", "east",
              "se", "southeast", "s", "south", "sw", "southwest",
               "w", "west", "nw", "northwest", "u", "up", "d", "down"]

  def all do
    [Commands.Abilities, Commands.Absorb, Commands.Ask, Commands.Attack,
     Commands.Bash, Commands.Class, Commands.Close, Commands.Construct,
     Commands.Cooldowns, Commands.Drop, Commands.Experience, Commands.Forms,
     Commands.Get, Commands.Gossip, Commands.Goto, Commands.Greet,
     Commands.Inventory, Commands.List, Commands.Lock, Commands.Look, Commands.Open,
     Commands.Possess, Commands.Protection, Commands.Remove, Commands.Reroll,
     Commands.Say, Commands.Score, Commands.Search, Commands.Spawn,
     Commands.Unpossess, Commands.Wear, Commands.Who, Commands.Turn]
  end

  def execute(mobile, command, arguments) do
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
      remote_action_exit = Room.remote_action_exit(room, full_command) ->
        cond do
          Mobile.confused(mobile) ->
            nil
          true ->
            ApathyDrive.Exits.RemoteAction.trigger_remote_action(room, mobile, remote_action_exit)
        end
      cmd = Systems.Match.one(Enum.map(all, &(&1.to_struct)), :keyword_starts_with, command) ->
        cmd.module.execute(mobile, arguments)
      true ->
        Mobile.use_ability(mobile, command, arguments)
    end
  end

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
