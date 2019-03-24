defmodule ApathyDrive.Command do
  defstruct name: nil, keywords: nil, module: nil
  require Logger

  alias ApathyDrive.{
    Ability,
    AbilityDamageType,
    AbilityTrait,
    Character,
    Commands,
    Monster,
    Match,
    Mobile,
    Room,
    RoomServer
  }

  @callback execute(%Room{}, %Monster{}, list) :: %Room{}

  @directions [
    "n",
    "north",
    "ne",
    "northeast",
    "e",
    "east",
    "se",
    "southeast",
    "s",
    "south",
    "sw",
    "southwest",
    "w",
    "west",
    "nw",
    "northwest",
    "u",
    "up",
    "d",
    "down"
  ]

  def all do
    [
      Commands.Abilities,
      Commands.Ask,
      Commands.Auto,
      Commands.Attack,
      Commands.Backstab,
      Commands.Bash,
      Commands.Buy,
      Commands.Cooldowns,
      Commands.Close,
      Commands.Dismiss,
      Commands.Drop,
      Commands.Experience,
      Commands.Get,
      Commands.Gossip,
      Commands.Grapevine,
      Commands.Greet,
      Commands.Help,
      Commands.Hire,
      Commands.Inventory,
      Commands.Join,
      Commands.Invite,
      Commands.Leave,
      Commands.List,
      Commands.Look,
      Commands.Open,
      Commands.Party,
      Commands.Remove,
      Commands.Reply,
      Commands.Return,
      Commands.Say,
      Commands.Search,
      Commands.Sell,
      Commands.Skills,
      Commands.Sneak,
      Commands.System,
      Commands.Tell,
      Commands.Top,
      Commands.Train,
      Commands.Use,
      Commands.Wear,
      Commands.Who
    ]
  end

  def execute(%Room{} = room, monster_ref, command, arguments, reattempt \\ false) do
    full_command = Enum.join([command | arguments], " ")

    monster = room.mobiles[monster_ref]

    unless reattempt, do: Logger.info("#{monster && monster.name} command: #{full_command}")

    {time, response} =
      :timer.tc(fn ->
        cond do
          is_nil(monster) ->
            {:error, :not_here, room}

          command in @directions ->
            Commands.Move.execute(room, monster, command, reattempt)

          command_exit = Room.command_exit(room, full_command) ->
            Commands.Move.execute(
              room,
              monster,
              Map.put(command_exit, "kind", "Action"),
              reattempt
            )

          remote_action_exit = Room.remote_action_exit(room, full_command) ->
            Room.initiate_remote_action(room, monster, remote_action_exit)

          scripts = Room.command(room, full_command) ->
            execute_room_command(room, monster, scripts)

          cmd = Match.one(Enum.map(all(), & &1.to_struct), :match_keyword, command) ->
            cmd.module.execute(room, monster, arguments)

          abilities = monster.abilities[String.downcase(command)] ->
            ability = Ability.select_ability(monster, abilities)

            Ability.execute(room, monster.ref, ability, Enum.join(arguments, " "))

          scroll = useable_scroll(monster, String.downcase(command)) ->
            ability = scroll.traits["Learn"]

            traits = AbilityTrait.load_traits(ability.id)

            traits =
              case AbilityDamageType.load_damage(ability.id) do
                [] ->
                  traits

                damage ->
                  Map.put(traits, "Damage", damage)
              end
              |> Map.put("DestroyItem", scroll.instance_id)
              |> Map.update("RequireItems", [scroll.instance_id], &[scroll.instance_id | &1])

            ability = Map.put(ability, :traits, traits)

            Ability.execute(room, monster.ref, ability, Enum.join(arguments, " "))

          true ->
            Mobile.send_scroll(monster, "<p>What?</p>")
            room
        end
      end)

    if match?(%Room{}, response) do
      Logger.info(
        "#{full_command} executed for #{monster && monster.name} in #{time / 1000 / 1000} seconds"
      )
    end

    response
  end

  defp useable_scroll(%Character{} = monster, command) do
    monster
    |> ApathyDrive.Commands.Abilities.scrolls()
    |> Enum.find(&(&1.traits["Learn"].command == command))
  end

  defp useable_scroll(_monster, _command), do: nil

  defp execute_room_command(room, monster, scripts) do
    if Mobile.confused(monster, room) do
      room
    else
      scripts = Enum.map(scripts, &ApathyDrive.Script.find/1)
      ApathyDrive.Script.execute(room, monster, scripts)
    end
  end

  defmacro __using__(_opts) do
    quote do
      import ApathyDrive.Text
      alias ApathyDrive.{Character, Mobile, Room, RoomServer}

      @behaviour ApathyDrive.Command

      def name do
        __MODULE__
        |> Atom.to_string()
        |> String.split(".")
        |> List.last()
        |> Inflex.underscore()
      end

      def to_struct do
        %ApathyDrive.Command{name: name(), keywords: keywords(), module: __MODULE__}
      end
    end
  end
end
