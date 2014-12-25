defmodule Systems.Monster do
  use Systems.Reload
  import Systems.Text
  import Utility
  import BlockTimer
  use Timex

  def spawn_monster(monster) do
    limit = Components.Module.value(monster).limit
    monster_name = Components.Name.value(monster)

    if !limit_reached?(monster_name, limit) do
      {:ok, entity} = Entity.init
      Entity.add_component(entity, Components.Name,        Components.Name.get_name(monster))
      Entity.add_component(entity, Components.Description, Components.Module.value(monster).description)
      Entity.add_component(entity, Components.Types, ["monster"])
      Entity.add_component(entity, Components.Limbs,  Components.Module.value(monster).limbs)
      Entity.add_component(entity, Components.Skills, %{})
      Entity.add_component(entity, Components.Flags, %{})
      Entity.add_component(entity, Components.Effects, %{})
      Components.Skills.set_base_skills(entity, Components.Module.value(monster).skills)
      Entity.add_component(entity, Components.Stats,  Components.Module.value(monster).stats)
      Entity.add_component(entity, Components.HP, Systems.HP.max(entity))
      Entity.add_component(entity, Components.Mana, Systems.Mana.max(entity))
      Entity.add_component(entity, Components.Hunting, [])
      Entity.add_component(entity, Components.Combat, %{"break_at" => Date.convert(Date.now, :secs)})
      Entity.add_component(entity, Components.Module, Components.Module.value(monster))
      Entity.add_component(entity, Components.Abilities, [])
      Entity.add_component(entity, Components.Experience, 0)
      Entity.add_component(entity, Components.Level, 1)
      Entity.add_component(entity, Components.Items, [])
      Entity.add_component(entity, Components.Gender, Components.Module.value(monster).gender)
      case Components.Module.value(monster).alignment do
        "good" ->
          Entity.add_component(entity, Components.Alignment, -75.0)
        "neutral" ->
          Entity.add_component(entity, Components.Alignment, 0.0)
        "evil" ->
          Entity.add_component(entity, Components.Alignment, 75.0)
      end
      equip_monster(entity)
      Components.Abilities.reset_abilities(entity)
      Entity.add_to_type_collection(entity)

      entity
    end
  end

  def spawn_monster(monster, room) do
    monster = spawn_monster(monster)
    if monster do
      Components.Monsters.add_monster(room, monster)

      display_enter_message(room, monster)

      if Components.Characters.get_characters(room) |> Enum.any? do
        Systems.Monster.monsters_in_room(room, monster)
        |> Enum.each(fn(monster_in_room) ->
             Systems.Aggression.monster_entered(monster_in_room, room)
           end)

        Systems.Aggression.monster_entered(monster, room)
      end
    end

    monster
  end

  def limit_reached?(monster, nil), do: false
  def limit_reached?(monster, limit) do
    limit <= Monsters.count(monster)
  end

  def equip_monster(monster) do
    Components.Module.value(monster).items
    |> Enum.each(fn(item_name) ->
         item_name
         |> ItemTemplates.find_by_id
         |> Systems.Item.spawn_item(monster)

         Systems.Item.equip(monster, item_name)
       end)
  end

  def display_enter_message(room, monster) do
    message = Components.Module.value(monster).enter_message
              |> interpolate(%{
                   "name" => Components.Name.value(monster),
                   "direction" => direction(room)
                 })
              |> capitalize_first

    observers(room, monster)
    |> Enum.each(fn(observer) ->
      send_message(observer,"scroll", "<p><span class='dark-green'>#{message}</span></p>")
    end)
  end

  def display_enter_message(room, monster, direction) do
    message = Components.Module.value(monster).enter_message
              |> interpolate(%{
                   "name" => Components.Name.value(monster),
                   "direction" => enter_direction(direction)
                 })
              |> capitalize_first

    observers(room, monster)
    |> Enum.each(fn(observer) ->
      send_message(observer,"scroll", "<p><span class='dark-green'>#{message}</span></p>")
    end)
  end

  def display_exit_message(room, monster) do
    message = Components.Module.value(monster).exit_message
              |> interpolate(%{
                   "name" => Components.Name.value(monster),
                   "direction" => ""
                 })
              |> capitalize_first

    observers(room, monster)
    |> Enum.each(fn(observer) ->
      send_message(observer, "scroll", "<p><span class='dark-green'>#{message}</span></p>")
    end)
  end

  def display_exit_message(room, monster, direction) do
    message = Components.Module.value(monster).exit_message
              |> interpolate(%{
                   "name" => Components.Name.value(monster),
                   "direction" => exit_direction(direction)
                 })
              |> capitalize_first

    observers(room, monster)
    |> Enum.each(fn(observer) ->
      send_message(observer, "scroll", "<p><span class='dark-green'>#{message}</span></p>")
    end)
  end

  def display_death_message(room, monster) do
    message = Components.Module.value(monster).death_message
              |> interpolate(%{
                   "name" => Components.Name.value(monster)
                 })
              |> capitalize_first

    observers(room, monster)
    |> Enum.each(fn(observer) ->
      send_message(observer, "scroll", "<p>#{message}</p>")
    end)
  end

  def pursue(room, monster, direction) do
    :random.seed(:os.timestamp)
    room
    |> monsters_in_room(monster)
    |> Enum.each fn(observer) ->
         if !Possession.possessor(observer) and (Components.Hunting.value(observer) |> Enum.member?(monster)) do
           if :random.uniform(100) < Components.Module.value(observer).chance_to_follow do
             Systems.Room.move(nil, observer, direction)
           end
         end
       end
  end

  def observers(room, monster) do
    spirits_in_room(room) ++ monsters_in_room(room, monster)
  end

  def monsters_in_room(room, monster) do
    room
    |> Components.Monsters.get_monsters
    |> Enum.reject(&(&1 == monster))
  end

  def spirits_in_room(room) do
    Characters.online
    |> Enum.filter(&(Parent.of(&1) == room))
    |> Enum.reject(&(!!Possession.possessed(&1)))
  end

  def exit_direction("up"),      do: "upwards"
  def exit_direction("down"),    do: "downwards"
  def exit_direction(direction), do: "to the #{direction}"

  def enter_direction("up"),      do: "above"
  def enter_direction("down"),    do: "below"
  def enter_direction(direction), do: "the #{direction}"

  def direction(room) do
    :random.seed(:os.timestamp)

    case Components.Exits.value(room) do
      nil ->
        "nowhere"
      {:error, :bad_module} ->
        "nowhere"
      {:error, :not_found} ->
        "nowhere"
      exits ->
        direction = exits
                    |> Enum.map(&(&1["direction"]))
                    |> Enum.shuffle
                    |> List.first

        enter_direction(direction)
    end
  end

  defmacro __using__(_opts) do
    quote do
      use Systems.Reload
      import Systems.Text
      import Utility
      import BlockTimer

      def name do
        __MODULE__
        |> Atom.to_string
        |> String.split(".")
        |> List.last
        |> Inflex.underscore
        |> String.replace("_", " ")
      end

      def keywords do
        String.split(name)
      end

      def description,   do: nil
      def death_message, do: ~s({{name}} drops dead before you.)
      def enter_message, do: ~s(<span class='yellow'>{{name}}</span> walks in from {{direction}}.)
      def exit_message,  do: ~s(<span class='yellow'>{{name}}</span> walks off to {{direction}}.)
      def abilities,     do: []
      def greeting,      do: "The #{name} completely ignores you."
      def gender,        do: nil
      def limit,         do: nil

      def stats do
        %{"strength"  => 1,
          "agility"   => 1,
          "intellect" => 1,
          "willpower" => 1,
          "health"    => 1,
          "charm"     => 1}
      end

      def skills do
        %{
          "melee" => 1,
          "dodge" => 1
         }
      end

      def hit_verbs, do: ["attack", "assault", "strike"]

      def limbs, do: humanoid

      def humanoid do
        %{
          "head" => %{
            "type" => "head",
            "fatal" => true,
            "items" => []
          },
          "torso" => %{
            "type" => "torso",
            "fatal" => true,
            "items" => []
          },
          "right arm" => %{
            "type" => "arm",
            "attached" => "right hand",
            "items"    => []
          },
          "right hand" => %{
            "type" => "hand",
            "items"    => []
          },
          "left arm" => %{
            "type" => "arm",
            "attached" => "left hand",
            "items"    => []
          },
          "left hand" => %{
            "type" => "hand",
            "items"    => []
          },
          "right leg" => %{
            "type" => "leg",
            "attached" => "right foot",
            "items"    => []
          },
          "right foot" => %{
            "type" => "foot",
            "items"    => []
          },
          "left leg" => %{
            "type" => "leg",
            "attached" => "left foot",
            "items"    => []
          },
          "left foot" => %{
            "type" => "foot",
            "items"    => []
          }
        }
      end

      def beast do
        %{
          "head" => %{
            "type" => "head",
            "fatal" => true,
            "items" => []
          },
          "torso" => %{
            "type" => "torso",
            "fatal" => true,
            "items" => []
          },
          "right foreleg" => %{
            "type" => "arm",
            "attached" => "right forepaw",
            "items"    => []
          },
          "right forepaw" => %{
            "type" => "hand",
            "items"    => []
          },
          "left foreleg" => %{
            "type" => "arm",
            "attached" => "left forepaw",
            "items"    => []
          },
          "left forepaw" => %{
            "type" => "hand",
            "items"    => []
          },
          "right rear leg" => %{
            "type" => "leg",
            "attached" => "right rear paw",
            "items"    => []
          },
          "right rear paw" => %{
            "type" => "foot",
            "items"    => []
          },
          "left rear leg" => %{
            "type" => "leg",
            "attached" => "left rear paw",
            "items"    => []
          },
          "left rear paw" => %{
            "type" => "foot",
            "items"    => []
          }
        }
      end

      def items, do: []

      def chance_to_follow, do: 0

      def damage do
        %{"impact"=>1.0}
      end

      defoverridable [description:      0,
                      death_message:    0,
                      enter_message:    0,
                      exit_message:     0,
                      stats:            0,
                      skills:           0,
                      limbs:            0,
                      abilities:        0,
                      hit_verbs:        0,
                      name:             0,
                      items:            0,
                      greeting:         0,
                      chance_to_follow: 0,
                      damage:           0,
                      gender:           0,
                      limit:            0]
    end
  end

end
