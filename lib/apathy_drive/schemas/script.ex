defmodule ApathyDrive.Script do
  use ApathyDriveWeb, :model

  alias ApathyDrive.{
    Ability,
    Character,
    CharacterTrait,
    Currency,
    Item,
    ItemInstance,
    Mobile,
    Room,
    RoomServer,
    Trait
  }

  schema "scripts" do
    field(:instructions, ApathyDrive.JSONB, default: [])

    timestamps()
  end

  def changeset(script, params \\ %{}) do
    script
    |> cast(params, ~w(instructions), ~w())
  end

  def find(id) do
    ApathyDrive.Repo.get(__MODULE__, id)
    |> Map.get(:instructions)
  end

  def execute(%Room{} = room, %{} = monster, scripts) do
    result =
      scripts
      |> Enum.find_value(fn script ->
        execute_script(room, monster, script)
      end)

    result || room
  end

  def execute_script(%Room{} = room, %{delayed: true}, _script) do
    room
  end

  def execute_script(%Room{} = room, %{} = monster, [instruction | rest])
      when is_list(instruction) do
    if room = execute_script(room, monster, instruction) do
      room
    else
      execute_script(room, monster, rest)
    end
  end

  def execute_script(%Room{} = room, %{} = monster, [instruction | script]) do
    execute_instruction(room, monster, instruction, script)
  end

  def execute_script(%Room{} = room, _monster, []), do: room
  def execute_script(_room, _monster, nil), do: false

  def execute_script(%Room{} = room, %{} = monster, script_id) when is_integer(script_id) do
    script =
      script_id
      |> find()

    execute_script(room, monster, script)
  end

  def execute_script(%Room{} = room, %{} = monster, instruction),
    do: execute_instruction(room, monster, instruction, nil)

  def execute_instruction(
        %Room{} = room,
        %{} = monster,
        %{"message" => %{"user" => user_message, "spectator" => spectator_message}},
        script
      ) do
    Mobile.send_scroll(monster, "<p>#{user_message}</p>")

    Room.send_scroll(
      room,
      "<p>#{ApathyDrive.Text.interpolate(spectator_message, %{"user" => monster})}</p>",
      [monster]
    )

    execute_script(room, monster, script)
  end

  def execute_instruction(%Room{} = room, %{} = monster, %{"message" => message}, script) do
    Mobile.send_scroll(monster, "<p>#{message}</p>")
    execute_script(room, monster, script)
  end

  def execute_instruction(
        %Room{} = room,
        %Character{} = monster,
        %{"fail_flag" => %{"failure_message" => message, "flag" => flag}},
        script
      ) do
    if Mobile.has_ability?(monster, flag) do
      Mobile.send_scroll(monster, "<p><span class='dark-green'>#{message}</p>")
      room
    else
      execute_script(room, monster, script)
    end
  end

  def execute_instruction(
        %Room{} = room,
        %Character{} = character,
        %{"add_flag" => %{"flag" => flag, "value" => value}},
        script
      ) do
    room =
      Room.update_mobile(room, character.ref, fn _room, char ->
        %Trait{id: id} = Repo.get_by!(Trait, name: flag)

        case Repo.get_by(CharacterTrait, character_id: char.id, trait_id: id) do
          %CharacterTrait{value: original} = ct ->
            ct
            |> Ecto.Changeset.change(%{value: original + value})
            |> Repo.update!()

          _ ->
            %CharacterTrait{character_id: char.id, trait_id: id, value: value}
            |> Repo.insert!()
        end

        Character.load_traits(char)
      end)

    execute_script(room, room.mobiles[character.ref], script)
  end

  def execute_instruction(
        %Room{} = room,
        %Character{} = character,
        %{"give_flag" => %{"flag" => flag, "value" => value}},
        script
      ) do
    room =
      Room.update_mobile(room, character.ref, fn _room, char ->
        %Trait{id: id} = Repo.get_by!(Trait, name: flag)

        case Repo.get_by(CharacterTrait, character_id: char.id, trait_id: id) do
          %CharacterTrait{} = ct ->
            ct
            |> Ecto.Changeset.change(%{value: value})
            |> Repo.update!()

          _ ->
            %CharacterTrait{character_id: char.id, trait_id: id, value: value}
            |> Repo.insert!()
        end

        Character.load_traits(char)
      end)

    execute_script(room, room.mobiles[character.ref], script)
  end

  def execute_instruction(
        %Room{} = room,
        %{spirit: nil},
        %{"flag_equals" => %{"flag" => _flag, "value" => _value}},
        _script
      ) do
    room
  end

  def execute_instruction(
        %Room{} = room,
        %{spirit: spirit} = monster,
        %{"flag_equals" => %{"flag" => flag, "value" => value}},
        script
      ) do
    if value == spirit.flags[flag] do
      execute_script(room, monster, script)
    else
      room
    end
  end

  def execute_instruction(
        %Room{} = room,
        %{spirit: nil},
        %{"flag_at_least" => %{"flag" => _flag, "value" => _value}},
        _script
      ) do
    room
  end

  def execute_instruction(
        room,
        monster,
        %{"flag_at_least" => %{"flag" => flag, "value" => value}},
        script
      ) do
    if monster.spirit.flags[flag] && monster.spirit.flags[flag] >= value do
      execute_script(room, monster, script)
    end
  end

  def execute_instruction(room, monster, %{"script_link" => new_script}, script) do
    execute_script(room, monster, [new_script | script])
  end

  def execute_instruction(
        %Room{} = room,
        %{alignment: alignment} = monster,
        %{"allowed_alignments" => %{"failure_message" => message, "alignments" => alignments}},
        script
      ) do
    if alignment in alignments do
      execute_script(room, monster, script)
    else
      Mobile.send_scroll(monster, "<p><span class='dark-green'>#{message}</p>")
      room
    end
  end

  def execute_instruction(
        %Room{} = room,
        %{} = character,
        %{"check_item" => %{"failure_message" => message, "item" => item_id}},
        script
      ) do
    if Enum.find(character.inventory, &(&1.id == item_id)) do
      execute_script(room, character, script)
    else
      Mobile.send_scroll(character, "<p><span class='dark-green'>#{message}</p>")
      room
    end
  end

  def execute_instruction(
        %Room{} = room,
        %{} = character,
        %{"take_item" => %{"failure_message" => message, "item" => item_id}},
        script
      ) do
    if item = Enum.find(character.inventory, &(&1.id == item_id)) do
      ItemInstance
      |> Repo.get!(item.instance_id)
      |> Repo.delete!()

      character = Character.load_items(character)
      room = put_in(room.mobiles[character.ref], character)
      execute_script(room, character, script)
    else
      Mobile.send_scroll(character, "<p><span class='dark-green'>#{message}</p>")
      room
    end
  end

  def execute_instruction(
        %Room{} = room,
        %{} = character,
        %{"monsters" => %{"failure_message" => message}},
        script
      ) do
    room.mobiles
    |> Map.values()
    |> Enum.any?(&(&1.__struct__ == Monster))
    |> if do
      execute_script(room, character, script)
    else
      Mobile.send_scroll(character, "<p><span class='dark-green'>#{message}</p>")
      room
    end
  end

  def execute_instruction(
        %Room{} = room,
        %{delayed: false} = monster,
        %{"add_delay" => delay},
        script
      ) do
    monster =
      monster
      |> ApathyDrive.TimerManager.send_after(
        {:delay_execute_script, delay * 1000, {:delay_execute_script, monster.ref, script}}
      )
      |> Map.put(:delayed, true)

    room = put_in(room.mobiles[monster.ref], monster)
    execute_script(room, monster, [])
  end

  def execute_instruction(
        %Room{} = room,
        %{hp: hp} = monster,
        %{"check_hitpoints" => %{"modifier" => amount, "failure_script" => failure_script}},
        script
      ) do
    if hp < :rand.uniform(100 + amount) do
      execute_script(room, monster, failure_script)
    else
      execute_script(room, monster, script)
    end
  end

  def execute_instruction(
        %Room{} = room,
        %{} = mobile,
        %{
          "check_stat" => %{
            "stat" => "strength",
            "modifier" => amount,
            "failure_script" => failure_script
          }
        },
        script
      ) do
    if Mobile.attribute_value(mobile, :strength) <
         :rand.uniform(100) + amount do
      execute_script(room, mobile, failure_script)
    else
      execute_script(room, mobile, script)
    end
  end

  def execute_instruction(
        %Room{} = room,
        %{} = mobile,
        %{
          "check_stat" => %{
            "stat" => "agility",
            "modifier" => amount,
            "failure_script" => failure_script
          }
        },
        script
      ) do
    if Mobile.attribute_value(mobile, :agility) <
         :rand.uniform(100) + amount do
      execute_script(room, mobile, failure_script)
    else
      execute_script(room, mobile, script)
    end
  end

  def execute_instruction(
        %Room{} = room,
        %{} = mobile,
        %{
          "check_stat" => %{
            "stat" => "intellect",
            "modifier" => amount,
            "failure_script" => failure_script
          }
        },
        script
      ) do
    if Mobile.attribute_value(mobile, :intellect) <
         :rand.uniform(100) + amount do
      execute_script(room, mobile, failure_script)
    else
      execute_script(room, mobile, script)
    end
  end

  def execute_instruction(
        %Room{} = room,
        %{} = mobile,
        %{
          "check_stat" => %{
            "stat" => "wisdom",
            "modifier" => amount,
            "failure_script" => failure_script
          }
        },
        script
      ) do
    if Mobile.attribute_value(mobile, :wisdom) <
         :rand.uniform(100) + amount do
      execute_script(room, mobile, failure_script)
    else
      execute_script(room, mobile, script)
    end
  end

  def execute_instruction(
        %Room{} = room,
        %{} = monster,
        %{
          "remote_action" => %{
            "direction" => direction,
            "message" => message,
            "room_id" => room_id
          }
        },
        script
      ) do
    room =
      Room.initiate_remote_action(
        room,
        monster,
        %{"destination" => room_id, "message" => message, "direction" => direction},
        open_remotely: true
      )

    execute_script(room, monster, script)
  end

  def execute_instruction(
        %Room{} = room,
        %{} = monster,
        %{"no_monsters" => %{"failure_message" => message}},
        script
      ) do
    if Enum.any?(room.mobiles, fn {_ref, mobile} -> mobile.__struct__ == ApathyDrive.Monster end) do
      Mobile.send_scroll(monster, "<p><span class='dark-green'>#{message}</p>")
      room
    else
      execute_script(room, monster, script)
    end
  end

  def execute_instruction(%Room{} = room, %{} = mobile, %{"give_coins" => coins}, script) do
    %{"amount" => amount, "denomination" => denomination} = coins

    denomination = String.to_existing_atom(denomination)

    if denomination in Currency.currencies() do
      mobile =
        mobile
        |> Ecto.Changeset.change(%{
          denomination => Map.get(mobile, denomination) + amount
        })
        |> Repo.update!()

      room = put_in(room.mobiles[mobile.ref], mobile)
      execute_script(room, mobile, script)
    else
      Mobile.send_scroll(
        mobile,
        "<p><span class='red'>Invalid Currency for give_coins: #{denomination}</span></p>"
      )

      execute_script(room, mobile, script)
    end
  end

  def execute_instruction(
        %Room{} = room,
        %{} = mobile,
        %{"give_item" => item_id},
        script
      ) do
    item = Repo.get!(Item, item_id)

    room =
      if item.weight <= Character.max_encumbrance(mobile) - Character.encumbrance(mobile) do
        %ItemInstance{
          item_id: item_id,
          room_id: nil,
          character_id: mobile.id,
          equipped: false,
          hidden: false
        }
        |> Repo.insert!()

        mobile = Character.load_items(mobile)
        put_in(room.mobiles[mobile.ref], mobile)
      else
        %ItemInstance{
          item_id: item_id,
          room_id: room.id,
          character_id: nil,
          equipped: false,
          hidden: false
        }
        |> Repo.insert!()

        Room.load_items(room)
      end

    execute_script(room, mobile, script)
  end

  def execute_instruction(
        %Room{} = room,
        %{} = character,
        %{"spawn_monster" => monster_id},
        script
      ) do
    room = ApathyDrive.MonsterSpawning.spawn_monster(room, monster_id) || room

    execute_script(room, character, script)
  end

  def execute_instruction(
        %Room{} = room,
        %{} = monster,
        %{"min_level" => %{"failure_message" => message, "level" => level}},
        script
      ) do
    if monster.level < level do
      Mobile.send_scroll(monster, "<p><span class='dark-green'>#{message}</p>")
      room
    else
      execute_script(room, monster, script)
    end
  end

  def execute_instruction(
        %Room{} = room,
        %Character{} = character,
        %{"add_experience" => exp},
        script
      ) do
    character =
      character
      |> Character.add_experience(exp)

    room = put_in(room.mobiles[character.ref], character)
    execute_script(room, character, script)
  end

  def execute_instruction(%Room{} = room, %{} = monster, %{"cast_ability" => ability_id}, script) do
    case Ability.find(ability_id) do
      nil ->
        Mobile.send_scroll(
          monster,
          "<p><span class='red'>Not Implemented: Ability ##{ability_id}</span></p>"
        )

        execute_script(room, monster, script)

      %Ability{} = ability ->
        ability =
          ability
          |> Map.put(:energy, 0)

        room = Ability.execute(room, monster.ref, ability, [monster.ref])
        monster = room.mobiles[monster.ref]
        execute_script(room, monster, script)
    end
  end

  def execute_instruction(
        %Room{} = room,
        %{} = monster,
        %{"room_item" => %{"failure_message" => failure_message, "item" => item_id}},
        script
      ) do
    if Enum.find(room.items, &(&1.id == item_id)) do
      execute_script(room, monster, script)
    else
      Mobile.send_scroll(monster, "<p><span class='dark-green'>#{failure_message}</p>")
      room
    end
  end

  def execute_instruction(
        %Room{} = room,
        %{spirit: nil} = monster,
        %{"price" => %{"failure_message" => failure_message, "price_in_copper" => _price}},
        _script
      ) do
    Mobile.send_scroll(monster, "<p><span class='dark-green'>#{failure_message}</p>")
    room
  end

  def execute_instruction(
        %Room{} = room,
        %Character{} = character,
        %{
          "price" => %{"failure_message" => failure_message, "price_in_copper" => price_in_copper}
        },
        script
      ) do
    if Currency.wealth(character) >= price_in_copper do
      char_currency = Currency.subtract(character, price_in_copper)

      character =
        character
        |> Ecto.Changeset.change(%{
          runic: char_currency.runic,
          platinum: char_currency.platinum,
          gold: char_currency.gold,
          silver: char_currency.silver,
          copper: char_currency.copper
        })
        |> Repo.update!()

      room = put_in(room.mobiles[character.ref], character)
      execute_script(room, character, script)
    else
      Mobile.send_scroll(character, "<p><span class='dark-green'>#{failure_message}</p>")
      room
    end
  end

  def execute_instruction(room, monster, %{"random" => scripts}, script) do
    roll = :rand.uniform(100)

    number =
      scripts
      |> Map.keys()
      |> Enum.map(&String.to_integer/1)
      |> Enum.sort()
      |> Enum.find(&(&1 >= roll))

    if script do
      send(self(), {:execute_script, monster.ref, script})
    end

    case scripts["#{number}"] do
      script_id when is_integer(script_id) ->
        found = find(script_id)

        execute_script(room, monster, found)

      instructions ->
        execute_script(room, monster, instructions)
    end
  end

  def execute_instruction(%Room{} = room, %{} = monster, %{"teleport" => room_id}, script) do
    room_exit = %{
      "kind" => "Action",
      "destination" => room_id,
      "mover_message" =>
        "<span class='blue'>You vanish into thin air and reappear somewhere else!</span>",
      "from_message" => "<span class='blue'>{{Name}} vanishes into thin air!</span>",
      "to_message" => "<span class='blue'>{{Name}} appears out of thin air!</span>"
    }

    room =
      case ApathyDrive.Commands.Move.execute(room, monster, room_exit, 0) do
        %Room{} = room ->
          room

        {:error, :too_tired, room} ->
          room
      end

    room_id
    |> RoomServer.find()
    |> send({:execute_script, monster.ref, script})

    room
  end

  def execute_instruction(%Room{} = room, %{} = monster, %{"clear_item" => nil}, script) do
    Enum.each(room.items, fn item ->
      ItemInstance
      |> Repo.get!(item.instance_id)
      |> Repo.delete!()
    end)

    room = Room.load_items(room)
    execute_script(room, monster, script)
  end

  def execute_instruction(%Room{} = room, %{} = monster, %{"clear_item" => item_id}, script) do
    Enum.each(room.items, fn item ->
      if item.id == item_id do
        ItemInstance
        |> Repo.get!(item.instance_id)
        |> Repo.delete!()
      end
    end)

    room = Room.load_items(room)
    execute_script(room, monster, script)
  end

  def execute_instruction(room, monster, instruction, _script) do
    Mobile.send_scroll(
      monster,
      "<p><span class='red'>Not Implemented: #{inspect(instruction)}</span></p>"
    )

    execute_script(room, monster, [])
  end
end
