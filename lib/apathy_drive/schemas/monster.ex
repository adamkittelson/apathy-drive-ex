defmodule ApathyDrive.Monster do
  use Ecto.Schema
  use ApathyDriveWeb, :model

  alias ApathyDrive.{
    Ability,
    Aggression,
    AI,
    ChannelHistory,
    Character,
    Currency,
    Regeneration,
    Item,
    ItemInstance,
    KillCount,
    LimbSet,
    LootPity,
    Mobile,
    Monster,
    MonsterAbility,
    MonsterItem,
    MonsterSpawning,
    MonsterTrait,
    Party,
    Room,
    RoomMonster,
    RoomServer,
    Text,
    TimerManager,
    Trait
  }

  require Logger

  @behaviour Access
  defdelegate get_and_update(container, key, fun), to: Map
  defdelegate fetch(container, key), to: Map
  defdelegate get(container, key, default), to: Map
  defdelegate pop(container, key), to: Map

  schema "monsters" do
    field(:name, :string)
    field(:gender, :string)
    field(:grade, :string)
    field(:hostile, :boolean)
    field(:movement, :string)
    field(:chance_to_follow, :integer)
    field(:description, :string)
    field(:enter_message, :string)
    field(:exit_message, :string)
    field(:death_message, :string)
    field(:adjectives, ApathyDrive.JSONB)
    field(:questions, ApathyDrive.JSONB)
    field(:greeting, :string)
    field(:next_spawn_at, :integer)
    field(:copper, :integer, default: 0)
    field(:silver, :integer, default: 0)
    field(:gold, :integer, default: 0)
    field(:platinum, :integer, default: 0)
    field(:runic, :integer, default: 0)
    field(:experience, :integer, default: 0)
    field(:base_hp, :integer)
    field(:hp_regen, :integer)
    field(:regen_time_in_hours, :integer)
    field(:game_limit, :integer)
    field(:alignment, :string)
    field(:lawful, :boolean)
    field(:npc, :boolean)

    field(:bust_cache, :boolean, virtual: true, default: false)
    field(:last_auto_attack_at, :any, virtual: true)
    field(:owner_id, :integer, virtual: true)
    field(:leader, :any, virtual: true)
    field(:hp, :float, virtual: true, default: 1.0)
    field(:mana, :float, virtual: true, default: 1.0)
    field(:ref, :any, virtual: true)
    field(:timers, :map, virtual: true, default: %{})
    field(:effects, :map, virtual: true, default: %{})
    field(:last_effect_key, :integer, virtual: true, default: 0)
    field(:abilities, :map, virtual: true, default: %{})
    field(:strength, :integer, virtual: true)
    field(:agility, :integer, virtual: true)
    field(:intellect, :integer, virtual: true)
    field(:willpower, :integer, virtual: true)
    field(:health, :integer, virtual: true)
    field(:charm, :integer, virtual: true)
    field(:room_monster_id, :integer, virtual: true)
    field(:room_id, :integer, virtual: true)
    field(:level, :integer, virtual: true)
    field(:spawned_at, :integer, virtual: true)
    field(:zone_spawned_at, :integer, virtual: true)
    field(:ability_shift, :float, virtual: true)
    field(:ability_special, :float, virtual: true)
    field(:energy, :integer, virtual: true, default: 1000)
    field(:max_energy, :integer, virtual: true, default: 1000)
    field(:casting, :any, virtual: true)
    field(:last_tick_at, :any, virtual: true)
    field(:last_room_id, :integer, virtual: true)
    field(:auto_heal, :boolean, virtual: true, default: true)
    field(:auto_bless, :boolean, virtual: true, default: true)
    field(:auto_curse, :boolean, virtual: true, default: true)
    field(:auto_nuke, :boolean, virtual: true, default: true)
    field(:auto_flee, :boolean, virtual: true, default: false)
    field(:auto_roam, :boolean, virtual: true, default: true)
    field(:drops, :any, virtual: true, default: [])
    field(:sneaking, :boolean, virtual: true, default: false)
    field(:delete_at, :any, virtual: true)
    field(:limbs, :map, virtual: true, default: %{})
    field(:missing_limbs, {:array, :string}, virtual: true, default: [])
    field(:lore, :any, virtual: true)
    field(:follow, :boolean, virtual: true)

    timestamps()

    belongs_to(:death_ability, ApathyDrive.Ability)
    belongs_to(:limb_set, ApathyDrive.LimbSet)

    has_many(:lairs, ApathyDrive.LairMonster)
    has_many(:lair_rooms, through: [:lairs, :room])

    has_many(:monsters_traits, ApathyDrive.MonsterTrait)
    has_many(:traits, through: [:monsters_traits, :trait])
  end

  def changeset(%Monster{} = monster, params \\ %{}) do
    monster
    |> cast(params, ~w(), ~w())
  end

  def loot_wealth_in_copper(%Monster{} = monster) do
    monster
    |> Map.put(:copper, Enum.random(0..monster.copper))
    |> Map.put(:silver, Enum.random(0..monster.silver))
    |> Map.put(:gold, Enum.random(0..monster.gold))
    |> Map.put(:platinum, Enum.random(0..monster.platinum))
    |> Map.put(:runic, Enum.random(0..monster.runic))
    |> Currency.wealth()
  end

  def enemies(%Monster{} = monster, room) do
    monster.effects
    |> Map.values()
    |> Enum.filter(&Map.has_key?(&1, "Aggro"))
    |> Enum.map(&Map.get(&1, "Aggro"))
    |> Enum.filter(&(&1 in Map.keys(room.mobiles)))
    |> Enum.reject(&room.mobiles[&1].sneaking)
  end

  def hireable?(%Monster{} = monster, character, room) do
    !(Mobile.has_ability?(monster, "NonLiving") or Mobile.has_ability?(monster, "Animal") or
        Mobile.has_ability?(monster, "Undead") or monster.hostile or
        character.ref in enemies(monster, room))
  end

  def chase_exit(monster, room, destination) do
    room
    |> AI.exits_in_area(monster)
    |> Enum.find(&(&1["destination"] == destination))
  end

  def chase(%Monster{} = monster, room, character, destination) do
    with {:aggro?, true} <- {:aggro?, Aggression.enemy?(monster, character)},
         {:follow?, true} <-
           {:follow?, :rand.uniform(100) < monster.chance_to_follow and !monster.follow},
         {:exit, %{} = room_exit} <- {:exit, chase_exit(monster, room, destination)} do
      case ApathyDrive.Commands.Move.execute(room, monster, room_exit) do
        %Room{} = room ->
          room

        {:error, :too_tired, room} ->
          room
      end
    else
      _ ->
        if monster.owner_id == character.id and monster.follow do
          room_exit =
            room.exits
            |> Enum.find(&(&1["destination"] == destination))

          case room_exit do
            %{"kind" => "Command"} ->
              ApathyDrive.Commands.Move.execute(
                room,
                monster,
                Map.put(room_exit, "kind", "Action")
              )

            _ ->
              ApathyDrive.Commands.Move.execute(room, monster, room_exit)
          end
        else
          room
        end
    end
  end

  def from_room_monster(room_monster, force \\ false)

  def from_room_monster(
        %RoomMonster{id: nil, room_id: room_id, monster_id: monster_id} = rm,
        force
      ) do
    now =
      DateTime.utc_now()
      |> DateTime.to_unix()

    monster =
      Repo.get(Monster, monster_id)
      |> Map.put(:room_id, room_id)
      |> Map.put(:level, rm.level)
      |> Map.put(:spawned_at, rm.spawned_at)
      |> Map.put(:zone_spawned_at, rm.zone_spawned_at)
      |> Map.put(:delete_at, rm.delete_at)
      |> Map.put(:missing_limbs, rm.missing_limbs)
      |> Map.put(:owner_id, rm.owner_id)
      |> Map.put(:lore, ApathyDrive.ElementalLores.lore(rm.lore))

    if force or (!MonsterSpawning.limit_reached?(monster) and spawnable?(monster, now)) do
      ref = :crypto.hash(:md5, inspect(make_ref())) |> Base.encode16()

      monster
      |> Map.put(:ref, ref)
      |> generate_monster_attributes()
      |> load_limbs()
      |> load_abilities()
      |> load_traits()
      |> load_drops()
      |> TimerManager.send_after({:heartbeat, 100, {:heartbeat, ref}})
    end
  end

  def from_room_monster(%RoomMonster{id: id, monster_id: monster_id} = rm, _force) do
    monster = Repo.get(Monster, monster_id)

    attributes =
      Map.take(rm, [
        :strength,
        :agility,
        :intellect,
        :willpower,
        :health,
        :charm,
        :name,
        :spawned_at,
        :room_spawned_at,
        :delete_at,
        :missing_limbs,
        :owner_id
      ])

    ref = :crypto.hash(:md5, inspect(make_ref())) |> Base.encode16()

    monster
    |> Map.merge(attributes)
    |> Map.put(:lore, ApathyDrive.ElementalLores.lore([rm.lore]))
    |> Map.put(:room_monster_id, id)
    |> Map.put(:ref, ref)
    |> Map.put(:level, rm.level)
    |> load_limbs()
    |> load_abilities()
    |> load_traits()
    |> load_drops()
    |> TimerManager.send_after({:heartbeat, 100, {:heartbeat, ref}})
  end

  def spawnable?(%Monster{next_spawn_at: nil}, _now), do: true
  def spawnable?(%Monster{regen_time_in_hours: nil}, _now), do: true
  def spawnable?(%Monster{next_spawn_at: time}, now), do: now >= time

  def load_traits(%Monster{id: id} = monster) do
    effect =
      MonsterTrait.load_traits(id)
      |> Map.put("stack_key", "monster")

    monster
    |> Systems.Effect.add(effect)
  end

  def load_drops(%Monster{} = monster) do
    MonsterItem.load_drops(monster)
  end

  def load_abilities(%Monster{} = monster) do
    MonsterAbility.load_abilities(monster)
  end

  def load_limbs(%Monster{} = monster) do
    limbs = LimbSet.load_limbs(monster, monster.limb_set_id)

    limbs
    |> Enum.reduce(monster, fn {limb_name, limb}, monster ->
      if limb.health == 0 do
        effect = %{
          "StatusMessage" => "Your #{limb_name} is severed!",
          "stack_key" => {:severed, limb_name}
        }

        Systems.Effect.add(monster, effect)
      else
        monster
      end
    end)
    |> Map.put(:limbs, limbs)
  end

  def generate_monster_attributes(%Monster{level: level} = monster) do
    name =
      if monster.lore do
        monster.lore.name <> " " <> monster.name
      else
        monster.name
      end

    room_monster = %RoomMonster{
      level: level,
      room_id: monster.room_id,
      monster_id: monster.id,
      spawned_at: monster.spawned_at,
      zone_spawned_at: monster.zone_spawned_at,
      name: name_with_adjective(name, monster.adjectives),
      delete_at: monster.delete_at,
      owner_id: monster.owner_id
    }

    room_monster =
      [:strength, :agility, :intellect, :willpower, :health, :charm]
      |> Enum.reduce(room_monster, fn attribute, rm ->
        rm
        |> Map.put(attribute, 50)
      end)
      |> Repo.insert!()

    monster
    |> Map.merge(
      Map.take(room_monster, [:strength, :agility, :intellect, :willpower, :health, :charm, :name])
    )
    |> Map.put(:room_monster_id, room_monster.id)
  end

  def generate_monster_attributes(%Monster{} = monster), do: monster

  def pity_bonus(%Monster{grade: "weak"}), do: 10
  def pity_bonus(%Monster{grade: "normal"}), do: 100
  def pity_bonus(%Monster{grade: "strong"}), do: 1_000
  def pity_bonus(%Monster{grade: "boss"}), do: 10_000

  def name_with_adjective(name, nil), do: name
  def name_with_adjective(name, []), do: name

  def name_with_adjective(name, adjectives) do
    adjective =
      adjectives
      |> Enum.random()

    "#{adjective} #{name}"
  end

  def auto_attack_target(_monster, [], _room) do
    nil
  end

  # weak monsters attack enemy with lowest % hp remaining
  def auto_attack_target(%Monster{grade: "weak"}, enemies, room) do
    enemies
    |> Enum.map(&room.mobiles[&1])
    |> Enum.sort_by(& &1.hp)
    |> List.first()
    |> Map.get(:ref)
  end

  # normal monsters attack a random enemy
  def auto_attack_target(%Monster{grade: "normal"}, enemies, _room) do
    Enum.random(enemies)
  end

  # strong monsters attack enemy with highest % hp remaining
  def auto_attack_target(%Monster{}, enemies, room) do
    enemies
    |> Enum.map(&room.mobiles[&1])
    |> Enum.sort_by(& &1.hp)
    |> List.last()
    |> Map.get(:ref)
  end

  def drop_loot_for_character(
        %Room{} = room,
        %Monster{} = monster,
        %Character{} = character
      ) do
    Enum.reduce(monster.drops, room, fn %{chance: chance, item_id: item_id, id: drop_id}, room ->
      pity = LootPity.pity_for_character(character, drop_id)

      if :rand.uniform(100) <= chance + pity do
        item =
          %ItemInstance{
            item_id: item_id,
            room_id: room.id,
            character_id: nil,
            equipped: false,
            hidden: false,
            level: max(1, character.level),
            delete_at: Timex.shift(DateTime.utc_now(), hours: 1)
          }
          |> Repo.insert!()
          |> Repo.preload(:item)
          |> Item.from_assoc()

        Mobile.send_scroll(
          character,
          "<p>A #{Item.colored_name(item, character: character)} drops to the floor.</p>"
        )

        LootPity.reset_pity(character, drop_id)
      else
        LootPity.increase_pity(character, drop_id)
      end

      room
    end)
    # |> CraftingRecipe.drop_loot_for_character(character)
    |> Room.load_items()
  end

  defimpl ApathyDrive.Mobile, for: Monster do
    def ability_value(monster, ability) do
      Trait.get_cached(monster, ability)
    end

    def accuracy_at_level(monster, level, _room) do
      agi = attribute_at_level(monster, :agility, level)
      cha = attribute_at_level(monster, :charm, level)
      agi = agi + cha / 10
      modifier = ability_value(monster, "Accuracy")
      trunc(agi * (1 + modifier / 100))
    end

    def attribute_at_level(%Monster{} = monster, attribute, level) do
      Map.get(monster, attribute) + level - 1 +
        ability_value(monster, attribute |> to_string |> String.capitalize())
    end

    def attack_ability(monster) do
      monster.abilities
      |> Map.values()
      |> Enum.filter(&(&1.kind == "auto attack" or !is_nil(&1.chance)))
      |> case do
        [] ->
          nil

        attacks ->
          attacks =
            attacks
            |> Enum.sort_by(& &1.chance)

          attack =
            attacks
            |> Enum.find(fn attack ->
              rand = :rand.uniform(100)

              attack.chance > rand or attack == List.last(attacks)
            end)

          auto_attack? = attack.kind == "auto attack"

          if auto_attack? do
            damage = Mobile.ability_value(monster, "WeaponDamage")
            update_in(attack.traits["Damage"], &(damage ++ &1))
          else
            attack
          end
          |> Map.put(:kind, "attack")
          |> Map.put(:ignores_round_cooldown?, true)
      end
    end

    def auto_attack_target(%Monster{} = monster, room) do
      enemies = Monster.enemies(monster, room)

      Monster.auto_attack_target(monster, enemies, room)
    end

    def color(%Monster{alignment: "evil"}), do: "magenta"
    def color(%Monster{alignment: "neutral"}), do: "dark-cyan"
    def color(%Monster{alignment: "good"}), do: "grey"

    def colored_name(%Monster{name: name} = monster) do
      "<span class='#{color(monster)}'>#{name}</span>"
    end

    def confused(%Monster{effects: effects} = monster, %Room{} = room) do
      effects
      |> Map.values()
      |> Enum.find(fn effect ->
        Map.has_key?(effect, "Confusion") && effect["Confusion"] >= :rand.uniform(100)
      end)
      |> confused(monster, room)
    end

    def confused(nil, %Monster{}, %Room{}), do: false

    def confused(%{"ConfusionMessage" => message} = effect, %Monster{} = monster, %Room{} = room) do
      Mobile.send_scroll(monster, "<p>#{message}</p>")

      if effect["ConfusionSpectatorMessage"],
        do:
          Room.send_scroll(
            room,
            "<p>#{Text.interpolate(effect["ConfusionSpectatorMessage"], %{"user" => monster})}</p>",
            [monster]
          )

      true
    end

    def confused(%{}, %Monster{} = monster, %Room{} = room) do
      send_scroll(monster, "<p><span class='cyan'>You fumble in confusion!</span></p>")

      Room.send_scroll(
        room,
        "<p><span class='cyan'>#{
          Text.interpolate("{{user}} fumbles in confusion!</span></p>", %{"user" => monster})
        }</span></p>",
        [monster]
      )

      true
    end

    def crits_at_level(monster, level) do
      intellect = attribute_at_level(monster, :intellect, level)
      charm = attribute_at_level(monster, :charm, level)

      base = div(intellect * 3 + charm, 6) + level * 2

      trunc(base / (250 + base) * 100) + ability_value(monster, "Crits")
    end

    def description(monster, _observer) do
      monster.description
    end

    def detected?(_monster, sneaker, _room) do
      :rand.uniform(100) >= Mobile.stealth_at_level(sneaker, sneaker.level)
    end

    def die?(monster) do
      max_hp = Mobile.max_hp_at_level(monster, monster.level)

      missing_fatal_limb =
        monster.limbs
        |> Map.values()
        |> Enum.any?(&(&1.fatal && &1.health <= 0))

      max_hp <= 0 or monster.hp <= 0 or missing_fatal_limb
    end

    def die(monster, room) do
      pets_and_players =
        room.mobiles
        |> Enum.reduce([], fn
          {_ref, %Character{} = character}, pets_and_players ->
            AI.pets_and_party(room, character) ++ pets_and_players

          _mobile, pets_and_players ->
            pets_and_players
        end)
        |> Enum.uniq()
        |> length()

      message =
        monster.death_message
        |> Text.interpolate(%{"name" => monster.name})
        |> Text.capitalize_first()

      room =
        Enum.reduce(room.mobiles, room, fn
          {ref, %Character{} = character}, updated_room ->
            updated_room =
              Room.update_mobile(updated_room, ref, fn _updated_room, character ->
                Mobile.send_scroll(character, "<p>#{message}</p>")

                exp = max(1, div(monster.experience, pets_and_players))

                character
                |> Character.add_experience_to_buffer(exp)
                |> Character.add_currency_from_monster(monster)
                |> KillCount.increment(monster, exp)
              end)

            updated_room =
              Monster.drop_loot_for_character(
                updated_room,
                monster,
                room.mobiles[character.ref]
              )

            Room.update_moblist(updated_room)
            updated_room

          _, updated_room ->
            updated_room
        end)

      if monster.regen_time_in_hours do
        spawn_at =
          DateTime.utc_now()
          |> DateTime.to_unix()
          |> Kernel.+(monster.regen_time_in_hours * 60 * 60)

        %Monster{id: monster.id}
        |> Ecto.Changeset.change(next_spawn_at: spawn_at)
        |> Repo.update!()
      end

      if monster.game_limit && monster.regen_time_in_hours do
        message = "<p>[<span class='yellow'>announce</span> : Apotheosis] #{message}</p>"

        Repo.insert!(%ChannelHistory{
          character_name: "Apotheosis",
          channel_name: "announce",
          message: message
        })

        ApathyDriveWeb.Endpoint.broadcast!("chat:gossip", "chat-sidebar", %{
          html: message,
          chat_tab: "announce"
        })
      end

      room =
        if monster.death_ability_id do
          ability =
            monster.death_ability_id
            |> Ability.find()
            |> Map.put(:ignores_round_cooldown?, true)
            |> Map.put(:energy, 0)

          Ability.execute(room, monster.ref, ability, [monster.ref])
        else
          room
        end

      ApathyDrive.Repo.delete!(%RoomMonster{id: monster.room_monster_id})

      room = put_in(room.mobiles, Map.delete(room.mobiles, monster.ref))

      Room.update_moblist(room)
      room
    end

    def dodge_at_level(monster, level, _room) do
      agi = attribute_at_level(monster, :agility, level)
      cha = attribute_at_level(monster, :charm, level)
      base = agi + cha / 10
      trunc(base + ability_value(monster, "Dodge"))
    end

    def enough_mana_for_ability?(monster, %Ability{mana: cost}) do
      mana = Mobile.max_mana_at_level(monster, monster.level)

      monster.mana >= cost / mana
    end

    def evil_points(monster, %Character{} = attacker) do
      cond do
        Ability.retaliate?(monster, attacker) ->
          # attacker has already received evil points for attacking monster
          0

        Ability.retaliate?(attacker, monster) ->
          # monster has attacked the character, it's not evil to fight back
          0

        monster.alignment == "good" ->
          40

        :else ->
          0
      end
    end

    # only characters can receive evil points
    def evil_points(_monster, %{} = _attacker), do: 0

    def enter_message(%Monster{name: name}) do
      "<p><span class='yellow'>#{name}</span><span class='dark-green'> walks in from {{direction}}.</span></p>"
    end

    def exit_message(%Monster{name: name}) do
      "<p><span class='yellow'>#{name}</span><span class='dark-green'> walks off {{direction}}.</span></p>"
    end

    def has_ability?(%Monster{} = monster, ability_name) do
      monster.effects
      |> Map.values()
      |> Enum.map(&Map.keys/1)
      |> List.flatten()
      |> Enum.member?(ability_name)
    end

    def exhausted(_monster, _req \\ nil), do: false

    def held(%{effects: effects} = mobile) do
      effects
      |> Map.values()
      |> Enum.find(fn effect ->
        Map.has_key?(effect, "Root")
      end)
      |> held(mobile)
    end

    def held(nil, %{}), do: false

    def held(%{"StatusMessage" => message}, %{} = mobile) do
      send_scroll(mobile, "<p>#{message}</p>")
      true
    end

    def hp_description(%Monster{hp: hp}) when hp >= 1.0, do: "unwounded"
    def hp_description(%Monster{hp: hp}) when hp >= 0.9, do: "slightly wounded"
    def hp_description(%Monster{hp: hp}) when hp >= 0.6, do: "moderately wounded"
    def hp_description(%Monster{hp: hp}) when hp >= 0.4, do: "heavily wounded"
    def hp_description(%Monster{hp: hp}) when hp >= 0.2, do: "severely wounded"
    def hp_description(%Monster{hp: hp}) when hp >= 0.1, do: "critically wounded"
    def hp_description(%Monster{hp: _hp}), do: "very critically wounded"

    def magical_resistance_at_level(monster, level) do
      willpower = attribute_at_level(monster, :willpower, level)

      mr_percent = ability_value(monster, "MR%")

      mr_from_percent = Ability.ac_for_mitigation_at_level(mr_percent)

      mr = ability_value(monster, "MR")

      max(willpower - 50 + mr + mr_from_percent, 0)
    end

    def max_hp_at_level(%Monster{} = monster, level) do
      health = attribute_at_level(monster, :health, level)

      base = monster.base_hp
      bonus = (health - 50) * level / 16

      max_hp_percent = ability_value(monster, "MaxHP%")

      modifier = if max_hp_percent > 0, do: max_hp_percent, else: 1.0

      trunc((base + bonus + ability_value(monster, "MaxHP")) * modifier)
    end

    def max_mana_at_level(monster, level) do
      mana_per_level = ability_value(monster, "ManaPerLevel")

      mana_per_level = if mana_per_level == 0, do: 4, else: 0

      bonus = ability_value(monster, "MaxMana")

      mana_per_level * (level - 1) + 6 + bonus
    end

    def party_refs(monster, room) do
      Party.refs(room, monster)
    end

    def perception_at_level(monster, level, _room) do
      int = attribute_at_level(monster, :intellect, level)
      cha = attribute_at_level(monster, :charm, level)
      int = int + cha / 10
      modifier = ability_value(monster, "Perception")
      trunc(int * (1 + modifier / 100))
    end

    def physical_resistance_at_level(monster, level) do
      strength = attribute_at_level(monster, :strength, level)

      ac_percent = ability_value(monster, "AC%")

      ac_from_percent = Ability.ac_for_mitigation_at_level(ac_percent)

      ac = ability_value(monster, "AC")

      max(strength - 50 + ac + ac_from_percent, 0)
    end

    def power_at_level(%Monster{} = monster, level) do
      [:strength, :agility, :intellect, :willpower, :health, :charm]
      |> Enum.reduce(0, &(&2 + Mobile.attribute_at_level(monster, &1, level)))
    end

    def heartbeat(%Monster{} = monster, %Room{} = room) do
      room =
        room
        |> Room.update_mobile(monster.ref, fn room, monster ->
          cond do
            monster.delete_at && :lt == DateTime.compare(monster.delete_at, DateTime.utc_now()) ->
              Room.send_scroll(
                room,
                "<p><span class='dark-yellow'>#{monster.name} winks out of existence.</span></p>"
              )

              ApathyDrive.Repo.delete!(%RoomMonster{id: monster.room_monster_id})

              room = put_in(room.mobiles, Map.delete(room.mobiles, monster.ref))

              Room.update_moblist(room)
              room

            Mobile.die?(monster) ->
              Mobile.die(monster, room)

            :else ->
              monster
          end
        end)
        |> Ability.unbalance(monster.ref)
        |> Room.update_mobile(monster.ref, fn room, monster ->
          if monster.bust_cache, do: Trait.bust_cache(monster)

          monster
          |> Map.put(:bust_cache, false)
          |> Regeneration.regenerate(room)
          |> TimerManager.send_after(
            {:heartbeat, ApathyDrive.Regeneration.tick_time(monster), {:heartbeat, monster.ref}}
          )
          |> RoomServer.execute_casting_ability(room)
        end)

      if :rand.uniform(100) > 75 do
        ApathyDrive.Aggression.react(room, monster.ref)
      else
        room
      end
      |> AI.think(monster.ref)
    end

    def hp_regen_per_30(%Monster{} = monster) do
      monster.hp_regen * (1 + ability_value(monster, "HPRegen") / 100)
    end

    def mana_regen_per_30(%Monster{} = monster) do
      max_mana = max_mana_at_level(monster, monster.level)

      base_mana_regen = max_mana * 0.2

      modified_mana_regen = base_mana_regen * (1 + ability_value(monster, "ManaRegen") / 100)

      if max_mana > 0 do
        modified_mana_regen
      else
        0
      end
    end

    def send_scroll(%Monster{} = monster, _html) do
      monster
    end

    def set_room_id(%Monster{} = monster, room_id) do
      %RoomMonster{id: monster.room_monster_id}
      |> Ecto.Changeset.change(%{room_id: room_id})
      |> Repo.update()

      monster
      |> Map.put(:last_room_id, monster.room_id)
      |> Map.put(:room_id, room_id)
    end

    def shift_hp(monster, percentage) do
      update_in(monster.hp, &max(0.0, min(1.0, &1 + percentage)))
    end

    def silenced(%Monster{effects: effects} = monster, %Room{} = room) do
      effects
      |> Map.values()
      |> Enum.find(fn effect ->
        Map.has_key?(effect, "Silence")
      end)
      |> silenced(monster, room)
    end

    def silenced(nil, %Monster{}, %Room{}), do: false

    def silenced(%{}, %Monster{} = monster, %Room{}) do
      Mobile.send_scroll(monster, "<p><span class='cyan'>You are silenced!</span></p>")
      true
    end

    def spellcasting_at_level(monster, level, _ability) do
      attribute_value = Room.average([monster.intellect, monster.willpower])

      sc = attribute_value + level * 2

      trunc(sc + ability_value(monster, "Spellcasting"))
    end

    def stealth_at_level(monster, level) do
      agi = attribute_at_level(monster, :agility, level)
      cha = attribute_at_level(monster, :charm, level)
      agi = agi + cha / 10
      modifier = ability_value(monster, "Stealth")
      trunc(agi * (modifier / 100))
    end

    def subtract_mana(monster, %{mana: cost} = _ability) do
      percentage = cost / Mobile.max_mana_at_level(monster, monster.level)
      update_in(monster.mana, &max(0, &1 - percentage))
    end

    def subtract_energy(monster, ability) do
      update_in(monster.energy, &(&1 - ability.energy))
    end

    def tracking_at_level(monster, level, room) do
      perception = perception_at_level(monster, level, room)
      modifier = ability_value(monster, "Tracking")
      perception * (modifier / 100)
    end

    def update_prompt(%Monster{} = monster, room) do
      Room.update_hp_bar(room, monster.ref)
      Room.update_mana_bar(room, monster.ref)
      monster
    end
  end
end
