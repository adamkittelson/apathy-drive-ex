defmodule ApathyDrive.Companion do
  alias ApathyDrive.{
    Ability,
    AI,
    Character,
    Companion,
    Regeneration,
    Mobile,
    Monster,
    MonsterAbility,
    MonsterTrait,
    Party,
    Repo,
    Room,
    RoomMonster,
    RoomServer,
    Stealth,
    Text,
    TimerManager
  }

  require Ecto.Query

  defstruct [
    :gender,
    :description,
    :enter_message,
    :exit_message,
    :death_message,
    :hp,
    :mana,
    :timers,
    :effects,
    :last_effect_key,
    :abilities,
    :strength,
    :agility,
    :intellect,
    :willpower,
    :health,
    :charm,
    :name,
    :room_id,
    :level,
    :monster_id,
    :character_id,
    :leader,
    :attack_target,
    :ability_shift,
    :ability_special,
    :energy,
    :max_energy,
    :casting,
    :hp_regen,
    :base_hp,
    :last_tick_at,
    :auto_heal,
    :auto_bless,
    :auto_curse,
    :auto_nuke,
    :auto_roam,
    :auto_flee,
    :alignment
  ]

  def dismiss(nil, %Room{} = room), do: room

  def dismiss(%Companion{} = companion, %Room{} = room) do
    character = Companion.character(companion, room)

    Mobile.send_scroll(
      character,
      "<p>You release #{Mobile.colored_name(companion)} from your service, and they wander off into the sunset.</p>"
    )

    room.mobiles
    |> Map.values()
    |> Enum.reject(&(&1.ref == character.ref))
    |> Enum.each(fn
      %Character{} = observer ->
        Mobile.send_scroll(
          observer,
          "<p>#{Mobile.colored_name(character)} releases #{Mobile.colored_name(companion)} from {{target:his/her/their}} service.</p>"
        )

      _ ->
        :noop
    end)

    %RoomMonster{id: companion.room_monster_id}
    |> Repo.delete!()

    room = update_in(room.mobiles, &Map.delete(&1, companion.ref))
    Room.update_moblist(room)
    room
  end

  def character_enemies(nil, _room), do: []

  def character_enemies(%Character{} = character, room) do
    character.effects
    |> Map.values()
    |> Enum.filter(&Map.has_key?(&1, "Aggro"))
    |> Enum.map(&Map.get(&1, "Aggro"))
    |> Enum.filter(&(&1 in Map.keys(room.mobiles)))
  end

  def enemies(%Companion{} = companion, room) do
    character_enemies =
      companion
      |> character(room)
      |> character_enemies(room)

    companion_enemies =
      companion.effects
      |> Map.values()
      |> Enum.filter(&Map.has_key?(&1, "Aggro"))
      |> Enum.map(&Map.get(&1, "Aggro"))
      |> Enum.filter(&(&1 in Map.keys(room.mobiles)))

    (companion_enemies ++ character_enemies)
    |> Enum.reject(&Stealth.invisible?(room.mobiles[&1], companion, room))
  end

  def character(%Companion{character_id: id}, %Room{} = room) do
    room.mobiles
    |> Map.values()
    |> Enum.find(&(&1.__struct__ == Character and &1.id == id))
  end

  def conversion_power(%Character{} = character) do
    base_power = Mobile.power_at_level(character, 1)

    level =
      character.equipment
      |> Enum.map(& &1.level)
      |> List.insert_at(0, character.level)
      |> Enum.sort()
      |> List.first()

    power_at_level = trunc(base_power + base_power / 10 * (level - 1))
    {base_power, level, power_at_level}
  end

  def hire_price(%Character{} = _character) do
    # {_base_power, _level, power_at_level} = conversion_power(character)

    # div(power_at_level, 10)
    0
  end

  def convert_for_character(%Room{} = room, %Monster{} = monster, %Character{} = character) do
    room_monster =
      RoomMonster
      |> Repo.get!(monster.room_monster_id)

    changes = %{
      level: character.level,
      character_id: character.id
    }

    room_monster
    |> Ecto.Changeset.change(changes)
    |> Repo.update!()

    Mobile.send_scroll(character, "<p>#{monster.name} started to follow you</p>")

    room =
      load_for_character(room, character)
      |> update_in([:mobiles], &Map.delete(&1, monster.ref))

    Room.update_moblist(room)
    room
  end

  def load_for_character(%Room{} = room, %Character{id: id} = character) do
    rm =
      RoomMonster
      |> Ecto.Query.where(character_id: ^id)
      |> Repo.one()

    if rm do
      companion =
        rm
        |> from_room_monster()
        |> Map.put(:leader, character.ref)
        |> Map.put(:alignment, character.alignment)
        |> Map.put(:character_id, id)

      mobiles =
        room.mobiles
        |> Enum.reject(fn {_ref, mobile} ->
          Map.get(mobile, :character_id) == id
        end)
        |> Enum.into(%{})
        |> Map.put(companion.ref, companion)

      put_in(room.mobiles, mobiles)
    else
      room
    end
  end

  def load_traits(%Companion{monster_id: id} = companion) do
    effect =
      MonsterTrait.load_traits(id)
      |> Map.put("stack_key", "monster")

    companion
    |> Systems.Effect.add(effect)
  end

  def load_abilities(%Companion{} = companion) do
    MonsterAbility.load_abilities(companion)
  end

  def from_room_monster(%RoomMonster{id: id, monster_id: monster_id} = rm) do
    monster =
      Monster
      |> Repo.get(monster_id)
      |> Map.take([
        :base_hp,
        :hp_regen,
        :energy,
        :max_energy,
        :gender,
        :description,
        :enter_message,
        :exit_message,
        :death_message,
        :hp,
        :mana,
        :timers,
        :effects,
        :last_effect_key,
        :abilities,
        :last_tick_at,
        :auto_heal,
        :auto_bless,
        :auto_curse,
        :auto_nuke,
        :auto_roam,
        :auto_flee
      ])

    room_monster =
      Map.take(rm, [
        :strength,
        :agility,
        :intellect,
        :willpower,
        :health,
        :charm,
        :name,
        :room_id,
        :level
      ])

    ref = :crypto.hash(:md5, inspect(make_ref())) |> Base.encode16()

    %Companion{}
    |> Map.merge(monster)
    |> Map.merge(room_monster)
    |> Map.put(:room_monster_id, id)
    |> Map.put(:monster_id, monster_id)
    |> Map.put(:ref, ref)
    |> Map.put(:level, rm.level)
    |> load_abilities()
    |> load_traits()
    |> Mobile.cpr()
  end

  defimpl ApathyDrive.Mobile, for: Companion do
    def ability_value(companion, ability) do
      Systems.Effect.effect_bonus(companion, ability)
    end

    def accuracy_at_level(companion, level, room) do
      agi = attribute_at_level(companion, :agility, level)
      cha = Party.charm_at_level(room, companion, level)
      agi = agi + cha / 10
      modifier = ability_value(companion, "Accuracy")
      trunc(agi * (1 + modifier / 100))
    end

    def attribute_at_level(%Companion{} = companion, attribute, level) do
      Map.get(companion, attribute) + level - 1
    end

    def attack_ability(companion) do
      companion.abilities
      |> Map.values()
      |> Enum.filter(&(&1.kind == "auto attack"))
      |> Enum.random()
      |> Map.put(:kind, "attack")
      |> Map.put(:ignores_round_cooldown?, true)
    end

    def auto_attack_target(%Companion{} = companion, room) do
      character = Companion.character(companion, room)

      character_target =
        if character do
          Mobile.auto_attack_target(character, room)
        end

      companion_target =
        case Companion.enemies(companion, room) do
          [] ->
            nil

          enemies ->
            Enum.random(enemies)
        end

      character_target || companion_target
    end

    def caster_level(%Companion{level: caster_level}, %{} = _target), do: caster_level

    def color(%Companion{alignment: "evil"}), do: "magenta"
    def color(%Companion{alignment: "neutral"}), do: "dark-cyan"
    def color(%Companion{alignment: "good"}), do: "grey"

    def colored_name(%Companion{name: name} = companion) do
      "<span style='color: #{color(companion)};'>#{name}</span>"
    end

    def confused(%Companion{effects: effects} = companion, %Room{} = room) do
      effects
      |> Map.values()
      |> Enum.find(fn effect ->
        Map.has_key?(effect, "Confusion") && effect["Confusion"] >= :rand.uniform(100)
      end)
      |> confused(companion, room)
    end

    def confused(nil, %Companion{}, %Room{}), do: false

    def confused(
          %{"ConfusionMessage" => message} = effect,
          %Companion{} = companion,
          %Room{} = room
        ) do
      Mobile.send_scroll(companion, "<p>#{message}</p>")

      if effect["ConfusionSpectatorMessage"],
        do:
          Room.send_scroll(
            room,
            "<p>#{Text.interpolate(effect["ConfusionSpectatorMessage"], %{"user" => companion})}</p>",
            [companion]
          )

      true
    end

    def confused(%{}, %Companion{} = companion, %Room{} = room) do
      send_scroll(companion, "<p><span class='cyan'>You fumble in confusion!</span></p>")

      Room.send_scroll(
        room,
        "<p><span class='cyan'>#{
          Text.interpolate("{{user}} fumbles in confusion!</span></p>", %{"user" => companion})
        }</span></p>",
        [companion]
      )

      true
    end

    def cpr(%Companion{} = companion) do
      time =
        min(
          Mobile.round_length_in_ms(companion),
          TimerManager.time_remaining(companion, :heartbeat)
        )

      TimerManager.send_after(companion, {:heartbeat, time, {:heartbeat, companion.ref}})
    end

    def crits_at_level(companion, level, room) do
      int = attribute_at_level(companion, :intellect, level)
      cha = Party.charm_at_level(room, companion, level)
      int = int + cha / 10
      modifier = ability_value(companion, "Crits")
      trunc(int * (1 + modifier / 100))
    end

    def description(companion, _observer) do
      companion.description
    end

    def die(companion, room) do
      message =
        companion.death_message
        |> Text.interpolate(%{"name" => companion.name})
        |> Text.capitalize_first()

      Room.send_scroll(room, "<p>#{message}</p>")

      ApathyDrive.Repo.delete!(%RoomMonster{id: companion.room_monster_id})

      room = put_in(room.mobiles, Map.delete(room.mobiles, companion.ref))
      Room.update_moblist(room)
      room
    end

    def dodge_at_level(companion, level, room) do
      agi = attribute_at_level(companion, :agility, level)
      cha = Party.charm_at_level(room, companion, level)
      agi = agi + cha / 10
      modifier = ability_value(companion, "Dodge")
      trunc(agi * (1 + modifier / 100))
    end

    def block_at_level(companion, _level) do
      trunc(Mobile.ability_value(companion, "Block"))
    end

    def parry_at_level(companion, _level) do
      Mobile.ability_value(companion, "Parry")
    end

    def enough_mana_for_ability?(companion, %Ability{mana: cost}) do
      mana = Mobile.max_mana_at_level(companion, companion.level)

      companion.mana >= cost / mana
    end

    def enter_message(%Companion{enter_message: enter_message}) do
      enter_message
    end

    def exit_message(%Companion{exit_message: exit_message}) do
      exit_message
    end

    def has_ability?(%Companion{} = companion, ability_name) do
      companion.effects
      |> Map.values()
      |> Enum.map(&Map.keys/1)
      |> List.flatten()
      |> Enum.member?(ability_name)
    end

    def heartbeat(%Companion{} = companion, %Room{} = room) do
      Room.update_mobile(room, companion.ref, fn companion ->
        companion
        |> Regeneration.regenerate(room)
        |> RoomServer.execute_casting_ability(room)
      end)
      |> AI.think(companion.ref)
    end

    def held(%{effects: effects} = mobile) do
      effects
      |> Map.values()
      |> Enum.find(fn effect ->
        Map.has_key?(effect, "held")
      end)
      |> held(mobile)
    end

    def held(nil, %{}), do: false

    def held(%{"effect_message" => message}, %{} = mobile) do
      send_scroll(mobile, "<p>#{message}</p>")
      true
    end

    def hp_description(%Companion{hp: hp}) when hp >= 1.0, do: "unwounded"
    def hp_description(%Companion{hp: hp}) when hp >= 0.9, do: "slightly wounded"
    def hp_description(%Companion{hp: hp}) when hp >= 0.6, do: "moderately wounded"
    def hp_description(%Companion{hp: hp}) when hp >= 0.4, do: "heavily wounded"
    def hp_description(%Companion{hp: hp}) when hp >= 0.2, do: "severely wounded"
    def hp_description(%Companion{hp: hp}) when hp >= 0.1, do: "critically wounded"
    def hp_description(%Companion{hp: _hp}), do: "very critically wounded"

    def magical_damage_at_level(companion, level) do
      damage = attribute_at_level(companion, :intellect, level)

      modifier =
        ability_value(companion, "ModifyDamage") + ability_value(companion, "ModifyMagicalDamage")

      damage * (1 + modifier / 100)
    end

    def magical_resistance_at_level(companion, level) do
      willpower = attribute_at_level(companion, :willpower, level)

      willpower + ability_value(companion, "MagicalResist")
    end

    def max_hp_at_level(%Companion{} = companion, level) do
      health = attribute_at_level(companion, :health, level)

      base = companion.base_hp
      bonus = (health - 50) * level / 16

      modifier = ability_value(companion, "MaxHP")
      trunc((base + bonus) * (1 + modifier / 100))
    end

    def max_mana_at_level(mobile, level) do
      mana_per_level = ability_value(mobile, "ManaPerLevel")

      mana_per_level * (level - 1) + 6
    end

    def party_refs(companion, room) do
      Party.refs(room, companion)
    end

    def perception_at_level(companion, level, room) do
      int = attribute_at_level(companion, :intellect, level)
      cha = Party.charm_at_level(room, companion, level)
      int = int + cha / 10
      modifier = ability_value(companion, "Perception")
      trunc(int * (1 + modifier / 100))
    end

    def physical_damage_at_level(companion, level) do
      damage = attribute_at_level(companion, :strength, level)

      modifier =
        ability_value(companion, "ModifyDamage") +
          ability_value(companion, "ModifyPhysicalDamage")

      damage * (1 + modifier / 100)
    end

    def physical_resistance_at_level(companion, level) do
      str = attribute_at_level(companion, :strength, level)
      ac = ability_value(companion, "AC")

      str + ac
    end

    def power_at_level(%Companion{} = companion, level) do
      [:strength, :agility, :intellect, :willpower, :health, :charm]
      |> Enum.reduce(0, &(&2 + Mobile.attribute_at_level(companion, &1, level)))
    end

    def hp_regen_per_round(%Companion{} = companion) do
      round_length = Mobile.round_length_in_ms(companion)

      base_hp_regen =
        (companion.hp_regen +
           (companion.level + 30) * attribute_at_level(companion, :health, companion.level) /
             500.0) * round_length / 30_000

      modified_hp_regen = base_hp_regen * (1 + ability_value(companion, "HPRegen") / 100)

      max_hp = max_hp_at_level(companion, companion.level)

      modified_hp_regen / max_hp
    end

    def mana_regen_per_round(%Companion{} = companion) do
      round_length = Mobile.round_length_in_ms(companion)

      max_mana = max_mana_at_level(companion, companion.level)

      base_mana_regen =
        (companion.level + 20) * attribute_at_level(companion, :willpower, companion.level) *
          (div(ability_value(companion, "ManaPerLevel"), 2) + 2) / 1650.0 * round_length / 30_000

      modified_mana_regen = base_mana_regen * (1 + ability_value(companion, "ManaRegen") / 100)

      if max_mana > 0 do
        modified_mana_regen / max_mana
      else
        0
      end
    end

    def round_length_in_ms(companion) do
      speed = ability_value(companion, "Speed")

      modifier =
        if speed == 0 do
          1
        else
          speed / 100
        end

      trunc(modifier * Application.get_env(:apathy_drive, :round_length_in_ms))
    end

    def send_scroll(%Companion{} = companion, _html) do
      companion
    end

    def set_room_id(%Companion{} = companion, room_id) do
      %RoomMonster{id: companion.room_monster_id}
      |> Ecto.Changeset.change(%{room_id: room_id})
      |> Repo.update!()

      companion
      |> Map.put(:room_id, room_id)
    end

    def shift_hp(companion, percentage, room \\ nil) do
      hp_description = hp_description(companion)
      companion = update_in(companion.hp, &min(1.0, &1 + percentage))
      updated_hp_description = hp_description(companion)

      if room && (companion.hp > 0 and hp_description != updated_hp_description) do
        room.mobiles
        |> Map.values()
        |> List.delete(companion)
        |> Enum.each(fn
          %Character{} = observer ->
            Mobile.send_scroll(
              observer,
              "<p>#{Mobile.colored_name(companion)} is #{updated_hp_description}.</p>"
            )

          _ ->
            :noop
        end)
      end

      companion
    end

    def silenced(%Companion{effects: effects} = companion, %Room{} = room) do
      effects
      |> Map.values()
      |> Enum.find(fn effect ->
        Map.has_key?(effect, "Silence")
      end)
      |> silenced(companion, room)
    end

    def silenced(nil, %Companion{}, %Room{}), do: false

    def silenced(%{}, %Companion{} = companion, %Room{}) do
      Mobile.send_scroll(companion, "<p><span class='cyan'>You are silenced!</span></p>")
      true
    end

    def spellcasting_at_level(companion, level, ability) do
      sc =
        ability.attributes
        |> Map.keys()
        |> Enum.map(&Mobile.attribute_at_level(companion, &1, level))
        |> Enum.sum()
        |> div(map_size(ability.attributes))

      sc + ability_value(companion, "Spellcasting")
    end

    def stealth_at_level(companion, level) do
      if Mobile.has_ability?(companion, "Revealed") do
        0
      else
        agi = attribute_at_level(companion, :agility, level)
        agi = agi + attribute_at_level(companion, :charm, level) / 10
        modifier = ability_value(companion, "Stealth")
        trunc(agi * (modifier / 100))
      end
    end

    def subtract_mana(companion, %{mana: cost}) do
      percentage = cost / Mobile.max_mana_at_level(companion, companion.level)
      update_in(companion.mana, &max(0, &1 - percentage))
    end

    def subtract_energy(companion, ability) do
      initial_energy = companion.energy
      companion = update_in(companion.energy, &max(0, &1 - ability.energy))

      if initial_energy == companion.max_energy do
        Regeneration.regenerate(companion)
      else
        companion
      end
    end

    def target_level(%Companion{level: _caster_level}, %Character{level: target_level}),
      do: target_level

    def target_level(%Companion{level: _caster_level}, %Companion{level: target_level}),
      do: target_level

    def target_level(%Companion{level: caster_level}, %Monster{level: target_level}),
      do: max(caster_level, target_level)

    def tracking_at_level(companion, level, room) do
      perception = perception_at_level(companion, level, room)
      modifier = ability_value(companion, "Tracking")
      perception * (modifier / 100)
    end

    def update_prompt(%Companion{} = companion) do
      companion
    end
  end
end
