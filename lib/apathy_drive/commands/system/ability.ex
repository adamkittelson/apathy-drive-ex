defmodule ApathyDrive.Commands.System.Ability do
  alias ApathyDrive.{
    Ability,
    AbilityAttribute,
    AbilityDamageType,
    AbilityTrait,
    Attribute,
    Character,
    Class,
    ClassAbility,
    DamageType,
    Mobile,
    Repo,
    Room,
    Trait
  }

  alias ApathyDrive.Commands.Help

  def execute(%Room{} = room, character, ["create" | ability_name]) do
    ability_name = Enum.join(ability_name, " ")

    create(room, character, ability_name)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, [
        "add",
        "trait" | trait
      ]) do
    add_trait(room, character, trait)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, [
        "remove",
        "trait" | trait
      ]) do
    remove_trait(room, character, trait)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, [
        "add",
        "class",
        class,
        level,
        auto_learn
      ]) do
    add_class(room, character, class, level, auto_learn)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, [
        "remove",
        "class" | trait
      ]) do
    remove_class(room, character, trait)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, [
        "add",
        "attribute" | attribute
      ]) do
    add_attribute(room, character, attribute)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, [
        "add",
        "damage" | damage
      ]) do
    add_damage(room, character, damage)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, [
        "remove",
        "damage",
        damage
      ]) do
    remove_damage(room, character, damage)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, [
        "set",
        "description" | description
      ]) do
    set_description(room, character, description)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, [
        "set",
        "name" | description
      ]) do
    set_name(room, character, description)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, [
        "set",
        "energy" | description
      ]) do
    set_energy(room, character, description)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, [
        "set",
        "difficulty" | description
      ]) do
    set_difficulty(room, character, description)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, [
        "set",
        "duration" | duration
      ]) do
    set_duration(room, character, duration)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, ["set", "mana" | mana]) do
    set_mana(room, character, mana)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, [
        "set",
        "user_message" | message
      ]) do
    set_user_message(room, character, message)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, [
        "set",
        "target_message" | message
      ]) do
    set_target_message(room, character, message)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, [
        "set",
        "spectator_message" | message
      ]) do
    set_spectator_message(room, character, message)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, [
        "set",
        "command" | command
      ]) do
    set_command(room, character, command)

    room
  end

  def execute(%Room{} = room, character, ["set", "targets" | targets]) do
    set_targets(room, character, targets)

    room
  end

  def execute(%Room{} = room, character, ["set", "kind" | kind]) do
    set_kind(room, character, kind)

    room
  end

  def execute(%Room{} = room, character, ["set", "cast_time" | time_in_seconds]) do
    set_cast_time(room, character, time_in_seconds)

    room
  end

  def execute(%Room{} = room, character, ["help" | ability_name]) do
    ability_name = Enum.join(ability_name, " ")

    Help.execute(room, character, [ability_name])

    room
  end

  def execute(%Room{} = room, character, _args) do
    Mobile.send_scroll(character, "<p>Invalid system command.</p>")

    room
  end

  defp create(room, character, ability_name) do
    Repo.insert!(%Ability{name: ability_name}, on_conflict: :nothing)

    Help.execute(room, character, [ability_name])
  end

  defp add_class(room, character, class, level, auto_learn) do
    level =
      level
      |> Integer.parse()
      |> case do
        {level, ""} ->
          level

        other ->
          other
      end

    auto_learn =
      case auto_learn do
        "true" ->
          true

        "false" ->
          false
      end

    ability = character.editing

    class = Class.match_by_name(class)

    cond do
      is_nil(class) ->
        Mobile.send_scroll(character, "<p>No class by that name was found.</p>")

      is_integer(level) ->
        on_conflict = [set: [level: level, auto_learn: auto_learn]]

        %ClassAbility{
          ability_id: ability.id,
          class_id: class.id,
          level: level,
          auto_learn: auto_learn
        }
        |> Repo.insert(on_conflict: on_conflict, conflict_target: [:ability_id, :class_id])

        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        Help.execute(room, character, [ability.name])

      :else ->
        Mobile.send_scroll(character, "<p>Must provide a valid level.</p>")
    end
  end

  defp remove_class(room, character, class) do
    class = Enum.join(class, " ")

    ability = character.editing

    if class = Class.match_by_name(class) do
      if ca = Repo.get_by(ClassAbility, class_id: class.id, ability_id: ability.id) do
        Repo.delete!(ca)
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        Help.execute(room, character, [ability.name])
      else
        Mobile.send_scroll(character, "<p>#{class.name} does not have #{ability.name}.</p>")
      end
    else
      Mobile.send_scroll(character, "<p>No class by that name was found.</p>")
    end
  end

  defp add_trait(room, character, [trait | value]) do
    {:ok, value} =
      value
      |> Enum.join(" ")
      |> ApathyDrive.JSONB.load()

    ability = character.editing

    trait = Repo.get_by(Trait, name: trait)

    cond do
      is_nil(trait) ->
        Mobile.send_scroll(character, "<p>No trait by that name was found.</p>")

      value == :error ->
        Mobile.send_scroll(character, "<p>Value for #{trait.name} is invalid.</p>")

      :else ->
        on_conflict = [set: [value: value]]

        %AbilityTrait{ability_id: ability.id, trait_id: trait.id, value: value}
        |> Repo.insert(on_conflict: on_conflict, conflict_target: [:ability_id, :trait_id])

        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        Help.execute(room, character, [ability.name])
    end
  end

  defp remove_trait(room, character, trait) do
    trait = Enum.join(trait, " ")

    ability = character.editing

    if trait = Repo.get_by(Trait, name: trait) do
      if ca = Repo.get_by(AbilityTrait, trait_id: trait.id, ability_id: ability.id) do
        Repo.delete!(ca)
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        Help.execute(room, character, [ability.name])
      else
        Mobile.send_scroll(character, "<p>#{ability.name} does not have #{trait.name}.</p>")
      end
    else
      Mobile.send_scroll(character, "<p>No trait by that name was found.</p>")
    end
  end

  defp add_attribute(room, character, [attribute]) do
    ability = character.editing

    attribute = Repo.get_by(Attribute, name: attribute)

    cond do
      is_nil(attribute) ->
        Mobile.send_scroll(character, "<p>No attribute by that name was found.</p>")

      :else ->
        %AbilityAttribute{ability_id: ability.id, attribute_id: attribute.id}
        |> Repo.insert()

        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        Help.execute(room, character, [ability.name])
    end
  end

  defp add_damage(room, character, damage) do
    damage
    |> Enum.join(" ")
    |> Poison.decode()
    |> case do
      {:ok, %{"type" => type, "min" => min, "max" => max}} ->
        ability = character.editing

        type = Repo.get_by(DamageType, name: type)

        cond do
          is_nil(type) ->
            Mobile.send_scroll(character, "<p>No damage type by that name was found.</p>")

          :else ->
            %AbilityDamageType{
              ability_id: ability.id,
              damage_type_id: type.id,
              min: min,
              max: max
            }
            |> Repo.insert()

            ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
            Help.execute(room, character, [ability.name])
        end
    end
  end

  defp remove_damage(room, character, damage) do
    ability = character.editing

    damage = String.capitalize(damage)

    type = Repo.get_by(DamageType, name: damage)

    if type do
      abt = Repo.get_by(AbilityDamageType, ability_id: ability.id, damage_type_id: type.id)

      if abt do
        Repo.delete!(abt)
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        Help.execute(room, character, [ability.name])
      else
        Mobile.send_scroll(character, "<p>No damage type by that name was found.</p>")
      end
    else
      Mobile.send_scroll(character, "<p>No damage type by that name was found.</p>")
    end
  end

  defp set_description(room, character, description) do
    description = Enum.join(description, " ")
    ability = character.editing

    ability
    |> Ability.set_description_changeset(description)
    |> Repo.update()
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        Help.execute(room, character, [ability.name])

      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
    end
  end

  defp set_name(room, character, description) do
    description = Enum.join(description, " ")
    ability = character.editing

    ability
    |> Ability.set_name_changeset(description)
    |> Repo.update()
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        Help.execute(room, character, [ability.name])

      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
    end
  end

  defp set_duration(room, character, duration) do
    duration = Enum.join(duration, " ")
    ability = character.editing

    ability
    |> Ability.set_duration_changeset(duration)
    |> Repo.update()
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        Help.execute(room, character, [ability.name])

      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
    end
  end

  defp set_energy(room, character, energy) do
    energy = Enum.join(energy, " ")
    ability = character.editing

    ability
    |> Ability.set_energy_changeset(energy)
    |> Repo.update()
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        Help.execute(room, character, [ability.name])

      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
    end
  end

  defp set_difficulty(room, character, difficulty) do
    difficulty = Enum.join(difficulty, " ")
    ability = character.editing

    ability
    |> Ability.set_difficulty_changeset(difficulty)
    |> Repo.update()
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        Help.execute(room, character, [ability.name])

      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
    end
  end

  defp set_mana(room, character, mana) do
    mana = Enum.join(mana, " ")
    ability = character.editing

    ability
    |> Ability.set_mana_changeset(mana)
    |> Repo.update()
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        Help.execute(room, character, [ability.name])

      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
    end
  end

  defp set_user_message(room, character, message) do
    message = Enum.join(message, " ")
    ability = character.editing

    ability
    |> Ability.set_user_message_changeset(message)
    |> Repo.update()
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        Help.execute(room, character, [ability.name])

      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
    end
  end

  defp set_target_message(room, character, message) do
    message = Enum.join(message, " ")
    ability = character.editing

    ability
    |> Ability.set_target_message_changeset(message)
    |> Repo.update()
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        Help.execute(room, character, [ability.name])

      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
    end
  end

  defp set_spectator_message(room, character, message) do
    message = Enum.join(message, " ")
    ability = character.editing

    ability
    |> Ability.set_spectator_message_changeset(message)
    |> Repo.update()
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        Help.execute(room, character, [ability.name])

      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
    end
  end

  defp set_kind(room, character, kind) do
    kind = Enum.join(kind, " ")
    ability = character.editing

    ability
    |> Ability.set_kind_changeset(kind)
    |> Repo.update()
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        Help.execute(room, character, [ability.name])

      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
    end
  end

  defp set_command(room, character, kind) do
    kind = Enum.join(kind, " ")
    ability = character.editing

    ability
    |> Ability.set_command_changeset(kind)
    |> Repo.update()
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        Help.execute(room, character, [ability.name])

      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
    end
  end

  defp set_targets(room, character, targets) do
    targets = Enum.join(targets, " ")
    ability = character.editing

    ability
    |> Ability.set_targets_changeset(targets)
    |> Repo.update()
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        Help.execute(room, character, [ability.name])

      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
    end
  end

  defp set_cast_time(room, character, cast_time) do
    cast_time = Enum.join(cast_time, " ")
    ability = character.editing

    ability
    |> Ability.set_cast_time_changeset(cast_time)
    |> Repo.update()
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        Help.execute(room, character, [ability.name])

      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
    end
  end
end
