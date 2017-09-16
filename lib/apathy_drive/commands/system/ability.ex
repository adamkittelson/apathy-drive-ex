defmodule ApathyDrive.Commands.System.Ability do
  alias ApathyDrive.{Ability, AbilityTrait, Character, Mobile, Repo, Room, Trait}

  def execute(%Room{} = room, character, ["create" | ability_name]) do
    ability_name = Enum.join(ability_name, " ")

    create(character, ability_name)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, ["add", "trait" | trait]) do
    add_trait(character, trait)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, ["set", "description" | description]) do
    set_description(character, description)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, ["set", "duration" | duration]) do
    set_duration(character, duration)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, ["set", "mana" | mana]) do
    set_mana(character, mana)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, ["set", "user_message" | message]) do
    set_user_message(character, message)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, ["set", "target_message" | message]) do
    set_target_message(character, message)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, ["set", "spectator_message" | message]) do
    set_spectator_message(character, message)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Ability{}} = character, ["set", "command" | command]) do
    set_command(character, command)

    room
  end

  def execute(%Room{} = room, character, ["set", "targets" | targets]) do
    set_targets(character, targets)

    room
  end

  def execute(%Room{} = room, character, ["set", "kind" | kind]) do
    set_kind(character, kind)

    room
  end

  def execute(%Room{} = room, character, ["set", "cast_time" | time_in_seconds]) do
    set_cast_time(character, time_in_seconds)

    room
  end

  def execute(%Room{} = room, character, ["help" | ability_name]) do
    ability_name = Enum.join(ability_name, " ")

    help(character, ability_name)

    room
  end

  def execute(%Room{} = room, character, _args) do
    Mobile.send_scroll(character, "<p>Invalid system command.</p>")

    room
  end

  defp create(character, ability_name) do
    Repo.insert!(%Ability{name: ability_name}, on_conflict: :nothing)

    help(character, ability_name)
  end

  defp add_trait(character, [trait | value]) do
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
        help(character, ability.name)
    end
  end

  defp set_description(character, description) do
    description = Enum.join(description, " ")
    ability = character.editing

    ability
    |> Ability.set_description_changeset(description)
    |> Repo.update
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        help(character, ability.name)
      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect changeset.errors}</p>")
    end
  end

  defp set_duration(character, duration) do
    duration = Enum.join(duration, " ")
    ability = character.editing

    ability
    |> Ability.set_duration_changeset(duration)
    |> Repo.update
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        help(character, ability.name)
      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect changeset.errors}</p>")
    end
  end

  defp set_mana(character, mana) do
    mana = Enum.join(mana, " ")
    ability = character.editing

    ability
    |> Ability.set_mana_changeset(mana)
    |> Repo.update
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        help(character, ability.name)
      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect changeset.errors}</p>")
    end
  end

  defp set_user_message(character, message) do
    message = Enum.join(message, " ")
    ability = character.editing

    ability
    |> Ability.set_user_message_changeset(message)
    |> Repo.update
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        help(character, ability.name)
      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect changeset.errors}</p>")
    end
  end

  defp set_target_message(character, message) do
    message = Enum.join(message, " ")
    ability = character.editing

    ability
    |> Ability.set_target_message_changeset(message)
    |> Repo.update
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        help(character, ability.name)
      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect changeset.errors}</p>")
    end
  end

  defp set_spectator_message(character, message) do
    message = Enum.join(message, " ")
    ability = character.editing

    ability
    |> Ability.set_spectator_message_changeset(message)
    |> Repo.update
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        help(character, ability.name)
      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect changeset.errors}</p>")
    end
  end

  defp set_kind(character, kind) do
    kind = Enum.join(kind, " ")
    ability = character.editing

    ability
    |> Ability.set_kind_changeset(kind)
    |> Repo.update
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        help(character, ability.name)
      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect changeset.errors}</p>")
    end
  end

  defp set_command(character, kind) do
    kind = Enum.join(kind, " ")
    ability = character.editing

    ability
    |> Ability.set_command_changeset(kind)
    |> Repo.update
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        help(character, ability.name)
      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect changeset.errors}</p>")
    end
  end

  defp set_targets(character, targets) do
    targets = Enum.join(targets, " ")
    ability = character.editing

    ability
    |> Ability.set_targets_changeset(targets)
    |> Repo.update
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        help(character, ability.name)
      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect changeset.errors}</p>")
    end
  end

  defp set_cast_time(character, cast_time) do
    cast_time = Enum.join(cast_time, " ")
    ability = character.editing

    ability
    |> Ability.set_cast_time_changeset(cast_time)
    |> Repo.update
    |> case do
      {:ok, _ability} ->
        ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)
        help(character, ability.name)
      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect changeset.errors}</p>")
    end
  end

  defp help(character, ability_name) do
    case Ability.match_by_name(ability_name, true) do
      nil ->
        Mobile.send_scroll(character, "<p>\"#{ability_name}\" does not match any abilities.</p>")
      %Ability{} = ability ->
        Mobile.send_scroll(character, "<p><span class='dark-cyan'>+------------------------------------------------------------------+</span></p>")
        Mobile.send_scroll(character, "<p>#{ability.name}</p>")
        Mobile.send_scroll(character, "<p>    #{ability.description}</p>")
        Mobile.send_scroll(character, "\n\n<p>Command: #{ability.command}</p>")
        Mobile.send_scroll(character, "<p>Kind: #{ability.kind}</p>")
        Mobile.send_scroll(character, "<p>Targets: #{ability.targets}</p>")

        if ability.mana && ability.mana > 0 do
          Mobile.send_scroll(character, "<p>Base Mana: #{ability.mana}</p>")
          Mobile.send_scroll(character, "<p>Mana Cost: #{Ability.mana_cost_at_level(ability, character.level)}</p>")
        end
        if ability.duration && ability.duration > 0 do
          Mobile.send_scroll(character, "<p>Duration: #{ability.duration} seconds</p>")
        end
        if ability.cast_time && ability.cast_time > 0 do
          Mobile.send_scroll(character, "<p>Cast Time: #{ability.cast_time} seconds</p>")
        end
        Mobile.send_scroll(character, "\n\n<p>Messages:</p>")
        Mobile.send_scroll(character, "<p>    #{ability.user_message}</p>")
        Mobile.send_scroll(character, "<p>    #{ability.target_message}</p>")
        Mobile.send_scroll(character, "<p>    #{ability.spectator_message}</p>")

        traits = AbilityTrait.load_traits(ability.id)

        Mobile.send_scroll(character, "\n\n<p>Traits:</p>")
        Enum.each(traits, fn
          {name, nil} ->
            Mobile.send_scroll(character, "<p>  #{name}</p>")
          {name, value} ->
            Mobile.send_scroll(character, "<p>  #{name}: #{inspect value}</p>")
        end)

        Mobile.send_scroll(character, "<p><span class='dark-cyan'>+------------------------------------------------------------------+</span></p>")
      matches ->
        Mobile.send_scroll(character, "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>")
        Enum.each(matches, fn(match) ->
          Mobile.send_scroll(character, "<p>-- #{match.name}</p>")
        end)
    end
  end

end
