defmodule ApathyDrive.Commands.System.Ability do
  alias ApathyDrive.{Ability, AbilityTrait, Mobile, Repo, Room}

  def execute(%Room{} = room, character, ["create" | ability_name]) do
    ability_name = Enum.join(ability_name, " ")

    create(character, ability_name)

    room
  end

  def execute(%Room{} = room, character, ["set", "description" | ability_name]) do
    set_description(character, ability_name)

    room
  end

  def execute(%Room{} = room, character, ["set", "targets" | ability_name]) do
    set_targets(character, ability_name)

    room
  end

  def execute(%Room{} = room, character, ["set", "kind" | ability_name]) do
    set_kind(character, ability_name)

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

  defp set_description(character, ability_name) do
    ability_name
    |> Enum.join(" ")
    |> String.split(" to ")
    |> case do
      [_] ->
        Mobile.send_scroll(character, "<p>Invalid syntax, should be like 'sys ability set description &lt;ability&gt; to &lt;description&gt;', e.g. 'sys ability set description frost jet to Fires a jet of frost at a single opponent.'</p>")
      [ability_name | description] ->
        case Ability.match_by_name(ability_name, true) do
          nil ->
        Mobile.send_scroll(character, "<p>\"#{ability_name}\" does not match any abilities.</p>")
      %Ability{} = ability ->
        description = Enum.join(description, " ")

        ability
        |> Ability.set_description_changeset(description)
        |> Repo.update
        |> case do
          {:ok, _ability} ->
            help(character, ability.name)
          {:error, changeset} ->
            Mobile.send_scroll(character, "<p>#{inspect changeset.errors}</p>")
        end
      matches ->
        Mobile.send_scroll(character, "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>")
        Enum.each(matches, fn(match) ->
          Mobile.send_scroll(character, "<p>-- #{match.name}</p>")
        end)

      end

    end
  end

  defp set_kind(character, ability_name) do
    ability_name
    |> Enum.join(" ")
    |> String.split(" to ")
    |> case do
      [_] ->
        Mobile.send_scroll(character, "<p>Invalid syntax, should be like 'sys ability set kind &lt;ability&gt; to &lt;kind&gt;', e.g. 'sys ability set kind frost jet to attack.'</p>")
      [ability_name | targets] ->
        case Ability.match_by_name(ability_name, true) do
          nil ->
        Mobile.send_scroll(character, "<p>\"#{ability_name}\" does not match any abilities.</p>")
      %Ability{} = ability ->
        targets = Enum.join(targets, " ")

        ability
        |> Ability.set_kind_changeset(targets)
        |> Repo.update
        |> case do
          {:ok, _ability} ->
            help(character, ability.name)
          {:error, changeset} ->
            Mobile.send_scroll(character, "<p>#{inspect changeset.errors}</p>")
        end
      matches ->
        Mobile.send_scroll(character, "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>")
        Enum.each(matches, fn(match) ->
          Mobile.send_scroll(character, "<p>-- #{match.name}</p>")
        end)

      end

    end
  end

  defp set_targets(character, ability_name) do
    ability_name
    |> Enum.join(" ")
    |> String.split(" to ")
    |> case do
      [_] ->
        Mobile.send_scroll(character, "<p>Invalid syntax, should be like 'sys ability set targets &lt;ability&gt; to &lt;targets&gt;', e.g. 'sys ability set targets frost jet to monster or single.'</p>")
      [ability_name | targets] ->
        case Ability.match_by_name(ability_name, true) do
          nil ->
        Mobile.send_scroll(character, "<p>\"#{ability_name}\" does not match any abilities.</p>")
      %Ability{} = ability ->
        targets = Enum.join(targets, " ")

        ability
        |> Ability.set_targets_changeset(targets)
        |> Repo.update
        |> case do
          {:ok, _ability} ->
            help(character, ability.name)
          {:error, changeset} ->
            Mobile.send_scroll(character, "<p>#{inspect changeset.errors}</p>")
        end
      matches ->
        Mobile.send_scroll(character, "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>")
        Enum.each(matches, fn(match) ->
          Mobile.send_scroll(character, "<p>-- #{match.name}</p>")
        end)

      end

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
        Mobile.send_scroll(character, "\n\n<p>Kind: #{ability.kind}</p>")
        Mobile.send_scroll(character, "<p>Targets: #{ability.targets}</p>")

        if ability.mana && ability.mana > 0 do
          Mobile.send_scroll(character, "<p>Base Mana: #{ability.mana}</p>")
          Mobile.send_scroll(character, "<p>Mana Cost: #{Ability.mana_cost_at_level(ability, character.level)}</p>")
        end
        if ability.duration_in_ms && ability.duration_in_ms > 0 do
          Mobile.send_scroll(character, "<p>Duration: #{div(ability.duration_in_ms, 1000)} seconds</p>")
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
