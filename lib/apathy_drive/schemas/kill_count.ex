defmodule ApathyDrive.KillCount do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{ChannelHistory, Character, KillCount, Mobile, Monster, Repo}

  schema "kill_counts" do
    belongs_to(:character, Character)
    belongs_to(:monster, Monster)

    field(:daily_count, :integer)
    field(:daily_reset_at, :utc_datetime)
    field(:weekly_count, :integer)
    field(:weekly_reset_at, :utc_datetime)
    field(:lifetime_count, :integer)

    field(:monster_name, :string, virtual: true)
  end

  def increment(%Character{kill_counts: counts} = character, %Monster{id: monster_id} = monster) do
    with {:kc, %KillCount{} = kill_count} <- {:kc, Map.get(counts, monster_id)},
         kill_count <- reset_counters(kill_count) do
      kill_count =
        kill_count
        |> Ecto.Changeset.change(%{
          daily_count: kill_count.daily_count + 1,
          weekly_count: kill_count.weekly_count + 1,
          lifetime_count: kill_count.lifetime_count + 1
        })
        |> Repo.update!()

      character
      |> put_in([Access.key!(:kill_counts), monster_id], kill_count)
      |> give_bonus_experience(kill_count, monster.experience)
    else
      {:kc, nil} ->
        put_in(character.kill_counts[monster_id], load(character, monster_id))
        |> increment(monster)
    end
  end

  def load(%Character{id: character_id}, monster_id) do
    monster_name =
      Monster
      |> Ecto.Query.select([:name])
      |> Repo.get!(monster_id)
      |> Map.get(:name)

    case Repo.get_by(KillCount, character_id: character_id, monster_id: monster_id) do
      %KillCount{} = kill_count ->
        Map.put(kill_count, :monster_name, monster_name)

      nil ->
        %KillCount{
          character_id: character_id,
          monster_id: monster_id,
          monster_name: monster_name,
          daily_count: 0,
          daily_reset_at: tomorrow(),
          weekly_count: 0,
          weekly_reset_at: next_week(),
          lifetime_count: 0
        }
        |> Repo.insert!()
    end
  end

  # used when moving from one area to the next to avoid keeping kill counts
  # in memory for monsters that the player is no where near
  def clear_kill_counts(%Character{} = character), do: Map.put(character, :kill_counts, %{})
  def clear_kill_counts(mobile), do: mobile

  defp give_bonus_experience(%Character{} = character, %KillCount{} = kill_count, exp) do
    {character, kills_required} =
      {character, []}
      |> give_bonus_experience(kill_count, :lifetime_count, exp, 2)
      |> give_bonus_experience(kill_count, :daily_count, exp, 2)
      |> give_bonus_experience(kill_count, :weekly_count, exp, 2)

    if Enum.any?(kills_required) do
      kills = Enum.min(kills_required)

      Mobile.send_scroll(
        character,
        "<p>#{kills} more #{kill_count.monster_name} kills until bonus.</p>"
      )
    end

    character
  end

  defp give_bonus_experience({character, kills_required}, kill_count, count, exp, multiplier) do
    case give_bonus_experience?(kill_count, count, multiplier) do
      true ->
        bonus = exp * multiplier

        modifier = 1 / ((character.race.exp_modifier + character.class.exp_modifier) / 100)
        exp = trunc(bonus * modifier)

        message = message(kill_count, count, exp)

        character =
          if count == :lifetime_count do
            Repo.insert!(%ChannelHistory{
              character_id: character.id,
              message: message
            })

            character
            |> Character.send_chat(message)
            |> Character.add_experience(bonus, true)
          else
            character
            |> Mobile.send_scroll(message)
            |> Character.add_experience(bonus, true)
          end

        {character, kills_required}

      {:done, kills_left} ->
        {character, [kills_left | kills_required]}

      false ->
        give_bonus_experience(
          {character, kills_required},
          kill_count,
          count,
          exp,
          multiplier * 2
        )
    end
  end

  defp give_bonus_experience?(kill_count, count, multiplier) do
    count = Map.get(kill_count, count)

    cond do
      multiplier > count ->
        {:done, multiplier - count}

      count == multiplier ->
        true

      :else ->
        false
    end
  end

  defp reset_counters(%KillCount{} = kill_count) do
    kill_count
    |> reset_counter(:daily_count, :daily_reset_at)
    |> reset_counter(:weekly_count, :weekly_reset_at)
  end

  defp reset_counter(%KillCount{} = kill_count, count, reset_at) do
    if DateTime.compare(DateTime.utc_now(), Map.get(kill_count, reset_at)) == :gt do
      kill_count
      |> Ecto.Changeset.change(%{
        count => 0,
        reset_at => updated_reset_at(reset_at)
      })
      |> Repo.update!()
    else
      kill_count
    end
  end

  defp updated_reset_at(:daily_reset_at), do: tomorrow()
  defp updated_reset_at(:weekly_reset_at), do: next_week()

  defp tomorrow do
    DateTime.utc_now() |> Timex.shift(days: 1) |> Timex.beginning_of_day()
  end

  defp next_week do
    DateTime.utc_now() |> Timex.shift(weeks: 1) |> Timex.beginning_of_week()
  end

  defp message(kill_count, count, bonus) do
    number =
      kill_count
      |> Map.get(count)
      |> Ordinal.ordinalize()

    "<p><span class='yellow'>You receive #{bonus} bonus experience for killing your #{number} #{
      kill_count.monster_name
    }#{count_type(count)}!</span></p>"
  end

  defp count_type(:lifetime_count), do: ""
  defp count_type(:weekly_count), do: " this week"
  defp count_type(:daily_count), do: " today"
end
