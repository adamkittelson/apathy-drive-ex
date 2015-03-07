defmodule Systems.Trainer do



  def list(%Spirit{} = spirit, %Room{}) do
    spirit
    |> Spirit.send_scroll("<p>You can learn nothing here.</p>")
  end

  def list(%Monster{} = monster, room) do
    devs = monster_power(monster)

    Monster.send_scroll(monster, "<p><span class='blue'>-=-=-=-=-=-=-=-</span>  <span class='white'>Skill Listing</span>  <span class='blue'>-=-=-=-=-=-=-=-</span></p>")
    skills_by_level(room) |> Map.keys |> Enum.each fn level ->
      row = "Level#{String.rjust("#{level}", 3)} -------------------- Cost ----- Rating"
      Monster.send_scroll(monster, "<p><span class='blue'>#{row}</span></p>")
      skills_by_level(room)[level] |> Enum.sort |> Enum.each fn skill ->
        skill_name = String.ljust(skill.name, 26)
        cost = cost(monster, skill)
        if devs < cost do
          cost = "<span class='dark-red'>#{"#{cost}" |> String.ljust(8)}</span>"
        else
          cost = "<span class='green'>#{"#{cost}" |> String.ljust(8)}</span>"
        end
        rating = "#{"#{Monster.modified_skill(monster, skill.name)}" |> String.rjust(4)}</span>"
        row = "    #{skill_name}#{cost}#{rating}%"
        Monster.send_scroll(monster, "<p>#{row}</p>")
      end
    end
    footer = "<span class='blue'>-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-</span>"
    Monster.send_scroll(monster, "<p>#{footer}</p>")
  end

  def train(%Monster{} = monster, _room, []) do
    Monster.send_scroll(monster, "<p>Which skill would you like to train?</p>")
  end

  def train(%Monster{} = monster, room, args) do
    args   = Enum.join(args, " ")
    skills = Skill.all(room)

    case Systems.Match.all(skills, :keyword_starts_with, args) do
      [] ->
        Monster.send_scroll(monster, "<p>You cannot train #{args} here.</p>")
      [match] ->
        train(monster, match)
      matches ->
        Monster.send_scroll(monster, "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>")
        Enum.each matches, fn(match) ->
          Monster.send_scroll(monster, "scroll", "<p>-- #{match.name}</p>")
        end
    end
  end

  def train(%Monster{} = monster, %Skill{} = skill) do
    devs = monster_power(monster)
    cost = cost(monster, skill)

    if devs < cost do
      Monster.send_scroll(monster, "<p>You need #{cost} development points to train that skill.</p>")
      Monster.send_scroll(monster, "<p>You only have #{devs}.</p>")
    else
      old_strength     = Monster.modified_stat(monster, "strength")
      old_agility      = Monster.modified_stat(monster, "agility")
      old_intelligence = Monster.modified_stat(monster, "intelligence")
      old_health       = Monster.modified_stat(monster, "health")

      old_abilities = monster.abilities |> Enum.map(&(&1.name))


      skills = Map.put_new(monster.skills, skill.name, %{})
      current = get_in(skills, [skill.name, "trained"]) || 0
      new = put_in skills[skill.name]["trained"], current + cost
      monster = Map.put(monster, :skills, new)

      rating = Monster.modified_skill(monster, skill.name)

      new_devs = max(0, devs - cost)
      spent = devs - new_devs

      monster = Monster.save(monster)

      Monster.send_scroll(monster, "<p>You spend #{spent} development points to train #{skill.name} to #{rating}%</p>")

      new_strength     = Monster.modified_stat(monster, "strength")
      new_agility      = Monster.modified_stat(monster, "agility")
      new_intelligence = Monster.modified_stat(monster, "intelligence")
      new_health       = Monster.modified_stat(monster, "health")

      difference = new_strength - old_strength
      if difference > 0 do
        Monster.send_scroll(monster, "<p>Your strength increases by #{difference}!</p>")
      end

      difference = new_agility - old_agility
      if difference > 0 do
        Monster.send_scroll(monster, "<p>Your agility increases by #{difference}!</p>")
      end

      difference = new_intelligence - old_intelligence
      if difference > 0 do
        Monster.send_scroll(monster, "<p>Your intelligence increases by #{difference}!</p>")
      end

      difference = new_health - old_health
      if difference > 0 do
        Monster.send_scroll(monster, "<p>Your health increases by #{difference}!</p>")
      end

      monster = Monster.set_abilities(monster)

      new_abilities = monster.abilities |> Enum.map(&(&1.name))

      new_abilities
      |> Enum.each(fn(ability) ->
           if !Enum.member?(old_abilities, ability) do
             Monster.send_scroll(monster, "<p><span class='dark-cyan'>You learn #{ability}!</span></p>")
           end
         end)

      cost = cost(monster, skill)

      monster
      |> Monster.send_scroll("<p>It will cost you #{cost} development points to advance this skill further.</p>")
      |> Monster.send_scroll("<p>You have #{new_devs} development points left.</p>")
      |> Monster.save
    end
  end

  def monster_power(%Monster{} = monster) do
    total_power(monster) - power_spent(monster)
  end

  def power_spent(%Monster{skills: skills} = monster) do
    skills
    |> Map.keys
    |> Enum.map(&power_spent(monster, &1))
    |> Enum.sum
  end

  def power_spent(%Monster{skills: skills}, skill_name) do
    get_in(skills, [skill_name, "trained"]) || 0
  end

  def total_power(%Monster{level: level, experience: exp}) do
    level_power = total_power(level)
    tolevel     = Systems.Level.exp_at_level(level + 1)
    percent     = exp / tolevel
    100 + level_power + round((total_power(level + 1) - level_power) * percent)
  end

  def total_power(level) when is_integer(level) do
    total_power(level, 0)
  end

  def total_power(level, power) when level > 0 do
    total_power(level - 1, power + (100 * (level - 1)))
  end

  def total_power(0, power), do: power

  def cost(%Monster{} = monster, skill) do
    cost(skill.cost, rating(skill, monster))
  end

  def cost(modifier, rating) when is_integer(rating) do
    [rating * modifier * 1.0 |> Float.ceil |> trunc, 1] |> Enum.max
  end

  def rating(skill, %Monster{} = monster) do
    rating(skill.cost, power_spent(monster, skill.name))
  end

  def rating(modifier, power_spent) when is_integer(power_spent) do
    rating(0, modifier, cost(modifier, 0), power_spent)
  end

  def rating(rating, modifier, cost, power) when power >= cost do
    new_rating = rating + 1
    new_cost = cost(modifier, new_rating)
    rating(new_rating, modifier, new_cost, power - cost)
  end

  def rating(rating, _modifier, cost, power) when power < cost do
    rating
  end

  def skills_by_level(%Room{} = room) do
    Skill.all(room)
    |> Enum.reduce %{}, fn skill, skills ->
         skills = Map.put_new(skills, skill.level, [])
         Map.put(skills, skill.level, [skill | skills[skill.level]])
       end
  end
end
