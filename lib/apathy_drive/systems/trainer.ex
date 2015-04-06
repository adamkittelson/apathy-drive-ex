defmodule Systems.Trainer do

  def list(%Spirit{} = spirit, %Room{} = room) do
    devs = spirit_power(spirit)

    Spirit.send_scroll(spirit, "<p><span class='blue'>-=-=-=-=-=-=-=-</span>  <span class='white'>Skill Listing</span>  <span class='blue'>-=-=-=-=-=-=-=-</span></p>")
    skills_by_level(room) |> Map.keys |> Enum.each fn level ->
      row = "Level#{String.rjust("#{level}", 3)} -------------------- Cost ----- Rating"
      Spirit.send_scroll(spirit, "<p><span class='blue'>#{row}</span></p>")
      skills_by_level(room)[level] |> Enum.sort |> Enum.each fn skill ->
        skill_name = String.ljust(skill.name, 26)
        cost = cost(spirit, skill)
        if devs < cost do
          cost = "<span class='dark-red'>#{"#{cost}" |> String.ljust(8)}</span>"
        else
          cost = "<span class='green'>#{"#{cost}" |> String.ljust(8)}</span>"
        end
        rating = "#{"#{Spirit.skill(spirit, skill.name)}" |> String.rjust(4)}</span>"
        row = "    #{skill_name}#{cost}#{rating}%"
        Spirit.send_scroll(spirit, "<p>#{row}</p>")
      end
    end
    footer = "<span class='blue'>-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-</span>"
    Spirit.send_scroll(spirit, "<p>#{footer}</p>")
  end

  def list(%Monster{} = monster, _room) do
    monster
    |> Monster.send_scroll("<p>You can learn nothing here.</p>")
  end

  def train(%Spirit{} = spirit, _room, []) do
    Spirit.send_scroll(spirit, "<p>Which skill would you like to train?</p>")
  end

  def train(%Spirit{} = spirit, room, args) do
    args   = Enum.join(args, " ")
    skills = Skill.all(room)

    case Systems.Match.all(skills, :keyword_starts_with, args) do
      [] ->
        Spirit.send_scroll(spirit, "<p>You cannot train #{args} here.</p>")
      [match] ->
        train(spirit, match)
      matches ->
        Spirit.send_scroll(spirit, "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>")
        Enum.each matches, fn(match) ->
          Spirit.send_scroll(spirit, "scroll", "<p>-- #{match.name}</p>")
        end
    end
  end

  def train(%Spirit{} = spirit, %Skill{} = skill) do
    devs = spirit_power(spirit)
    cost = cost(spirit, skill)

    if devs < cost do
      Spirit.send_scroll(spirit, "<p>You need #{cost} development points to train that skill.</p>")
      Spirit.send_scroll(spirit, "<p>You only have #{devs}.</p>")
    else
      old_abilities = spirit.abilities |> Enum.map(&(&1.name))

      skills = Map.put_new(spirit.skills, skill.name, 0)
      current = get_in(skills, [skill.name])
      new = put_in skills[skill.name], current + cost
      spirit = Map.put(spirit, :skills, new)

      rating = Spirit.skill(spirit, skill.name)

      new_devs = max(0, devs - cost)
      spent = devs - new_devs

      spirit = Spirit.save(spirit)

      Spirit.send_scroll(spirit, "<p>You spend #{spent} development points to train #{skill.name} to #{rating}%</p>")

      spirit = Spirit.set_abilities(spirit)

      new_abilities = spirit.abilities |> Enum.map(&(&1.name))

      new_abilities
      |> Enum.each(fn(ability) ->
           if !Enum.member?(old_abilities, ability) do
             Spirit.send_scroll(spirit, "<p><span class='dark-cyan'>You learn #{ability}!</span></p>")
           end
         end)

      cost = cost(spirit, skill)

      spirit
      |> Spirit.send_scroll("<p>It will cost you #{cost} development points to advance this skill further.</p>")
      |> Spirit.send_scroll("<p>You have #{new_devs} development points left.</p>")
    end
  end

  def spirit_power(%Spirit{} = spirit) do
    total_power(spirit) - power_spent(spirit)
  end

  def power_spent(%Spirit{skills: skills} = spirit) do
    skills
    |> Map.keys
    |> Enum.map(&power_spent(spirit, &1))
    |> Enum.sum
  end

  def power_spent(%Spirit{skills: skills}, skill_name) do
    get_in(skills, [skill_name]) || 0
  end

  def total_power(%Spirit{level: level, experience: exp}) do
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

  def cost(%Spirit{} = spirit, skill) do
    cost(skill.cost, rating(skill, spirit))
  end

  def cost(modifier, rating) when is_integer(rating) do
    [rating * modifier * 1.0 |> Float.ceil |> trunc, 1] |> Enum.max
  end

  def rating(skill, %Spirit{} = spirit) do
    rating(skill.cost, power_spent(spirit, skill.name))
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
