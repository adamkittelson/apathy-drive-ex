defmodule Systems.Trainer do
  use Systems.Reload
  import Utility

  def list(spirit, nil, _room) do
    send_message(spirit, "scroll", "<p>You can learn nothing here.</p>")
  end

  def list(spirit, monster, room) do
    power = monster_power(monster) + spirit_power(spirit)
    header = "<span class='blue'>-=-=-=-=-=-=-=-</span>  <span class='white'>Skill Listing</span>  <span class='blue'>-=-=-=-=-=-=-=-</span>"
    send_message(spirit, "scroll", "<p>#{header}</p>")
    skills_by_level(room) |> Map.keys |> Enum.each fn level ->
      row = "Level#{String.rjust("#{level}", 3)} -------------------- Cost ----- Rating"
      send_message(spirit, "scroll", "<p><span class='blue'>#{row}</span></p>")
      skills_by_level(room)[level] |> Enum.sort |> Enum.each fn skill ->
        skill_name = String.ljust(skill.name, 26)
        cost = cost(monster, skill)
        if power < cost do
          cost = "<span class='dark-red'>#{"#{cost}" |> String.ljust(8)}</span>"
        else
          cost = "<span class='green'>#{"#{cost}" |> String.ljust(8)}</span>"
        end
        rating = "#{"#{rating(skill, monster)}" |> String.rjust(4)}</span>"
        row = "    #{skill_name}#{cost}#{rating}%"
        send_message(spirit, "scroll", "<p>#{row}</p>")
      end
    end
    footer = "<span class='blue'>-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-</span>"
    send_message(spirit, "scroll", "<p>#{footer}</p>")
  end

  def train(entity, _room, []) do
    send_message(entity, "scroll", "<p>Which skill would you like to train?</p>")
  end

  def train(entity, room, args) do
    args   = Enum.join(args, " ")
    skills = Components.Trainer.skills(room)

    case Systems.Match.all(skills, :keyword_starts_with, args) do
      [] ->
        send_message(entity, "scroll", "<p>You cannot train #{args} here.</p>")
      [match] ->
        train(entity, Components.Module.value(match))
      matches ->
        send_message(entity, "scroll", "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>")
        Enum.each matches, fn(match) ->
          send_message(entity, "scroll", "<p>-- #{Components.Name.value(match)}</p>")
        end
    end
  end

  def train(monster, skill) do
    devs = monster_power(monster)
    cost = cost(monster, skill)
    Components.Skills.train(monster, skill, devs, cost)
  end

  def spirit_power(spirit) do
    total_power(spirit) - Components.Investments.power_invested(spirit)
  end

  def monster_power(monster) do
    total_power(monster) - Components.Skills.power_spent(monster) + Components.Investments.power_invested(monster)
  end

  def total_power(entity) when is_pid(entity) do
    level       = Components.Level.value(entity)
    level_power = total_power(level)
    exp         = Components.Experience.value(entity)
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

  def cost(entity, skill) when is_atom(skill) do
    cost(skill.cost, rating(skill, entity))
  end

  def cost(modifier, rating) when is_integer(rating) do
    [rating * modifier * 1.0 |> Float.ceil |> trunc, 1] |> Enum.max
  end

  def rating(skill, entity) when is_pid(entity) do
    rating(skill.cost, Components.Skills.power_spent(entity, skill))
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

  def skills_by_level(room) do
    room
    |> Components.Trainer.value
    |> Enum.reduce %{}, fn skill_name, skills ->
         skill = Skills.find(skill_name)
         skills = Map.put_new(skills, skill.level, [])
         Map.put(skills, skill.level, [skill | skills[skill.level]])
       end
  end
end
