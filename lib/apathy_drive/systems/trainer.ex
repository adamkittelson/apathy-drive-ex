defmodule Systems.Trainer do
  use Systems.Reload
  import Utility

  def list(character, room) do
    power = power(character)
    header = "<span class='blue'>-=-=-=-=-=-=-=-</span>  <span class='white'>Skill Listing</span>  <span class='blue'>-=-=-=-=-=-=-=-</span>"
    send_message(character, "scroll", "<p>#{header}</p>")
    skills_by_level(room) |> Map.keys |> Enum.each fn level ->
      row = "Level#{String.rjust("#{level}", 3)} -------------------- Cost ----- Rating"
      send_message(character, "scroll", "<p><span class='blue'>#{row}</span></p>")
      skills_by_level(room)[level] |> Enum.each fn skill ->
        skill_name = String.ljust(skill.name, 26)
        cost = cost(character, skill)
        if power < cost do
          cost = "<span class='dark-red'>#{"#{cost}" |> String.ljust(8)}</span>"
        else
          cost = "<span class='green'>#{"#{cost}" |> String.ljust(8)}</span>"
        end
        rating = "#{"#{rating(skill, character)}" |> String.rjust(4)}</span>"
        row = "    #{skill_name}#{cost}#{rating}%"
        send_message(character, "scroll", "<p>#{row}</p>")
      end
    end
    footer = "<span class='blue'>-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-</span>"
    send_message(character, "scroll", "<p>#{footer}</p>")
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

  def train(entity, skill) do
    power = power(entity)
    cost = cost(entity, skill)
    Components.Skills.train(entity, skill, power, cost)
  end

  def power(entity) do
    total_power(entity) - racial_power_cost(entity) - Components.Skills.power_spent(entity)
  end

  def racial_power_cost(entity) do
    if Entity.has_component?(entity, Components.Race) do
      race = entity
             |> Components.Race.value
             |> Components.Module.value
      race.cost
    else
      0
    end
  end

  def total_power(entity) when is_pid(entity) do
    level = Components.Level.value(entity)
    total_power(level)
  end

  def total_power(level) when is_integer(level) do
    total_power(level, 0)
  end

  def total_power(level, power) when level > 0 do
    total_power(level - 1, power + (100 * (level - 1)))
  end

  def total_power(0, power) do
    1000 + power
  end

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
