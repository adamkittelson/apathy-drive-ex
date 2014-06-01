defmodule Systems.Trainer do
  def list(character, room) do
    devs = devs(character)
    header = "<span class='blue'>-=-=-=-=-=-=-=-</span>  <span class='white'>Skill Listing</span>  <span class='blue'>-=-=-=-=-=-=-=-</span>"
    Components.Player.send_message(character, ["scroll", "<p>#{header}</p>"])
    skills_by_level(room) |> Map.keys |> Enum.each fn level ->
      row = "Level#{String.rjust("#{level}", 3)} -------------------- Cost ----- Rating"
      Components.Player.send_message(character, ["scroll", "<p><span class='blue'>#{row}</span></p>"])
      skills_by_level(room)[level] |> Enum.each fn skill ->
        skill_name = Components.Name.value(skill) |> String.ljust(26)
        cost = cost(character, skill)
        if devs < cost do
          cost = "<span class='dark-red'>#{"#{cost}" |> String.ljust(8)}</span>"
        else
          cost = "<span class='green'>#{"#{cost}" |> String.ljust(8)}</span>"
        end
        rating = "#{"#{rating(skill, character)}" |> String.rjust(4)}</span>"
        row = "    #{skill_name}#{cost}#{rating}%"
        Components.Player.send_message(character, ["scroll", "<p>#{row}</p>"])
      end
    end
    footer = "<span class='blue'>-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-</span>"
    Components.Player.send_message(character, ["scroll", "<p>#{footer}</p>"])
  end

  def train(entity, _room, []) do
    Components.Player.send_message(entity, ["scroll", "<p>Which skill would you like to train?</p>"])
  end

  def train(entity, room, args) do
    args   = Enum.join(args, " ")
    skills = Components.Trainer.skills(room)

    case Systems.Match.all(skills, :keyword_starts_with, args) do
      [] ->
        Components.Player.send_message(entity, ["scroll", "<p>You cannot train #{args} here.</p>"])
      [match] ->
        train(entity, match)
      matches ->
        Components.Player.send_message(entity, ["scroll", "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"])
        Enum.each matches, fn(match) ->
          Components.Player.send_message(entity, ["scroll", "<p>-- #{Components.Name.value(match)}</p>"])
        end
    end
  end

  def train(entity, skill) do
    devs = devs(entity)
    cost = cost(entity, skill)
    Components.Skills.train(entity, skill, devs, cost)
  end

  def devs(entity) do
    total_devs(entity) - (entity |> Components.Skills.value |> Map.values |> Enum.sum)
  end

  def total_devs(entity) when is_pid(entity) do
    level = Components.Level.value(entity)
    total_devs(level)
  end

  def total_devs(level) when is_integer(level) do
    total_devs(level, 0)
  end

  def total_devs(level, devs) when level > 0 do
    total_devs(level - 1, devs + (100 * (level - 1)))
  end

  def total_devs(0, devs) do
    1000 + devs
  end

  def cost(entity, skill) when is_pid(skill) do
    modifier = Components.Cost.value(skill)
    cost(modifier, rating(skill, entity))
  end

  def cost(modifier, rating) when is_integer(rating) do
    [rating * modifier |> Float.ceil, 1] |> Enum.max
  end

  def rating(skill, entity) when is_pid(entity) do
    modifier = Components.Cost.value(skill)
    rating(modifier, devs_spent(entity, skill))
  end

  def rating(modifier, devs_spent) when is_integer(devs_spent) do
    rating(0, modifier, cost(modifier, 0), devs_spent)
  end

  def rating(rating, modifier, cost, devs) when devs >= cost do
    new_rating = rating + 1
    new_cost = cost(modifier, new_rating)
    rating(new_rating, modifier, new_cost, devs - cost)
  end

  def rating(rating, _modifier, cost, devs) when devs < cost do
    rating
  end

  def devs_spent(entity, skill) do
    Components.Skills.value(entity)
    |> Map.get(Components.Name.value(skill), 0)
  end

  def skills_by_level(room) do
    room
    |> Components.Trainer.value
    |> Enum.reduce %{}, fn skill_name, skills ->
         skill = Skills.all[skill_name]
         level = Components.Level.value(skill)
         skills = Map.put_new(skills, level, [])
         Map.put(skills, level, [skill | skills[level]])
       end
  end
end