defmodule Components.Skills do
  use Systems.Reload
  use GenEvent
  import Utility

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Skills, :value)
  end

  def list(entity) do
    GenEvent.call(entity, Components.Skills, :list)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_skills, new_value})
  end

  def trained(entity) do
    GenEvent.call(entity, Components.Skills, :trained)
  end

  def power_spent(entity, skill) when is_atom(skill) do
    power_spent(entity, skill.name)
  end

  def power_spent(entity, skill_name) do
    GenEvent.call(entity, Components.Skills, {:power_spent, skill_name})
  end

  def power_spent(entity) do
    entity
    |> list
    |> Enum.map(&power_spent(entity, &1))
    |> Enum.sum
  end

  def set_base_skills(_entity, nil), do: nil
  def set_base_skills(entity, skills) do
    GenEvent.notify(entity, {:set_base_skills, skills})
  end

  def base(entity, skill_name) do
    GenEvent.call(entity, Components.Skills, {:base_skill, skill_name})
  end

  def train(entity, _skill, power, cost) when power < cost do
    send_message(entity, "scroll", "<p>You need #{cost} power to train that skill.</p>")
    send_message(entity, "scroll", "<p>You only have #{power}.</p>")
  end

  def train(entity, skill, _power, cost) do
    old_stats = Systems.Stat.modified(entity)
    old_abilities = Components.Abilities.names(entity)

    GenEvent.notify(entity, {:train, skill.name, cost})
    rating = skill.base(entity)
    send_message(entity, "scroll", "<p>You spend #{cost} power to train #{skill.name} to #{rating}%</p>")

    new_stats = Systems.Stat.modified(entity)
    new_stats |> Map.keys
              |> Enum.each fn(stat) ->
                   difference = new_stats[stat] - old_stats[stat]
                   if difference > 0 do
                     send_message(entity, "scroll", "<p>Your #{stat} increases by #{difference}!</p>")
                   end
                 end

    Components.Abilities.reset_abilities(entity)
    new_abilities = Components.Abilities.names(entity)

    new_abilities
    |> Enum.each(fn(ability) ->
         if !Enum.member?(old_abilities, ability) do
           send_message(entity, "scroll", "<p>You've learned #{ability}!</p>")
         end
       end)

    cost = Systems.Trainer.cost(skill.cost, skill.trained(entity))
    send_message(entity, "scroll", "<p>It will cost you #{cost} power to advance this skill further.</p>")
    send_message(entity, "scroll", "<p>You have #{Systems.Trainer.power(entity)} power left.</p>")
    Entities.save!(entity)
  end

  def serialize(entity) do
    %{"Skills" => value(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_call(:list, value) do
    {:ok, Map.keys(value), value}
  end

  def handle_call(:trained, value) do
    {:ok, Map.get(value, "trained", %{}), value}
  end

  def handle_call({:power_spent, skill_name}, value) do
    {:ok, get_in(value, [skill_name, "trained"]) || 0, value}
  end

  def handle_call({:base_skill, skill_name}, value) do
    {:ok, get_in(value, [skill_name, "base"]) || 0, value}
  end

  def handle_event({:set_skills, new_value}, _value) do
    {:ok, new_value}
  end

  def handle_event({:set_base_skills, base_skills}, value) do
    new_value = base_skills
                |> Map.keys
                |> Enum.reduce(value, fn(skill_name, skills) ->
                     skills = Map.put_new(skills, skill_name, %{})
                     put_in skills[skill_name]["base"], base_skills[skill_name]
                   end)
    {:ok, new_value}
  end

  def handle_event({:train, skill_name, amount}, value) do
    value = Map.put_new(value, skill_name, %{})
    current = get_in(value, [skill_name, "trained"]) || 0
    new = put_in value[skill_name]["trained"], current + amount
    {:ok, new}
  end

  def handle_event(_, value) do
    {:ok, value}
  end
end
