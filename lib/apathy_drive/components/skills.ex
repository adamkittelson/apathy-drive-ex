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

  def train(entity, _skill, devs, cost) when devs < cost do
    send_message(entity, "scroll", "<p>You need #{cost} development points to train that skill.</p>")
    send_message(entity, "scroll", "<p>You only have #{devs}.</p>")
  end

  def train(monster, skill, devs, cost) do
    old_stats = Systems.Stat.modified(monster)
    old_abilities = Components.Abilities.names(monster)

    GenEvent.notify(monster, {:train, skill.name, cost})
    rating = skill.modified(monster)

    new_devs = max(0, devs - cost)
    spent = devs - new_devs

    Entities.save!(monster)

    send_message(monster, "scroll", "<p>You spend #{spent} development points to train #{skill.name} to #{rating}%</p>")

    new_stats = Systems.Stat.modified(monster)
    new_stats |> Map.keys
              |> Enum.each fn(stat) ->
                   difference = new_stats[stat] - old_stats[stat]
                   if difference > 0 do
                     send_message(monster, "scroll", "<p>Your #{stat} increases by #{difference}!</p>")
                   end
                 end

    Components.Abilities.reset_abilities(monster)
    new_abilities = Components.Abilities.names(monster)

    new_abilities
    |> Enum.each(fn(ability) ->
         if !Enum.member?(old_abilities, ability) do
           send_message(monster, "scroll", "<p><span class='dark-cyan'>You learn #{ability}!</span></p>")
         end
       end)

    cost = Systems.Trainer.cost(skill.cost, skill.trained(monster))
    send_message(monster, "scroll", "<p>It will cost you #{cost} development points to advance this skill further.</p>")
    send_message(monster, "scroll", "<p>You have #{new_devs} development points left.</p>")
    Entities.save!(monster)
    HPRegen.add(monster)
    ManaRegen.add(monster)
    Entities.save!(Parent.of(monster))
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
