defmodule Components.Skills do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Skills, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_skills, new_value})
  end

  def train(entity, _skill, power, cost) when power < cost do
    Components.Player.send_message(entity, ["scroll", "<p>You need #{cost} power to train that skill.</p>"])
    Components.Player.send_message(entity, ["scroll", "<p>You only have #{power}.</p>"])
  end

  def train(entity, skill, _power, cost) do
    old_stats = Systems.Stat.modified(entity)

    GenEvent.notify(entity, {:train, skill.name, cost})
    rating = skill.base(entity)
    Components.Player.send_message(entity, ["scroll", "<p>You spend #{cost} power to train two handed blade to #{rating}%</p>"])

    new_stats = Systems.Stat.modified(entity)
    new_stats |> Map.keys
              |> Enum.each fn(stat) ->
                   difference = new_stats[stat] - old_stats[stat]
                   if difference > 0 do
                     Components.Player.send_message(entity, ["scroll", "<p>Your #{stat} increases by #{difference}!</p>"])
                   end
                 end

    cost = Systems.Trainer.cost(skill.cost, rating)
    Components.Player.send_message(entity, ["scroll", "<p>It will cost you #{cost} power to advance this skill further.</p>"])
    Components.Player.send_message(entity, ["scroll", "<p>You have #{Systems.Trainer.power(entity)} power left.</p>"])
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

  def handle_event({:train, skill_name, amount}, value) do
    skills = Map.put(value, skill_name, Map.get(value, skill_name, 0) + amount)
    {:ok, skills}
  end

  def handle_event(_, value) do
    {:ok, value}
  end
end
