defmodule Components.Skills do
  use Systems.Reload
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.Skills, :value)
  end

  def value(entity, new_value) do
    Entity.notify(entity, {:set_skills, new_value})
  end

  def train(entity, _skill, devs, cost) when devs < cost do
    Components.Player.send_message(entity, ["scroll", "<p>You need #{cost} development points to train that skill.</p>"])
    Components.Player.send_message(entity, ["scroll", "<p>You only have #{devs}.</p>"])
  end

  def train(entity, skill, _devs, cost) do
    Entity.notify(entity, {:train, skill.name, cost})
    rating = Systems.Trainer.rating(skill, entity)
    Components.Player.send_message(entity, ["scroll", "<p>You spend #{cost} development points to train two handed blade to #{rating}%</p>"])
    cost = Systems.Trainer.cost(skill.cost, rating)
    Components.Player.send_message(entity, ["scroll", "<p>It will cost you #{cost} development points to advance this skill further.</p>"])
    Components.Player.send_message(entity, ["scroll", "<p>You have #{Systems.Trainer.devs(entity)} development points left.</p>"])
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
