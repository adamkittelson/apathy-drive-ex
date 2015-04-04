defmodule Systems.Combat do

  def dodge?(accuracy, target) when is_pid(target) do
    dodge_skill = Skills.Dodge.modified(target)
    dodge?(accuracy, dodge_skill)
  end

  

  def parry?(accuracy, target) when is_pid(target) do
    parry_skill = Skills.Parry.modified(target)
    parry?(accuracy, parry_skill)
  end

  def parry?(accuracy, parry_skill) do
    chance = 30
    if parry_skill > 0 do
      difference = parry_skill - accuracy
      chance = if difference > 0 do
        chance + difference * 0.2
      else
        chance + difference * 0.3
      end

      :random.seed(:os.timestamp)
      :random.uniform(100) < trunc(chance)
    else
      false
    end
  end


end
