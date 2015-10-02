defmodule ApathyDrive.AI do
  alias ApathyDrive.Mobile

  def think(%Mobile{} = mobile) do
    #heal(monster) || bless(monster) ||
    attack(mobile) || mobile
  end

  def heal(%Monster{hp: hp, max_hp: max_hp, spirit: nil} = monster) do
    unless Monster.on_global_cooldown?(monster) do
      chance = trunc((max_hp - hp) / max_hp * 100)

      roll = :random.uniform(100)

      if chance > roll do
        ability = monster
                  |> Monster.heal_abilities
                  |> random_ability

        if ability do
          send(self, {:execute_ability, ability})
          monster
        end
      end
    end
  end
  def heal(%Monster{}), do: nil

  def bless(%Monster{spirit: nil} = monster) do
    unless Monster.on_global_cooldown?(monster) do
      ability = monster
                |> Monster.bless_abilities
                |> random_ability

      if ability do
        send(self, {:execute_ability, ability})
        monster
      end
    end
  end
  def bless(%Monster{}), do: nil

  def attack(%Mobile{spirit: nil} = mobile) do
    if target = Mobile.aggro_target(mobile) do

      attack = cond do
        !Ability.on_global_cooldown?(mobile) ->
           mobile
           |> Ability.attack_abilities
           |> random_ability
        true ->
          nil
      end

      if attack do
        send(self, {:execute_ability, attack, [target]})
        mobile
        |> Mobile.set_attack_target(target)
        |> Mobile.initiate_combat
      else
        mobile
        |> Mobile.set_attack_target(target)
        |> Mobile.initiate_combat
      end
    end
  end
  def attack(%{spirit: _} = mobile), do: mobile

  def random_ability(abilities) do
    case abilities do
      [ability] ->
        ability
      [] ->
        nil
      abilities ->
        abilities |> Enum.shuffle |> List.first
    end
  end

  def move(%Monster{spirit: nil} = monster) do
    if !Monster.on_ai_move_cooldown?(monster) && !Monster.aggro_target(monster) do

      room = Monster.find_room(monster)
      roll = :random.uniform(100)

      if room && (roll < trunc(monster.chance_to_follow / 5)) do
        direction = Room.random_direction(room)

        if direction do
          ApathyDrive.Command.execute(monster, direction, [])
        end
      end
    end
  end
  def move(%Monster{}), do: nil

end
