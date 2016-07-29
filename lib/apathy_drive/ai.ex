defmodule ApathyDrive.AI do
  alias ApathyDrive.{Room, Mobile, Ability, TimerManager}

  def think(%Room{} = room, mobile_ref) do
    Room.update_mobile(room, mobile_ref, fn mobile ->
      mobile =
        room
        |> calm_down(mobile)
        |> TimerManager.send_after({:monster_ai, 5_000, {:think, mobile_ref}})

      if casting?(mobile) do
        mobile
      else
        heal(room, mobile) || bless(room, mobile) || curse(room, mobile) || attack(room, mobile) || mobile
      end
    end)
  end

  def calm_down(%Room{mobiles: mobiles}, %Mobile{hate: hate} = mobile) do
    mobiles_in_room =
      mobiles
      |> Map.keys
      |> Enum.into(HashSet.new)

    hate =
      hate
      |> Map.keys
      |> Enum.into(HashSet.new)
      |> HashSet.difference(mobiles_in_room)
      |> Enum.reduce(hate, fn(enemy, new_hate) ->
           current = Map.get(new_hate, enemy)
           if current > 5 do
             Map.put(new_hate, enemy, current - 5)
           else
             Map.delete(new_hate, enemy)
           end
         end)

    Map.put(mobile, :hate, hate)
  end

  def heal(%Room{} = room, %Mobile{hp: hp, max_hp: max_hp, spirit: nil} = mobile) do
    unless Ability.on_global_cooldown?(mobile) do
      chance = trunc((max_hp - hp) / max_hp * 100)

      roll = :rand.uniform(100)

      if chance > roll do
        ability =
          mobile
          |> Ability.heal_abilities
          |> random_ability(mobile)

        if ability do
          Ability.execute(room, mobile.ref, ability, [mobile.ref])
        end
      end
    end
  end
  def heal(%Room{}, %Mobile{}), do: nil

  def bless(%Room{} = room, %Mobile{spirit: nil} = mobile) do
    unless Ability.on_global_cooldown?(mobile) do
      ability = mobile
                |> Ability.bless_abilities
                |> random_ability(mobile)

      if ability do
        Ability.execute(room, mobile.ref, ability, [mobile.ref])
      end
    end
  end
  def bless(%Room{}, %Mobile{}), do: nil

  def curse(%Room{} = room, %Mobile{spirit: nil} = mobile) do
    if :rand.uniform(100) > 95 do
      if target = Mobile.aggro_target(room, mobile) do
        curse =
          if !Ability.on_global_cooldown?(mobile) do

            mobile
            |> Ability.curse_abilities
            |> random_ability(mobile)
          end

        if curse do
          Ability.execute(room, mobile.ref, curse, [target])
        end
      end
    end
  end
  def curse(%Room{}, %Mobile{}), do: nil

  def attack(%Room{} = room, %Mobile{spirit: nil} = mobile) do
    if target = Mobile.aggro_target(room, mobile) do
      attack = cond do
        !Ability.on_global_cooldown?(mobile) ->
           mobile
           |> Ability.attack_abilities
           |> random_ability(mobile)
        true ->
          nil
      end

      if attack do
        room = Ability.execute(room, mobile.ref, attack, [target])

        mobile =
          mobile
          |> Mobile.set_attack_target(target)
          |> Mobile.initiate_combat

        put_in(room.mobiles[mobile.ref], mobile)
      else
        mobile
        |> Mobile.set_attack_target(target)
        |> Mobile.initiate_combat
      end
    end
  end
  def attack(%Room{} = room, %{spirit: _} = mobile) do
    if target = Mobile.aggro_target(room, mobile) do

      mobile
      |> Mobile.set_attack_target(target)
      |> Mobile.initiate_combat
    end
  end

  def random_ability(abilities, mobile) do
    case abilities do
      [ability] ->
        unless Ability.on_cooldown?(mobile, ability) do
          ability
        end
      [] ->
        nil
      abilities ->
        abilities
        |> Enum.reject(&(Ability.on_cooldown?(mobile, &1)))
        |> Enum.random
    end
  end

  defp casting?(%Mobile{timers: timers}) do
    !!Map.get(timers, :cast_timer)
  end

end
