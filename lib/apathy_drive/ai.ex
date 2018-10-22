defmodule ApathyDrive.AI do
  alias ApathyDrive.{Ability, Mobile, Party, Room}

  def think(%Room{} = room, ref) do
    Room.update_mobile(room, ref, fn mobile ->
      if mobile.casting do
        mobile
      else
        # move(mobile, room) || mobile
        heal(mobile, room) || bless(mobile, room) || curse(mobile, room) || attack(mobile, room) ||
          auto_attack(mobile, room) || mobile
      end
    end)
  end

  def move(%{} = mobile, %Room{} = room) do
    if should_move?(mobile, room) do
      exits =
        case room.exits do
          nil ->
            []

          _exits ->
            exits_in_area(room)
        end

      if Enum.any?(exits) do
        new_exits =
          exits
          |> Enum.reject(&(&1["destination"] == mobile.last_room_id))

        room_exit =
          cond do
            is_nil(mobile.last_room_id) ->
              Enum.random(exits)

            Enum.any?(new_exits) ->
              Enum.random(new_exits)

            :else ->
              Enum.find(exits, &(&1["destination"] == mobile.last_room_id))
          end

        ApathyDrive.Commands.Move.execute(room, mobile, room_exit)
      end
    end
  end

  def heal(%{} = mobile, %Room{} = room) do
    injured_party_member =
      room
      |> Party.members(mobile)
      |> Enum.sort_by(& &1.hp)
      |> List.first()

    chance = trunc(:math.pow(20, 2 - injured_party_member.hp) - 20)

    roll = :rand.uniform(100)

    if chance > roll do
      ability =
        mobile
        |> Ability.heal_abilities()
        |> random_ability(mobile)

      if ability do
        Ability.execute(room, mobile.ref, ability, [injured_party_member.ref])
      end
    end
  end

  def bless(%{mana: mana} = mobile, %Room{} = room) when mana > 0.5 do
    member_to_bless =
      room
      |> Party.members(mobile)
      |> Enum.random()

    ability =
      mobile
      |> Ability.bless_abilities(member_to_bless)
      |> random_ability(mobile)

    if ability do
      Ability.execute(room, mobile.ref, ability, [member_to_bless.ref])
    end
  end

  def bless(%{} = _mobile, %Room{}), do: nil

  def curse(%{mana: mana} = mobile, %Room{} = room) when mana > 0.9 do
    target = room.mobiles[Mobile.auto_attack_target(mobile, room)]

    if target && :rand.uniform(100) > 95 do
      ability =
        mobile
        |> Ability.curse_abilities(target)
        |> random_ability(mobile)

      if ability do
        Ability.execute(room, mobile.ref, ability, [target.ref])
      end
    end
  end

  def curse(%{} = _mobile, %Room{}), do: nil

  def attack(%{mana: mana} = mobile, %Room{} = room) when mana > 0.75 do
    target = room.mobiles[Mobile.auto_attack_target(mobile, room)]

    if target do
      ability =
        mobile
        |> Ability.attack_abilities(target)
        |> random_ability(mobile)

      if ability do
        Ability.execute(room, mobile.ref, ability, [target.ref])
      end
    end
  end

  def attack(%{} = _mobile, %Room{}), do: nil

  def auto_attack(mobile, room) do
    attack = Mobile.attack_ability(mobile)

    if attack && mobile.energy >= attack.energy && !mobile.casting do
      if target_ref = Mobile.auto_attack_target(mobile, room) do
        Ability.execute(room, mobile.ref, attack, [target_ref])
      else
        case mobile do
          %{attack_target: target} = mobile when not is_nil(target) ->
            Mobile.send_scroll(
              mobile,
              "<p><span class='dark-yellow'>*Combat Off*</span></p>"
            )

            mobile =
              mobile
              |> Map.put(:attack_target, nil)

            room = put_in(room.mobiles[mobile.ref], mobile)

            Room.update_hp_bar(room, mobile.ref)
            Room.update_mana_bar(room, mobile.ref)

            room

          _mobile ->
            nil
        end
      end
    end
  end

  def random_ability([ability], mobile) do
    unless Ability.on_cooldown?(mobile, ability) do
      ability
    end
  end

  def random_ability([], _mobile), do: nil

  def random_ability(abilities, mobile) do
    abilities
    |> Enum.reject(&Ability.on_cooldown?(mobile, &1))
    |> Enum.random()
  end

  defp exits_in_area(%Room{exits: exits} = room) do
    Enum.filter(exits, fn %{"direction" => _direction} = room_exit ->
      room_exit["area"] == room.area.id && passable?(room, room_exit)
    end)
  end

  defp passable?(room, %{"kind" => kind} = room_exit) when kind in ["Door", "Gate"],
    do: ApathyDrive.Doors.open?(room, room_exit)

  defp passable?(_room, %{"kind" => kind}) when kind in ["Normal", "Action", "Trap", "Cast"],
    do: true

  defp passable?(_room, _room_exit), do: false

  defp should_move?(%ApathyDrive.Companion{}, _room), do: false
  defp should_move?(%ApathyDrive.Character{}, _room), do: false
  defp should_move?(%{movement: "stationary"}, _room), do: false

  defp should_move?(%{} = mobile, room) do
    mobile.hp > 0.8 and mobile.mana > 0.8 and is_nil(Mobile.auto_attack_target(mobile, room))
  end
end
