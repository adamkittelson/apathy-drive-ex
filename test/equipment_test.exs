defmodule EquipmentTest do
  use ExUnit.Case
  use ShouldI
  import ApathyDrive.Matchers

  with "a monster with a sword, a staff, and a shield" do
    setup context do
      monster = %Monster{} |> ApathyDrive.Repo.insert

      sword = %Item{name: "sword",
                    worn_on: "Weapon Hand",
                    monster_id: monster.id} |> ApathyDrive.Repo.insert

      staff = %Item{name: "staff",
                    worn_on: "Two Handed",
                    monster_id: monster.id} |> ApathyDrive.Repo.insert

      shield = %Item{name: "shield",
                     worn_on: "Off-Hand",
                     monster_id: monster.id} |> ApathyDrive.Repo.insert

      monster = put_in(monster.inventory, [sword, staff, shield])

      context
      |> Dict.put(:monster, monster)
      |> Dict.put(:sword,   sword)
      |> Dict.put(:staff,   staff)
      |> Dict.put(:shield,  shield)
    end

    should("have a sword, a staff, and a shield", context) do
      assert_lists_match context.monster.inventory, [context.shield, context.staff, context.sword]
    end

    should("have nothing equipped", context) do
      assert_lists_match context.monster.equipment |> Map.values, []
    end

    should("equip a sword", context) do
      monster = Monster.equip_item(context.monster, context.sword, nil)

      assert_lists_match monster.inventory, [context.shield, context.staff]
      assert_lists_match monster.equipment |> Map.values, [context.sword]
    end

    should("equip a staff", context) do
      monster = Monster.equip_item(context.monster, context.staff, nil)

      assert_lists_match monster.inventory, [context.sword, context.shield]
      assert_lists_match monster.equipment |> Map.values, [context.staff]
    end

    should("equip a shield", context) do
      monster = Monster.equip_item(context.monster, context.shield, nil)

      assert_lists_match monster.inventory, [context.sword, context.staff]
      assert_lists_match monster.equipment |> Map.values, [context.shield]
    end
  end

  with "a monster wearing a sword and a shield, with a staff in its inventory" do
    setup context do
      monster = %Monster{} |> ApathyDrive.Repo.insert

      sword = %Item{name: "sword",
                    worn_on: "Weapon Hand",
                    equipped: true,
                    monster_id: monster.id} |> ApathyDrive.Repo.insert

      staff = %Item{name: "staff",
                    worn_on: "Two Handed",
                    monster_id: monster.id} |> ApathyDrive.Repo.insert

      shield = %Item{name: "shield",
                     equipped: true,
                     worn_on: "Off-Hand",
                     monster_id: monster.id} |> ApathyDrive.Repo.insert

      monster = put_in(monster.inventory, [staff])
      monster = put_in(monster.equipment, %{"Weapon Hand" => sword, "Off-Hand" => shield})

      context
      |> Dict.put(:monster, monster)
      |> Dict.put(:sword,   sword)
      |> Dict.put(:staff,   staff)
      |> Dict.put(:shield,  shield)
    end

    should("have a staff in its inventory", context) do
      assert_lists_match context.monster.inventory, [context.staff]
    end

    should("have a sword and shield equipped", context) do
      assert_lists_match context.monster.equipment |> Map.values, [context.sword, context.shield]
    end

    should("remove the sword and shield when equipping a staff", context) do
      monster = Monster.equip_item(context.monster, context.staff, nil)

      assert_lists_match monster.inventory, [context.sword, context.shield]
      assert_lists_match monster.equipment |> Map.values, [context.staff]
    end
  end

  with "a monster wearing a staff, with a sword and shield in its inventory" do
    setup context do
      monster = %Monster{} |> ApathyDrive.Repo.insert

      sword = %Item{name: "sword",
                    worn_on: "Weapon Hand",
                    monster_id: monster.id} |> ApathyDrive.Repo.insert

      staff = %Item{name: "staff",
                    worn_on: "Two Handed",
                    equipped: true,
                    monster_id: monster.id} |> ApathyDrive.Repo.insert

      shield = %Item{name: "shield",
                     worn_on: "Off-Hand",
                     monster_id: monster.id} |> ApathyDrive.Repo.insert

      monster = put_in(monster.inventory, [sword, shield])
      monster = put_in(monster.equipment, %{"Two Handed" => staff})

      context
      |> Dict.put(:monster, monster)
      |> Dict.put(:sword,   sword)
      |> Dict.put(:staff,   staff)
      |> Dict.put(:shield,  shield)
    end

    should("have a sword and shield in its inventory", context) do
      assert_lists_match context.monster.inventory, [context.shield, context.sword]
    end

    should("have a staff equipped", context) do
      assert_lists_match context.monster.equipment |> Map.values, [context.staff]
    end

    should("remove the staff when equipping a sword", context) do
      monster = Monster.equip_item(context.monster, context.sword, nil)

      assert_lists_match monster.inventory, [context.staff, context.shield]
      assert_lists_match monster.equipment |> Map.values, [context.sword]
    end

    should("remove the staff when equipping a shield", context) do
      monster = Monster.equip_item(context.monster, context.shield, nil)

      assert_lists_match monster.inventory, [context.sword, context.staff]
      assert_lists_match monster.equipment |> Map.values, [context.shield]
    end
  end
end
