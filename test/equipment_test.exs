defmodule EquipmentTest do
  use ExUnit.Case
  use ShouldI
  import ApathyDrive.Matchers

  with "a monster with a sword, a staff, and a shield" do
    setup context do
      {:ok, monster} = GenServer.start_link(Monster, %Monster{id: 1})

      {:ok, sword }  = GenServer.start_link(Item, %Item{name: "sword",
                                                        worn_on: "Weapon Hand",
                                                        monster_id: 1})

      {:ok, staff }  = GenServer.start_link(Item, %Item{name: "staff",
                                                        worn_on: "Two Handed",
                                                        monster_id: 1})

      {:ok, shield } = GenServer.start_link(Item, %Item{name: "shield",
                                                        worn_on: "Off-Hand",
                                                        monster_id: 1})

      context
      |> Dict.put(:monster, Monster.value(monster))
      |> Dict.put(:sword,   Item.value(sword))
      |> Dict.put(:staff,   Item.value(staff))
      |> Dict.put(:shield,  Item.value(shield))
    end

    should("have a sword, a staff, and a shield", context) do
      assert_lists_match Monster.inventory(context.monster), [context.shield, context.staff, context.sword]
    end

    should("have nothing equipped", context) do
      assert_lists_match Monster.equipped_items(context.monster), []
    end

    should("equip a sword", context) do
      Monster.equip_item(context.monster, context.sword, nil)

      :timer.sleep(100)

      assert_lists_match Monster.inventory(context.monster), [context.shield, context.staff]
      assert_lists_match Monster.equipped_items(context.monster), [Item.value(context.sword.pid)]
    end

    should("equip a staff", context) do
      Monster.equip_item(context.monster, context.staff, nil)

      :timer.sleep(100)

      assert_lists_match Monster.inventory(context.monster), [context.sword, context.shield]
      assert_lists_match Monster.equipped_items(context.monster), [Item.value(context.staff.pid)]
    end

    should("equip a shield", context) do
      Monster.equip_item(context.monster, context.shield, nil)

      :timer.sleep(100)

      assert_lists_match Monster.inventory(context.monster), [context.sword, context.staff]
      assert_lists_match Monster.equipped_items(context.monster), [Item.value(context.shield.pid)]
    end
  end

  with "a monster wearing a sword and a shield, with a staff in its inventory" do
    setup context do
      {:ok, monster} = GenServer.start_link(Monster, %Monster{id: 1})

      {:ok, sword }  = GenServer.start_link(Item, %Item{name: "sword",
                                                        worn_on: "Weapon Hand",
                                                        equipped: true,
                                                        monster_id: 1})

      {:ok, staff }  = GenServer.start_link(Item, %Item{name: "staff",
                                                        worn_on: "Two Handed",
                                                        monster_id: 1})

      {:ok, shield } = GenServer.start_link(Item, %Item{name: "shield",
                                                        equipped: true,
                                                        worn_on: "Off-Hand",
                                                        monster_id: 1})

      context
      |> Dict.put(:monster, Monster.value(monster))
      |> Dict.put(:sword,   Item.value(sword))
      |> Dict.put(:staff,   Item.value(staff))
      |> Dict.put(:shield,  Item.value(shield))
    end

    should("have a staff in its inventory", context) do
      assert_lists_match Monster.inventory(context.monster), [context.staff]
    end

    should("have a sword and shield equipped", context) do
      assert_lists_match Monster.equipped_items(context.monster), [context.sword, context.shield]
    end

    should("remove the sword and shield when equipping a staff", context) do
      Monster.equip_item(context.monster, context.staff, nil)

      :timer.sleep(100)

      assert_lists_match Monster.inventory(context.monster), [Item.value(context.sword.pid), Item.value(context.shield.pid)]
      assert_lists_match Monster.equipped_items(context.monster), [Item.value(context.staff.pid)]
    end
  end

  with "a monster wearing a staff, with a sword and shield in its inventory" do
    setup context do
      {:ok, monster} = GenServer.start_link(Monster, %Monster{id: 1})

      {:ok, sword }  = GenServer.start_link(Item, %Item{name: "sword",
                                                        worn_on: "Weapon Hand",
                                                        monster_id: 1})

      {:ok, staff }  = GenServer.start_link(Item, %Item{name: "staff",
                                                        worn_on: "Two Handed",
                                                        equipped: true,
                                                        monster_id: 1})

      {:ok, shield } = GenServer.start_link(Item, %Item{name: "shield",
                                                        worn_on: "Off-Hand",
                                                        monster_id: 1})

      context
      |> Dict.put(:monster, Monster.value(monster))
      |> Dict.put(:sword,   Item.value(sword))
      |> Dict.put(:staff,   Item.value(staff))
      |> Dict.put(:shield,  Item.value(shield))
    end

    should("have a sword and shield in its inventory", context) do
      assert_lists_match Monster.inventory(context.monster), [context.shield, context.sword]
    end

    should("have a staff equipped", context) do
      assert_lists_match Monster.equipped_items(context.monster), [context.staff]
    end

    should("remove the staff when equipping a sword", context) do
      Monster.equip_item(context.monster, context.sword, nil)

      :timer.sleep(100)

      assert_lists_match Monster.inventory(context.monster), [Item.value(context.staff.pid), Item.value(context.shield.pid)]
      assert_lists_match Monster.equipped_items(context.monster), [Item.value(context.sword.pid)]
    end

    should("remove the staff when equipping a shield", context) do
      Monster.equip_item(context.monster, context.shield, nil)

      :timer.sleep(100)

      assert_lists_match Monster.inventory(context.monster), [Item.value(context.sword.pid), Item.value(context.staff.pid)]
      assert_lists_match Monster.equipped_items(context.monster), [Item.value(context.shield.pid)]
    end
  end
end
