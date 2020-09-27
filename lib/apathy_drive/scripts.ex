defmodule ApathyDrive.Scripts do
  alias ApathyDrive.{
    Character,
    Currency,
    Item,
    ItemInstance,
    Mobile,
    Monster,
    Repo,
    Room,
    RoomMonster
  }

  require Ecto.Query

  def check_item(mobile, item_id, failure_message) do
    if Enum.find(mobile.inventory, &(&1.id == item_id)) do
      true
    else
      Mobile.send_scroll(mobile, "<p>#{failure_message}</p>")
      false
    end
  end

  def give_coins_up_to(%Room{} = room, mobile_ref, %{} = coins) do
    coins = Map.merge(%{copper: 0, silver: 0, gold: 0, platinum: 0, runic: 0}, coins)

    coins =
      coins
      |> Enum.reduce(coins, fn {denomination, amount}, coins ->
        Map.put(coins, denomination, Enum.random(0..amount))
      end)
      |> Map.take([:runic, :platinum, :gold, :silver, :copper])

    value = Currency.wealth(coins)

    # smallest number of coins
    coins = Currency.set_value(coins, value)

    Room.update_mobile(room, mobile_ref, fn _room, mobile ->
      Mobile.send_scroll(mobile, "<p>You receive #{Currency.to_string(coins)}.</p>")

      char_currency = Currency.add(mobile, value)

      mobile
      |> Ecto.Changeset.change(%{
        runic: char_currency.runic,
        platinum: char_currency.platinum,
        gold: char_currency.gold,
        silver: char_currency.silver,
        copper: char_currency.copper
      })
      |> Repo.update!()
    end)
  end

  def give_item(%Room{} = room, mobile_ref, item_id) when is_integer(item_id) do
    item = Repo.get!(Item, item_id)

    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      level = if item.type in ["Armour", "Shield,", "Weapon"], do: mobile.level, else: nil

      if item.weight <= Character.max_encumbrance(mobile) - Character.encumbrance(mobile) do
        instance =
          %ItemInstance{
            item_id: item_id,
            room_id: nil,
            character_id: mobile.id,
            equipped: false,
            hidden: false,
            level: level
          }
          |> Repo.insert!()

        mobile = Character.load_items(mobile)
        item = Enum.find(mobile.inventory, &(&1.instance_id == instance.id))

        Mobile.send_scroll(
          mobile,
          "<p>You receive a #{Item.colored_name(item, character: mobile)}.</p>"
        )

        Room.send_scroll(
          room,
          "<p>#{mobile.name} receives a #{Item.colored_name(item, character: mobile)}.</p>",
          [mobile]
        )

        mobile
      else
        instance =
          %ItemInstance{
            item_id: item_id,
            room_id: room.id,
            character_id: nil,
            equipped: false,
            hidden: false,
            level: level
          }
          |> Repo.insert!()

        room = Room.load_items(room)

        item = Enum.find(room.items, &(&1.instance_id == instance.id))

        Mobile.send_scroll(
          mobile,
          "<p>A #{Item.colored_name(item, character: mobile)} drops to the floor.</p>"
        )

        Room.send_scroll(
          room,
          "<p>A #{Item.colored_name(item, character: mobile)} drops to the floor.</p>",
          [mobile]
        )

        room
      end
    end)
  end

  def give_item(%Room{} = room, mobile_ref, name) do
    Item
    |> Ecto.Query.where(name: ^name)
    |> Repo.one()
    |> case do
      %Item{id: item_id} ->
        give_item(room, mobile_ref, item_id)

      nil ->
        raise "Item for Scripts.give_item/3 not found: #{name}"
    end
  end

  def take_item(%Room{} = room, mobile_ref, item_id) when is_integer(item_id) do
    Room.update_mobile(room, mobile_ref, fn _room, mobile ->
      item = Enum.find(mobile.inventory, &(&1.id == item_id))

      ItemInstance
      |> Repo.get!(item.instance_id)
      |> Repo.delete!()

      Character.load_items(mobile)
    end)
  end

  def take_item(%Room{} = room, mobile_ref, name) do
    Item
    |> Ecto.Query.where(name: ^name)
    |> Repo.one()
    |> case do
      %Item{id: item_id} ->
        take_item(room, mobile_ref, item_id)

      nil ->
        raise "Item for Scripts.take_item/3 not found: #{name}"
    end
  end

  def summon(room, monster, force \\ false)

  def summon(%Room{} = room, monster_id, force) when is_integer(monster_id) do
    Repo.get!(Monster, monster_id)

    monster =
      %RoomMonster{
        room_id: room.id,
        monster_id: monster_id,
        level: room.area.level,
        spawned_at: room.id,
        zone_spawned_at: room.zone_controller_id
      }
      |> Monster.from_room_monster(force)

    Room.mobile_entered(room, monster, "")
  end

  def summon(%Room{} = room, name, force) do
    Monster
    |> Ecto.Query.where(name: ^name)
    |> Repo.one()
    |> case do
      %Monster{id: monster_id} ->
        summon(room, monster_id, force)

      nil ->
        raise "Monster for Scripts.summon/2 not found: #{name}"
    end
  end

  def random_item_871(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 8 ->
        give_item(room, mobile_ref, "azurite stone")

      roll <= 16 ->
        give_item(room, mobile_ref, "agate stone")

      roll <= 24 ->
        give_item(room, mobile_ref, "hematite stone")

      roll <= 32 ->
        give_item(room, mobile_ref, "malachite stone")

      roll <= 41 ->
        give_item(room, mobile_ref, "obsidian stone")

      roll <= 50 ->
        give_item(room, mobile_ref, "turquoise stone")

      roll <= 55 ->
        give_item(room, mobile_ref, "bloodstone")

      roll <= 60 ->
        give_item(room, mobile_ref, "carnelian stone")

      roll <= 65 ->
        # moonstone
        give_item(room, mobile_ref, 889)

      roll <= 70 ->
        give_item(room, mobile_ref, "onyx stone")

      roll <= 75 ->
        give_item(room, mobile_ref, "piece of crystal")

      roll <= 82 ->
        give_item(room, mobile_ref, "piece of amber")

      roll <= 89 ->
        give_item(room, mobile_ref, "amethyst stone")

      roll <= 95 ->
        give_item(room, mobile_ref, "piece of jade")

      roll <= 96 ->
        give_item(room, mobile_ref, "aquamarine stone")

      roll <= 97 ->
        give_item(room, mobile_ref, "garnet")

      roll <= 99 ->
        # pearl
        give_item(room, mobile_ref, 897)

      roll <= 100 ->
        give_item(room, mobile_ref, "topaz stone")
    end
  end

  def random_item_874(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 9 ->
        give_item(room, mobile_ref, "minor healing potion")

      roll <= 14 ->
        give_item(room, mobile_ref, "cure poison potion")

      roll <= 15 ->
        give_item(room, mobile_ref, "blue potion")

      roll <= 16 ->
        give_item(room, mobile_ref, "yellow potion")

      roll <= 17 ->
        give_item(room, mobile_ref, "white potion")

      roll <= 18 ->
        give_item(room, mobile_ref, "silvery potion")

      roll <= 19 ->
        give_item(room, mobile_ref, "turquoise potion")

      roll <= 16 ->
        give_item(room, mobile_ref, "starsteel shortsword")

      roll <= 17 ->
        give_item(room, mobile_ref, "starsteel corselet")

      roll <= 18 ->
        give_item(room, mobile_ref, "turquoise potion")

      roll <= 20 ->
        give_item(room, mobile_ref, "shimmering vial")

      roll <= 22 ->
        give_item(room, mobile_ref, "amethyst pendant")

      roll <= 23 ->
        give_item(room, mobile_ref, "sapphire ring")

      roll <= 24 ->
        give_item(room, mobile_ref, "golden snake earrings")

      roll <= 25 ->
        give_item(room, mobile_ref, "jade amulet")

      roll <= 26 ->
        give_item(room, mobile_ref, "ethereal amulet")

      roll <= 27 ->
        give_item(room, mobile_ref, "serpent armband")

      roll <= 29 ->
        give_item(room, mobile_ref, "padded black boot")

      roll <= 31 ->
        give_item(room, mobile_ref, "runed cowl")

      roll <= 32 ->
        give_item(room, mobile_ref, "eagle helm")

      roll <= 33 ->
        give_item(room, mobile_ref, "shadow cloak")

      roll <= 34 ->
        give_item(room, mobile_ref, "elven cloak")

      roll <= 35 ->
        give_item(room, mobile_ref, "elven boot")

      roll <= 40 ->
        give_item(room, mobile_ref, "gilded robes")

      roll <= 45 ->
        give_item(room, mobile_ref, "gianthair vest")

      roll <= 50 ->
        give_item(room, mobile_ref, "fine chainmail hauberk")

      roll <= 55 ->
        give_item(room, mobile_ref, "silvery plate corselet")

      roll <= 57 ->
        give_item(room, mobile_ref, "runed robes")

      roll <= 59 ->
        give_item(room, mobile_ref, "ogre-skin shirt")

      roll <= 61 ->
        give_item(room, mobile_ref, "runed chainmail hauberk")

      roll <= 63 ->
        give_item(room, mobile_ref, "fine breastplate")

      roll <= 65 ->
        give_item(room, mobile_ref, "silvery shield")

      roll <= 70 ->
        give_item(room, mobile_ref, "fine broadsword")

      roll <= 75 ->
        give_item(room, mobile_ref, "fine mace")

      roll <= 77 ->
        give_item(room, mobile_ref, "glowing broadsword")

      roll <= 79 ->
        give_item(room, mobile_ref, "glowing warhammer")

      roll <= 80 ->
        give_item(room, mobile_ref, "dwarven axe")

      roll <= 81 ->
        give_item(room, mobile_ref, "dwarven hammer")

      roll <= 86 ->
        give_item(room, mobile_ref, "fine greatsword")

      roll <= 91 ->
        give_item(room, mobile_ref, "oaken staff")

      roll <= 93 ->
        give_item(room, mobile_ref, "black steel halberd")

      roll <= 95 ->
        give_item(room, mobile_ref, "great bronzewood club")

      roll <= 97 ->
        give_item(room, mobile_ref, "ashwood staff")

      roll <= 98 ->
        give_item(room, mobile_ref, "bladed staff")

      roll <= 99 ->
        give_item(room, mobile_ref, "three-headed flail")

      roll <= 100 ->
        give_item(room, mobile_ref, "long golden staff")
    end
  end

  def random_item_886(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 4 ->
        give_item(room, mobile_ref, "azurite stone")

      roll <= 8 ->
        give_item(room, mobile_ref, "agate stone")

      roll <= 12 ->
        give_item(room, mobile_ref, "hematite stone")

      roll <= 17 ->
        give_item(room, mobile_ref, "malachite stone")

      roll <= 21 ->
        give_item(room, mobile_ref, "obsidian stone")

      roll <= 26 ->
        give_item(room, mobile_ref, "turquoise stone")

      roll <= 31 ->
        give_item(room, mobile_ref, "bloodstone")

      roll <= 36 ->
        give_item(room, mobile_ref, "carnelian stone")

      roll <= 41 ->
        # moonstone
        give_item(room, mobile_ref, 889)

      roll <= 46 ->
        give_item(room, mobile_ref, "onyx stone")

      roll <= 51 ->
        give_item(room, mobile_ref, "piece of crystal")

      roll <= 59 ->
        give_item(room, mobile_ref, "piece of amber")

      roll <= 67 ->
        give_item(room, mobile_ref, "amethyst stone")

      roll <= 75 ->
        give_item(room, mobile_ref, "piece of jade")

      roll <= 78 ->
        give_item(room, mobile_ref, "aquamarine stone")

      roll <= 81 ->
        give_item(room, mobile_ref, "garnet")

      roll <= 84 ->
        # pearl
        give_item(room, mobile_ref, 897)

      roll <= 87 ->
        give_item(room, mobile_ref, "topaz stone")

      roll <= 90 ->
        give_item(room, mobile_ref, "black opal")

      roll <= 93 ->
        give_item(room, mobile_ref, "fire opal")

      roll <= 96 ->
        give_item(room, mobile_ref, "opal")

      roll <= 97 ->
        # diamond
        give_item(room, mobile_ref, 902)

      roll <= 98 ->
        give_item(room, mobile_ref, "emerald")

      roll <= 99 ->
        give_item(room, mobile_ref, "ruby")

      roll <= 100 ->
        give_item(room, mobile_ref, "sapphire")
    end
  end

  def random_item_889(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 10 ->
        give_item(room, mobile_ref, "turquoise potion")

      roll <= 15 ->
        give_item(room, mobile_ref, "shimmering vial")

      roll <= 18 ->
        give_item(room, mobile_ref, "green potion")

      roll <= 23 ->
        give_item(room, mobile_ref, "orange potion")

      roll <= 25 ->
        give_item(room, mobile_ref, "jade amulet")

      roll <= 27 ->
        give_item(room, mobile_ref, "ethereal amulet")

      roll <= 29 ->
        give_item(room, mobile_ref, "serpent armband")

      roll <= 30 ->
        give_item(room, mobile_ref, "cat's-eye pendant")

      roll <= 31 ->
        give_item(room, mobile_ref, "demonhide bracer")

      roll <= 32 ->
        give_item(room, mobile_ref, "crimson bracer")

      roll <= 33 ->
        give_item(room, mobile_ref, "mithril ring")

      roll <= 34 ->
        give_item(room, mobile_ref, "moonstone ring")

      roll <= 37 ->
        give_item(room, mobile_ref, "belt of might")

      roll <= 40 ->
        give_item(room, mobile_ref, "eagle helm")

      roll <= 43 ->
        give_item(room, mobile_ref, "shadow cloak")

      roll <= 46 ->
        give_item(room, mobile_ref, "elven cloak")

      roll <= 48 ->
        give_item(room, mobile_ref, "elven boot")

      roll <= 50 ->
        give_item(room, mobile_ref, "crested war helm")

      roll <= 52 ->
        give_item(room, mobile_ref, "gauntlet of power")

      roll <= 54 ->
        give_item(room, mobile_ref, "high-necked robes")

      roll <= 56 ->
        give_item(room, mobile_ref, "wyvernhide tunic")

      roll <= 58 ->
        give_item(room, mobile_ref, "dwarven chainmail hauberk")

      roll <= 60 ->
        give_item(room, mobile_ref, "crystal platemail")

      roll <= 61 ->
        give_item(room, mobile_ref, "long battle shield")

      roll <= 62 ->
        give_item(room, mobile_ref, "prismatic robes")

      roll <= 63 ->
        give_item(room, mobile_ref, "lacquered battle armour")

      roll <= 64 ->
        give_item(room, mobile_ref, "adamantite chainmail hauberk")

      roll <= 65 ->
        give_item(room, mobile_ref, "adamantite platemail tunic")

      roll <= 69 ->
        give_item(room, mobile_ref, "skull shield")

      roll <= 73 ->
        give_item(room, mobile_ref, "dwarven axe")

      roll <= 75 ->
        give_item(room, mobile_ref, "dwarven hammer")

      roll <= 77 ->
        give_item(room, mobile_ref, "soulsword")

      roll <= 78 ->
        give_item(room, mobile_ref, "runic warhammer")

      roll <= 79 ->
        give_item(room, mobile_ref, "sunsword")

      roll <= 83 ->
        give_item(room, mobile_ref, "starsteel mace")

      roll <= 87 ->
        give_item(room, mobile_ref, "bladed staff")

      roll <= 91 ->
        give_item(room, mobile_ref, "three-headed flail")

      roll <= 93 ->
        give_item(room, mobile_ref, "long golden staff")

      roll <= 95 ->
        give_item(room, mobile_ref, "elven war-spear")

      roll <= 97 ->
        give_item(room, mobile_ref, "dwarven-craft hammer")

      roll <= 98 ->
        give_item(room, mobile_ref, "staff of the elements")

      roll <= 99 ->
        give_item(room, mobile_ref, "dragontooth trident")

      roll <= 100 ->
        give_item(room, mobile_ref, "serpent staff")
    end
  end

  def random_item_895(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 5 ->
        give_item(room, mobile_ref, "bloodstone")

      roll <= 10 ->
        give_item(room, mobile_ref, "carnelian stone")

      roll <= 15 ->
        # moonstone
        give_item(room, mobile_ref, 889)

      roll <= 20 ->
        give_item(room, mobile_ref, "onyx stone")

      roll <= 25 ->
        give_item(room, mobile_ref, "piece of crystal")

      roll <= 34 ->
        give_item(room, mobile_ref, "piece of amber")

      roll <= 43 ->
        give_item(room, mobile_ref, "amethyst stone")

      roll <= 52 ->
        give_item(room, mobile_ref, "piece of jade")

      roll <= 58 ->
        give_item(room, mobile_ref, "aquamarine stone")

      roll <= 64 ->
        give_item(room, mobile_ref, "garnet")

      roll <= 70 ->
        # pearl
        give_item(room, mobile_ref, 897)

      roll <= 76 ->
        give_item(room, mobile_ref, "topaz stone")

      roll <= 81 ->
        give_item(room, mobile_ref, "black opal")

      roll <= 86 ->
        give_item(room, mobile_ref, "fire opal")

      roll <= 92 ->
        give_item(room, mobile_ref, "opal")

      roll <= 94 ->
        # diamond
        give_item(room, mobile_ref, 902)

      roll <= 96 ->
        give_item(room, mobile_ref, "emerald")

      roll <= 98 ->
        give_item(room, mobile_ref, "ruby")

      roll <= 100 ->
        give_item(room, mobile_ref, "sapphire")
    end
  end

  def random_item_898(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 3 ->
        give_item(room, mobile_ref, "black potion")

      roll <= 5 ->
        give_item(room, mobile_ref, "golden vial")

      roll <= 6 ->
        give_item(room, mobile_ref, "starsteel plate boot")

      roll <= 8 ->
        give_item(room, mobile_ref, "starsteel plate gauntlet")

      roll <= 10 ->
        give_item(room, mobile_ref, "starsteel greathelm")

      roll <= 12 ->
        give_item(room, mobile_ref, "starsteel plate leggings")

      roll <= 14 ->
        give_item(room, mobile_ref, "starsteel bracer")

      roll <= 16 ->
        give_item(room, mobile_ref, "starsteel shortsword")

      roll <= 17 ->
        give_item(room, mobile_ref, "starsteel corselet")

      roll <= 18 ->
        give_item(room, mobile_ref, "turquoise potion")

      roll <= 22 ->
        give_item(room, mobile_ref, "crimson bracer")

      roll <= 26 ->
        give_item(room, mobile_ref, "mithril ring")

      roll <= 30 ->
        give_item(room, mobile_ref, "moonstone ring")

      roll <= 34 ->
        give_item(room, mobile_ref, "belt of might")

      roll <= 38 ->
        give_item(room, mobile_ref, "gauntlet of power")

      roll <= 43 ->
        give_item(room, mobile_ref, "prismatic robes")

      roll <= 48 ->
        give_item(room, mobile_ref, "lacquered battle armour")

      roll <= 53 ->
        give_item(room, mobile_ref, "adamantite chainmail hauberk")

      roll <= 58 ->
        give_item(room, mobile_ref, "adamantite platemail tunic")

      roll <= 63 ->
        give_item(room, mobile_ref, "skull shield")

      roll <= 65 ->
        give_item(room, mobile_ref, "astral robes")

      roll <= 67 ->
        give_item(room, mobile_ref, "behemoth hide")

      roll <= 68 ->
        give_item(room, mobile_ref, "prismatic slipper")

      roll <= 69 ->
        give_item(room, mobile_ref, "mithril chainmail hauberk")

      roll <= 70 ->
        give_item(room, mobile_ref, "prismatic glove")

      roll <= 71 ->
        give_item(room, mobile_ref, "dragonscale armour")

      roll <= 72 ->
        give_item(room, mobile_ref, "prismatic trousers")

      roll <= 73 ->
        give_item(room, mobile_ref, "aegis shield")

      roll <= 74 ->
        give_item(room, mobile_ref, "silversilk tunic")

      roll <= 79 ->
        give_item(room, mobile_ref, "sunsword")

      roll <= 84 ->
        give_item(room, mobile_ref, "starsteel mace")

      roll <= 86 ->
        give_item(room, mobile_ref, "vorpal sword")

      roll <= 88 ->
        give_item(room, mobile_ref, "stormhammer")

      roll <= 89 ->
        give_item(room, mobile_ref, "astral slipper")

      roll <= 91 ->
        give_item(room, mobile_ref, "astral trousers")

      roll <= 93 ->
        give_item(room, mobile_ref, "astral glove")

      roll <= 95 ->
        give_item(room, mobile_ref, "dragontooth trident")

      roll <= 98 ->
        give_item(room, mobile_ref, "serpent staff")

      roll <= 100 ->
        give_item(room, mobile_ref, "wicked bone scythe")
    end
  end

  def random_item_2944(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 3 ->
        give_item(room, mobile_ref, "sapphire")

      roll <= 6 ->
        # diamond
        give_item(room, mobile_ref, 902)

      roll <= 9 ->
        give_item(room, mobile_ref, "onyx stone")

      roll <= 14 ->
        # moonstone
        give_item(room, mobile_ref, 889)

      roll <= 29 ->
        give_item(room, mobile_ref, "glowing broadsword")

      roll <= 32 ->
        room = give_item(room, mobile_ref, "ice crystal falchion")
        character = room.mobiles[mobile_ref]

        # if ice crystal falchion could not be spawned because it is a limited item
        # then give a longsword instead
        if !Enum.find(character.inventory ++ room.items, &(&1.name == "ice crystal falchion")) do
          give_item(room, mobile_ref, "longsword")
        else
          room
        end

      roll <= 33 ->
        give_item(room, mobile_ref, "azurite stone")

      roll <= 35 ->
        give_item(room, mobile_ref, "serpent armband")

      roll <= 36 ->
        give_item(room, mobile_ref, "crimson scale gauntlet")

      roll <= 37 ->
        give_item(room, mobile_ref, "vorpal sword")

      roll <= 38 ->
        give_item(room, mobile_ref, "emerald-studded bracelet")

      roll <= 45 ->
        give_item(room, mobile_ref, "gilded robes")

      roll <= 50 ->
        give_item(room, mobile_ref, "gianthair vest")

      roll <= 52 ->
        give_item(room, mobile_ref, "red dragon boot")

      roll <= 54 ->
        give_item(room, mobile_ref, "silver bracelet")

      roll <= 60 ->
        give_item(room, mobile_ref, "fingerbone bracelet")

      roll <= 69 ->
        give_item(room, mobile_ref, "shimmering vial")

      roll <= 71 ->
        give_item(room, mobile_ref, "agate stone")

      roll <= 74 ->
        give_item(room, mobile_ref, "hematite stone")

      roll <= 79 ->
        give_item(room, mobile_ref, "obsidian stone")

      roll <= 80 ->
        give_item(room, mobile_ref, "emerald")

      roll <= 87 ->
        # "icy scroll"
        give_item(room, mobile_ref, 1889)

      roll <= 94 ->
        # another "icy scroll"?
        give_item(room, mobile_ref, 1342)

      roll <= 100 ->
        random_item_2944(room, mobile_ref)
    end
  end

  def random_item_2921(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 25 ->
        room
        |> random_item_2922(mobile_ref)
        |> random_item_2922(mobile_ref)
        |> random_item_2922(mobile_ref)

      roll <= 50 ->
        room
        |> random_item_2922(mobile_ref)
        |> random_item_2922(mobile_ref)
        |> random_item_2922(mobile_ref)
        |> random_item_2922(mobile_ref)

      roll <= 75 ->
        room
        |> random_item_2922(mobile_ref)
        |> random_item_2922(mobile_ref)
        |> random_item_2922(mobile_ref)
        |> random_item_2922(mobile_ref)
        |> random_item_2922(mobile_ref)

      roll <= 100 ->
        room
        |> random_item_2922(mobile_ref)
        |> random_item_2922(mobile_ref)
        |> random_item_2922(mobile_ref)
        |> random_item_895(mobile_ref)
        |> random_item_895(mobile_ref)
        |> random_item_2922(mobile_ref)
        |> random_item_2922(mobile_ref)
    end
  end

  def random_item_2922(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 9 ->
        give_item(room, mobile_ref, "carnelian stone")

      roll <= 14 ->
        give_item(room, mobile_ref, 889)

      roll <= 25 ->
        give_item(room, mobile_ref, "glowing broadsword")

      roll <= 30 ->
        give_item(room, mobile_ref, "fine silvery broadsword")

      roll <= 33 ->
        give_item(room, mobile_ref, "azurite stone")

      roll <= 35 ->
        give_item(room, mobile_ref, "serpent armband")

      roll <= 36 ->
        give_item(room, mobile_ref, "fine platinum chain")

      roll <= 37 ->
        give_item(room, mobile_ref, "glowing warhammer")

      roll <= 38 ->
        give_item(room, mobile_ref, "emerald-studded bracelet")

      roll <= 45 ->
        give_item(room, mobile_ref, "gilded robes")

      roll <= 50 ->
        give_item(room, mobile_ref, "gianthair vest")

      roll <= 52 ->
        give_item(room, mobile_ref, "crimson cloak")

      roll <= 57 ->
        give_item(room, mobile_ref, "silver bracelet")

      roll <= 60 ->
        give_item(room, mobile_ref, "fingerbone bracelet")

      roll <= 69 ->
        give_item(room, mobile_ref, "azurite stone")

      roll <= 71 ->
        give_item(room, mobile_ref, "agate stone")

      roll <= 74 ->
        give_item(room, mobile_ref, "hematite stone")

      roll <= 77 ->
        give_item(room, mobile_ref, "malachite stone")

      roll <= 85 ->
        give_item(room, mobile_ref, "obsidian stone")

      roll <= 90 ->
        give_item(room, mobile_ref, "turquoise stone")

      roll <= 100 ->
        give_item(room, mobile_ref, "bloodstone")
    end
  end

  def random_item_4102(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 4 ->
        # moonstone
        give_item(room, mobile_ref, 889)

      roll <= 9 ->
        # glowing broadsword
        give_item(room, mobile_ref, 361)

      roll <= 12 ->
        # starsteel greatsword
        room = give_item(room, mobile_ref, 450)
        character = room.mobiles[mobile_ref]

        # if starsteel greatsword could not be spawned because it is a limited item
        # then give a red dragon boots instead
        if !Enum.find(character.inventory ++ room.items, &(&1.id == 450)) do
          # red dragon boots
          give_item(room, mobile_ref, 1434)
        else
          room
        end

      roll <= 13 ->
        # emerald
        give_item(room, mobile_ref, 903)

      roll <= 15 ->
        # serpent armbands
        give_item(room, mobile_ref, 202)

      roll <= 16 ->
        # moonstone
        give_item(room, mobile_ref, 436)

      roll <= 17 ->
        # glowing warhammer
        give_item(room, mobile_ref, 116)

      roll <= 18 ->
        # adamantite bracer
        give_item(room, mobile_ref, 423)

      roll <= 19 ->
        # scroll of dragonfire
        give_item(room, mobile_ref, 549)

      roll <= 25 ->
        # adamantite chainmail
        give_item(room, mobile_ref, 419)

      roll <= 30 ->
        # red enamled scalemail
        give_item(room, mobile_ref, 415)

      roll <= 32 ->
        # ebony greataxe
        give_item(room, mobile_ref, 226)

      roll <= 34 ->
        # silver bracelet
        give_item(room, mobile_ref, 426)

      roll <= 40 ->
        # fingerbone bracelet
        give_item(room, mobile_ref, 428)

      roll <= 49 ->
        # crimson cloak
        give_item(room, mobile_ref, 421)

      roll <= 51 ->
        # agate stone
        give_item(room, mobile_ref, 882)

      roll <= 54 ->
        # hematite stone
        give_item(room, mobile_ref, 883)

      roll <= 59 ->
        # obsidian stone
        give_item(room, mobile_ref, 885)

      roll <= 65 ->
        # emerald
        give_item(room, mobile_ref, 903)

      roll <= 70 ->
        # diamond
        give_item(room, mobile_ref, 902)

      roll <= 72 ->
        # tattered white bible
        give_item(room, mobile_ref, 1869)

      roll <= 75 ->
        # starsteel shortsword
        give_item(room, mobile_ref, 1904)

      roll <= 78 ->
        # blood-stained manual
        give_item(room, mobile_ref, 1876)

      roll <= 80 ->
        # starsteel scimitar
        give_item(room, mobile_ref, 272)

      roll <= 85 ->
        random_item_889(room, mobile_ref)

      roll <= 95 ->
        random_item_898(room, mobile_ref)

      roll <= 100 ->
        room
        |> random_item_898(mobile_ref)
        |> random_item_898(mobile_ref)
    end
  end

  def random_item_4104(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 25 ->
        # diamond-studded ring
        give_item(room, mobile_ref, 829)

      roll <= 50 ->
        # mithril-runed tunic
        give_item(room, mobile_ref, 836)

      roll <= 75 ->
        # pristine scroll
        give_item(room, mobile_ref, 835)

      roll <= 100 ->
        # large silvery cross
        give_item(room, mobile_ref, 831)
    end
  end

  def random_item_4106(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 10 ->
        # shadow cloak
        give_item(room, mobile_ref, 420)

      roll <= 20 ->
        # embroidered black gauntlet
        give_item(room, mobile_ref, 432)

      roll <= 30 ->
        # bloodstained shortsword
        give_item(room, mobile_ref, 204)

      roll <= 40 ->
        # black chainmail hauberk
        give_item(room, mobile_ref, 179)

      roll <= 50 ->
        # sacrificial robes
        give_item(room, mobile_ref, 202)

      roll <= 60 ->
        # opal ring
        give_item(room, mobile_ref, 435)

      roll <= 70 ->
        # ebony greataxe
        give_item(room, mobile_ref, 226)

      roll <= 80 ->
        # black scimitar
        give_item(room, mobile_ref, 458)

      roll <= 100 ->
        # yellow potion
        give_item(room, mobile_ref, 467)
    end
  end

  def random_item_4149(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 10 ->
        # ivory bracelet
        give_item(room, mobile_ref, 832)

      roll <= 30 ->
        # arcane tome
        give_item(room, mobile_ref, 833)

      roll <= 40 ->
        # darkwood shield
        give_item(room, mobile_ref, 834)

      roll <= 70 ->
        # pristine scroll
        give_item(room, mobile_ref, 835)

      roll <= 100 ->
        room
        |> random_item_2922(mobile_ref)
    end
  end

  def random_item_4151(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 25 ->
        room
        |> random_item_898(mobile_ref)
        |> random_item_898(mobile_ref)

      roll <= 45 ->
        room
        |> random_item_895(mobile_ref)
        |> random_item_898(mobile_ref)
        |> random_item_874(mobile_ref)
        |> random_item_886(mobile_ref)
        |> random_item_898(mobile_ref)
        |> random_item_889(mobile_ref)

      roll <= 80 ->
        room
        |> random_item_886(mobile_ref)
        |> random_item_889(mobile_ref)
        |> random_item_889(mobile_ref)
        |> random_item_889(mobile_ref)
        |> random_item_886(mobile_ref)

      roll <= 90 ->
        room
        |> random_item_2922(mobile_ref)
        |> random_item_889(mobile_ref)
        |> random_item_874(mobile_ref)
        # |> random_item_896(mobile_ref)
        |> random_item_898(mobile_ref)

      roll <= 100 ->
        room
        |> random_item_898(mobile_ref)
        |> random_item_895(mobile_ref)
        |> random_item_898(mobile_ref)
        |> random_item_874(mobile_ref)
        |> random_item_886(mobile_ref)
        |> random_item_889(mobile_ref)
        |> random_item_2922(mobile_ref)
    end
  end

  def random_item_9059(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 9 ->
        # sharktooth trident
        give_item(room, mobile_ref, 1068)

      roll <= 18 ->
        # beaded belt
        give_item(room, mobile_ref, 1218)

      roll <= 26 ->
        # trollskin boots
        give_item(room, mobile_ref, 1232)

      roll <= 29 ->
        # mithril earrings
        give_item(room, mobile_ref, 1238)

      roll <= 30 ->
        # ruby earrings
        give_item(room, mobile_ref, 1237)

      roll <= 34 ->
        # sapling longbow
        give_item(room, mobile_ref, 1201)

      roll <= 38 ->
        # ruby earrings
        give_item(room, mobile_ref, 1237)

      roll <= 43 ->
        # wyvernhide pants
        give_item(room, mobile_ref, 1194)

      roll <= 48 ->
        # emerald studded bracelet
        give_item(room, mobile_ref, 1325)

      roll <= 53 ->
        # wyvernhide tunic
        give_item(room, mobile_ref, 412)

      roll <= 58 ->
        # stormetal scalemail tunic
        give_item(room, mobile_ref, 1214)

      roll <= 63 ->
        # red chitin shield
        give_item(room, mobile_ref, 1200)

      roll <= 65 ->
        # mithril plate leggings
        give_item(room, mobile_ref, 1192)

      roll <= 66 ->
        # giant spiked collar
        give_item(room, mobile_ref, 1205)

      roll <= 69 ->
        # bronze chakram
        give_item(room, mobile_ref, 1203)

      roll <= 72 ->
        # mithril chain leggings
        give_item(room, mobile_ref, 1193)

      roll <= 74 ->
        # ogre bone bracelet
        give_item(room, mobile_ref, 1220)

      roll <= 79 ->
        # javelin
        give_item(room, mobile_ref, 1190)

      roll <= 84 ->
        # lionskin belt
        give_item(room, mobile_ref, 1209)

      roll <= 86 ->
        # stormhammer
        give_item(room, mobile_ref, 877)

      roll <= 90 ->
        # stormhammer
        give_item(room, mobile_ref, 1207)

      roll <= 100 ->
        room
        |> random_item_9062(mobile_ref)
    end
  end

  def random_item_9060(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 5 ->
        # bloodstone
        give_item(room, mobile_ref, 887)

      roll <= 10 ->
        # carnelion stone
        give_item(room, mobile_ref, 888)

      roll <= 15 ->
        # moonstone
        give_item(room, mobile_ref, 889)

      roll <= 20 ->
        # onyx stone
        give_item(room, mobile_ref, 890)

      roll <= 25 ->
        # piece of crystal
        give_item(room, mobile_ref, 891)

      roll <= 34 ->
        # piece of amber
        give_item(room, mobile_ref, 892)

      roll <= 43 ->
        # amethyst stone
        give_item(room, mobile_ref, 893)

      roll <= 52 ->
        # piece of jade
        give_item(room, mobile_ref, 894)

      roll <= 58 ->
        # aquamarine stone
        give_item(room, mobile_ref, 895)

      roll <= 64 ->
        # garnet
        give_item(room, mobile_ref, 896)

      roll <= 70 ->
        # pearl
        give_item(room, mobile_ref, 897)

      roll <= 76 ->
        # topaz stone
        give_item(room, mobile_ref, 898)

      roll <= 81 ->
        # black opal
        give_item(room, mobile_ref, 899)

      roll <= 86 ->
        # fire opal
        give_item(room, mobile_ref, 900)

      roll <= 92 ->
        # opal
        give_item(room, mobile_ref, 901)

      roll <= 94 ->
        # diamond
        give_item(room, mobile_ref, 902)

      roll <= 96 ->
        # emerald
        give_item(room, mobile_ref, 903)

      roll <= 98 ->
        # ruby
        give_item(room, mobile_ref, 904)

      roll <= 100 ->
        room
        |> random_item_9060(mobile_ref)
        |> random_item_9061(mobile_ref)
    end
  end

  def random_item_9061(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 3 ->
        # black potion
        give_item(room, mobile_ref, 879)

      roll <= 8 ->
        # golden vial
        give_item(room, mobile_ref, 880)

      roll <= 18 ->
        # turquoise potion
        give_item(room, mobile_ref, 465)

      roll <= 22 ->
        # crimson bracers
        give_item(room, mobile_ref, 425)

      roll <= 26 ->
        # mithril ring
        give_item(room, mobile_ref, 434)

      roll <= 30 ->
        # moonstone ring
        give_item(room, mobile_ref, 436)

      roll <= 34 ->
        # piece of amber
        give_item(room, mobile_ref, 892)

      roll <= 43 ->
        # amethyst stone
        give_item(room, mobile_ref, 893)

      roll <= 42 ->
        # piece of jade
        give_item(room, mobile_ref, 894)

      roll <= 48 ->
        # aquamarine stone
        give_item(room, mobile_ref, 895)

      roll <= 54 ->
        # garnet
        give_item(room, mobile_ref, 896)

      roll <= 60 ->
        # pearl
        give_item(room, mobile_ref, 897)

      roll <= 66 ->
        # topaz stone
        give_item(room, mobile_ref, 898)

      roll <= 76 ->
        # fire opal
        give_item(room, mobile_ref, 900)

      roll <= 77 ->
        # opal
        give_item(room, mobile_ref, 901)

      roll <= 80 ->
        # diamond
        give_item(room, mobile_ref, 902)

      roll <= 86 ->
        # emerald
        give_item(room, mobile_ref, 903)

      roll <= 90 ->
        # ruby
        give_item(room, mobile_ref, 904)

      roll <= 100 ->
        room
        |> random_item_9061(mobile_ref)
        |> random_item_9062(mobile_ref)
    end
  end

  def random_item_9062(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 4 ->
        # crimson scale gauntlets
        give_item(room, mobile_ref, 1336)

      roll <= 10 ->
        # hide inscripted scroll
        give_item(room, mobile_ref, 1879)

      roll <= 15 ->
        # black leather belt
        give_item(room, mobile_ref, 1227)

      roll <= 20 ->
        # sunsword
        give_item(room, mobile_ref, 221)

      roll <= 29 ->
        # dragontooth trident
        give_item(room, mobile_ref, 874)

      roll <= 35 ->
        # gunsen
        give_item(room, mobile_ref, 1253)

      roll <= 40 ->
        # spider silk robes
        give_item(room, mobile_ref, 1262)

      roll <= 43 ->
        # webbed gauntlets
        give_item(room, mobile_ref, 1256)

      roll <= 53 ->
        # bronze chakram
        give_item(room, mobile_ref, 1203)

      roll <= 65 ->
        # stormmetal bracers
        give_item(room, mobile_ref, 1215)

      roll <= 76 ->
        # black potion
        give_item(room, mobile_ref, 879)

      roll <= 81 ->
        # emerald studded bracelet
        give_item(room, mobile_ref, 1325)

      roll <= 86 ->
        # trollskin boots
        give_item(room, mobile_ref, 1232)

      roll <= 94 ->
        # golden vial
        give_item(room, mobile_ref, 880)

      roll <= 100 ->
        # vorpal sword
        give_item(room, mobile_ref, 220)
    end
  end

  def random_item_9384(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 10 ->
        room
        |> random_item_895(mobile_ref)

      roll <= 20 ->
        room
        |> random_item_898(mobile_ref)

      roll <= 30 ->
        room
        |> random_item_2944(mobile_ref)

      roll <= 40 ->
        room
        |> random_item_9059(mobile_ref)

      roll <= 50 ->
        room
        |> random_item_9469(mobile_ref)

      roll <= 100 ->
        room
        |> random_item_9387(mobile_ref)
    end
  end

  def random_item_9385(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 10 ->
        room
        |> random_item_895(mobile_ref)

      roll <= 20 ->
        room
        |> random_item_898(mobile_ref)

      roll <= 30 ->
        room
        |> random_item_2944(mobile_ref)

      roll <= 40 ->
        room
        |> random_item_9059(mobile_ref)

      roll <= 50 ->
        room
        |> random_item_9469(mobile_ref)

      roll <= 100 ->
        room
        |> random_item_9387(mobile_ref)
    end
  end

  def random_item_9387(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 25 ->
        room
        |> random_item_9467(mobile_ref)

      roll <= 55 ->
        room
        |> random_item_9468(mobile_ref)

      roll <= 75 ->
        room
        |> random_item_9469(mobile_ref)

      roll <= 100 ->
        room
        |> random_item_9470(mobile_ref)
    end
  end

  def random_item_9467(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 10 ->
        # cruel bone greatsword
        give_item(room, mobile_ref, 1436)

      roll <= 15 ->
        # jewel-encrusted warhammer
        give_item(room, mobile_ref, 1448)

      roll <= 18 ->
        # rod of might
        give_item(room, mobile_ref, 1475)

      roll <= 25 ->
        # demon-carved halberd
        give_item(room, mobile_ref, 1470)

      roll <= 30 ->
        # ruby staff
        give_item(room, mobile_ref, 1514)

      roll <= 40 ->
        # iron wand
        give_item(room, mobile_ref, 1503)

      roll <= 50 ->
        # yellow potion
        give_item(room, mobile_ref, 1658)

      roll <= 60 ->
        # crimson potion
        give_item(room, mobile_ref, 1659)

      roll <= 70 ->
        # bubbling blue potion
        give_item(room, mobile_ref, 1660)

      roll <= 80 ->
        # grey potion
        give_item(room, mobile_ref, 1661)

      roll <= 90 ->
        # sickly green potion
        give_item(room, mobile_ref, 1662)

      roll <= 100 ->
        # frothing brown potion
        give_item(room, mobile_ref, 1664)
    end
  end

  def random_item_9468(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 20 ->
        room

      roll <= 30 ->
        # vorpal hatchet
        give_item(room, mobile_ref, 1445)

      roll <= 40 ->
        # ethereal dagger
        give_item(room, mobile_ref, 1454)

      roll <= 50 ->
        # bone club
        give_item(room, mobile_ref, 1473)

      roll <= 55 ->
        # blackwood cudgel
        give_item(room, mobile_ref, 1474)

      roll <= 60 ->
        # blackwood longbow
        give_item(room, mobile_ref, 1514)

      roll <= 65 ->
        # blackwood war-spear
        give_item(room, mobile_ref, 1478)

      roll <= 70 ->
        # templar greatsword
        give_item(room, mobile_ref, 1480)

      roll <= 80 ->
        # arlesian longsword
        give_item(room, mobile_ref, 1651)

      roll <= 90 ->
        # arlysian mace
        give_item(room, mobile_ref, 1652)

      roll <= 100 ->
        # arlysian dagger
        give_item(room, mobile_ref, 1653)
    end
  end

  def random_item_9469(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 5 ->
        # platinum plate boots
        give_item(room, mobile_ref, 1880)

      roll <= 9 ->
        # hide inscripted scroll
        give_item(room, mobile_ref, 1879)

      roll <= 16 ->
        # diamond-encrusted bracelet
        give_item(room, mobile_ref, 1468)

      roll <= 20 ->
        # blackened bracelet
        give_item(room, mobile_ref, 1470)

      roll <= 25 ->
        # bone ring
        give_item(room, mobile_ref, 1472)

      roll <= 30 ->
        # feather-tipped hat
        give_item(room, mobile_ref, 1482)

      roll <= 40 ->
        # blackwood shield
        give_item(room, mobile_ref, 1645)

      roll <= 45 ->
        # blackwood harp
        give_item(room, mobile_ref, 1650)

      roll <= 50 ->
        # amber pearl earrings
        give_item(room, mobile_ref, 1516)

      roll <= 55 ->
        # fur cloak
        give_item(room, mobile_ref, 1450)

      roll <= 56 ->
        # fur gloves
        give_item(room, mobile_ref, 1451)

      roll <= 57 ->
        # amber pearl earrings
        give_item(room, mobile_ref, 1516)

      roll <= 59 ->
        # fur boots
        give_item(room, mobile_ref, 1517)

      roll <= 60 ->
        # brown leather boots
        give_item(room, mobile_ref, 1518)

      roll <= 71 ->
        # bleeding main-gauche
        give_item(room, mobile_ref, 1520)

      roll <= 82 ->
        # onyx earrings
        give_item(room, mobile_ref, 1521)

      roll <= 90 ->
        # battered chainmail hauberk
        give_item(room, mobile_ref, 1522)

      roll <= 96 ->
        # serrated spiked shield
        give_item(room, mobile_ref, 1523)

      roll <= 100 ->
        # black velvet gloves
        give_item(room, mobile_ref, 1524)
    end
  end

  def random_item_9470(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 10 ->
        # crimson scalemail tunic
        give_item(room, mobile_ref, 1491)

      roll <= 20 ->
        # crimson scalemail leggings
        give_item(room, mobile_ref, 1492)

      roll <= 30 ->
        # crimson helm
        give_item(room, mobile_ref, 1493)

      roll <= 40 ->
        # etched adamant broadsword
        give_item(room, mobile_ref, 1005)

      roll <= 50 ->
        # etched adamant warhammer
        give_item(room, mobile_ref, 1069)

      roll <= 60 ->
        # etched adamant greatsword
        give_item(room, mobile_ref, 1070)

      roll <= 70 ->
        # crimson scale gauntlets
        give_item(room, mobile_ref, 1336)

      roll <= 80 ->
        # crimson spiked shield
        give_item(room, mobile_ref, 1067)

      roll <= 100 ->
        room
    end
  end

  def random_item_9645(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 50 ->
        room

      roll <= 100 ->
        room
        |> random_item_9646(mobile_ref)
        |> random_item_9646(mobile_ref)
        |> random_item_9646(mobile_ref)
    end
  end

  def random_item_9646(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 5 ->
        # golden sun robes
        room = give_item(room, mobile_ref, 1666)
        character = room.mobiles[mobile_ref]

        # if golden sun robes could not be spawned because it is a limited item
        # then give a longsword instead
        if !Enum.find(character.inventory ++ room.items, &(&1.name == "golden sun robes")) do
          # morningstar ring
          give_item(room, mobile_ref, 1656)
        else
          room
        end

      roll <= 25 ->
        # morningstar ring
        give_item(room, mobile_ref, 1656)

      roll <= 50 ->
        # holy miter
        give_item(room, mobile_ref, 400)

      roll <= 100 ->
        # holy vestements
        give_item(room, mobile_ref, 409)
    end
  end
end
