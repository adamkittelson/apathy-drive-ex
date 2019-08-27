defmodule ApathyDrive.Scripts do
  alias ApathyDrive.{Character, Currency, Item, ItemInstance, Mobile, Repo, Room}
  require Ecto.Query

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

    Room.update_mobile(room, mobile_ref, fn mobile ->
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

    Room.update_mobile(room, mobile_ref, fn mobile ->
      level = if item.type in ["Armour", "Weapon"], do: mobile.level, else: nil

      if item.weight <= Character.max_encumbrance(mobile) - Character.encumbrance(mobile) do
        instance =
          %ItemInstance{
            item_id: item_id,
            room_id: nil,
            character_id: mobile.id,
            dropped_for_character_id: mobile.id,
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

        mobile
      else
        instance =
          %ItemInstance{
            item_id: item_id,
            room_id: room.id,
            character_id: nil,
            dropped_for_character_id: mobile.id,
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
        give_item(room, mobile_ref, "serpent armbands")

      roll <= 29 ->
        give_item(room, mobile_ref, "padded black boots")

      roll <= 31 ->
        give_item(room, mobile_ref, "runed cowl")

      roll <= 32 ->
        give_item(room, mobile_ref, "eagle helm")

      roll <= 33 ->
        give_item(room, mobile_ref, "shadow cloak")

      roll <= 34 ->
        give_item(room, mobile_ref, "elven cloak")

      roll <= 35 ->
        give_item(room, mobile_ref, "elven boots")

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
        give_item(room, mobile_ref, "diamond")

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
        give_item(room, mobile_ref, "serpent armbands")

      roll <= 30 ->
        give_item(room, mobile_ref, "cat's-eye pendant")

      roll <= 31 ->
        give_item(room, mobile_ref, "demonhide bracers")

      roll <= 32 ->
        give_item(room, mobile_ref, "crimson bracers")

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
        give_item(room, mobile_ref, "elven boots")

      roll <= 50 ->
        give_item(room, mobile_ref, "crested war helm")

      roll <= 52 ->
        give_item(room, mobile_ref, "gauntlets of power")

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

  def random_item_898(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 3 ->
        give_item(room, mobile_ref, "black potion")

      roll <= 5 ->
        give_item(room, mobile_ref, "golden vial")

      roll <= 6 ->
        give_item(room, mobile_ref, "starsteel plate boots")

      roll <= 8 ->
        give_item(room, mobile_ref, "starsteel plate gauntlets")

      roll <= 10 ->
        give_item(room, mobile_ref, "starsteel greathelm")

      roll <= 12 ->
        give_item(room, mobile_ref, "starsteel plate leggings")

      roll <= 14 ->
        give_item(room, mobile_ref, "starsteel bracers")

      roll <= 16 ->
        give_item(room, mobile_ref, "starsteel shortsword")

      roll <= 17 ->
        give_item(room, mobile_ref, "starsteel corselet")

      roll <= 18 ->
        give_item(room, mobile_ref, "turquoise potion")

      roll <= 22 ->
        give_item(room, mobile_ref, "crimson bracers")

      roll <= 26 ->
        give_item(room, mobile_ref, "mithril ring")

      roll <= 30 ->
        give_item(room, mobile_ref, "moonstone ring")

      roll <= 34 ->
        give_item(room, mobile_ref, "belt of might")

      roll <= 38 ->
        give_item(room, mobile_ref, "gauntlets of power")

      roll <= 43 ->
        give_item(room, mobile_ref, "prismatic robes")

      roll <= 48 ->
        give_item(room, mobile_ref, "laquered battle armour")

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
        give_item(room, mobile_ref, "prismatic slippers")

      roll <= 69 ->
        give_item(room, mobile_ref, "mithril chainmail hauberk")

      roll <= 70 ->
        give_item(room, mobile_ref, "prismatic gloves")

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
        give_item(room, mobile_ref, "astral slippers")

      roll <= 91 ->
        give_item(room, mobile_ref, "astral trousers")

      roll <= 93 ->
        give_item(room, mobile_ref, "astral gloves")

      roll <= 95 ->
        give_item(room, mobile_ref, "dragontooth trident")

      roll <= 98 ->
        give_item(room, mobile_ref, "serpent staff")

      roll <= 100 ->
        give_item(room, mobile_ref, "wicked bone scythe")
    end
  end
end
