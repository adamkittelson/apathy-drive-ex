defmodule ApathyDrive.Scripts.WoodenBox do
  import ApathyDrive.Scripts
  alias ApathyDrive.{Mobile, Room}

  def execute(%Room{} = room, mobile_ref) do
    Room.update_mobile(room, mobile_ref, fn mobile ->
      Mobile.send_scroll(mobile, "<p>You open the wooden box, and find...</p>")
      Room.send_scroll(room, "<p>#{mobile.name} opens a wooden box, and finds...</p>", [mobile])
      Room.send_scroll(room, "<p>Some coins!</p>")

      room
      |> give_coins_up_to(mobile_ref, %{gold: 30, silver: 300})
      |> give_stone(mobile_ref)
      |> give_stone(mobile_ref)
      |> random_item(mobile_ref)
    end)
  end

  def random_item(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 20 ->
        Room.send_scroll(room, "<p>A potion!</p>")

        # minor healing potion, cure poison potion, blue potion, turquoise potion
        # yellow potion, silvery potion, white potion, shimmering vial
        item_id = Enum.random([223, 269, 464, 465, 467, 469, 471, 878])

        give_item(room, mobile_ref, item_id)

      roll <= 27 ->
        Room.send_scroll(room, "<p>A piece of jewelry!</p>")

        # jade amulet, amethyst pendant, ethereal amulet,
        # serpent armbands, sapphire ring, golden snake earrings
        item_id = Enum.random([406, 407, 408, 424, 433, 611])

        give_item(room, mobile_ref, item_id)

      roll <= 35 ->
        Room.send_scroll(room, "<p>A piece of armour!</p>")

        # padded black boots, eagle helm, runed cowl, shadow cloak, elven cloak, elven boots, silvery shield
        item_id = Enum.random([46, 402, 404, 420, 422, 445, 848])

        give_item(room, mobile_ref, item_id)

      roll <= 63 ->
        Room.send_scroll(room, "<p>A suit of armour!</p>")

        # runed chainmail hauberk, gilded robes, runed robes
        # gianthair vest, fine breastplate, silvery plate corselet,
        # fine chainmail hauberk, ogre-skin shirt
        item_id = Enum.random([362, 410, 411, 414, 418, 521, 846, 847])

        give_item(room, mobile_ref, item_id)

      roll <= 81 ->
        Room.send_scroll(room, "<p>A weapon!</p>")

        # glowing warhammer, fine broadsword, glowing broadsword,
        # fine mace, dwarven axe, dwarven hammer
        item_id = Enum.random([116, 295, 361, 864, 868, 869])

        give_item(room, mobile_ref, item_id)

      :else ->
        Room.send_scroll(room, "<p>A large weapon!</p>")

        # oaken staff, bladed staff, fine greatsword, black steel halberd
        # great bronzewood club, ashwood staff, three-headed flail, long golden staff
        item_id = Enum.random([459, 461, 863, 865, 866, 867, 870, 871])

        give_item(room, mobile_ref, item_id)
    end
  end

  def give_stone(room, mobile_ref) do
    roll = :rand.uniform(100)

    cond do
      roll <= 50 ->
        Room.send_scroll(room, "<p>An ornamental stone!</p>")

        # azurite stone, agate stone, hematite stone, malachite stone, obsidian stone, turquoise stone
        item_id = Enum.random(881..886)

        give_item(room, mobile_ref, item_id)

      roll <= 75 ->
        Room.send_scroll(room, "<p>A semi-precious stone!</p>")

        # bloodstone, carnelian stone, moonstone, onyx stone, piece of crystal
        item_id = Enum.random(887..891)

        give_item(room, mobile_ref, item_id)

      roll <= 95 ->
        Room.send_scroll(room, "<p>A fancy stone!</p>")

        # piece of amber, amethyst stone, piece of jade
        item_id = Enum.random(892..894)

        give_item(room, mobile_ref, item_id)

      :else ->
        Room.send_scroll(room, "<p>A precious stone!</p>")

        # aquamarine stone, garnet, pearl, topaz stone
        item_id = Enum.random(895..898)

        give_item(room, mobile_ref, item_id)
    end
  end
end
