defmodule ApathyDrive.Shop do
  use ApathyDriveWeb, :model
  require Logger

  alias ApathyDrive.{Character, Item, ItemInstance, Mobile, Room, Shop, ShopItem, ShopItemType}

  schema "shops" do
    field(:cost_multiplier, :float)

    belongs_to(:room, Room)
    has_many(:shop_items, ShopItem)
    has_many(:shop_item_types, ShopItemType)
    has_many(:item_instances, ItemInstance)
  end

  def room_ids do
    Shop
    |> select([s], s.room_id)
    |> Repo.all()
  end

  def shop?(%Room{shop: %Shop{}}), do: true
  def shop?(%Room{}), do: false

  def restock(%Room{} = room) do
    Enum.each(room.shop.shop_items, fn shop_item ->
      ShopItem.restock!(room, shop_item)
    end)

    schedule_next_restock(room.shop)

    load(room)
  end

  def schedule_next_restock(%Shop{shop_items: shop_items}) do
    next_shop_item =
      shop_items
      |> Enum.filter(&(!is_nil(&1.next_restock_at)))
      |> Enum.sort_by(&DateTime.to_unix(&1.next_restock_at))
      |> List.first()

    if next_shop_item do
      time = DateTime.diff(next_shop_item.next_restock_at, DateTime.utc_now(), :millisecond)

      if time > 0 do
        Process.send_after(self(), :restock_shop, time)
      else
        send(self(), :restock_shop)
      end
    end
  end

  def load(%Room{} = room) do
    room =
      room
      |> Repo.preload([shop: [shop_items: :item, item_instances: :item]], force: true)

    if shop?(room) do
      Enum.reduce(room.shop.shop_items, room, fn original_shop_item, room ->
        count =
          room.shop.item_instances
          |> Enum.filter(&(&1.item_id == original_shop_item.item_id))
          |> length

        shop_item =
          original_shop_item
          |> Map.put(:count, count)
          |> Map.put(:name, original_shop_item.item.name)
          |> put_in([Access.key!(:item)], Item.from_assoc(original_shop_item))

        room
        |> update_in(
          [Access.key!(:shop), Access.key!(:shop_items)],
          &List.delete(&1, original_shop_item)
        )
        |> update_in([Access.key!(:shop), Access.key!(:shop_items)], &[shop_item | &1])
      end)
    else
      room
    end
  end

  def buy_price(%Shop{} = shop, %Character{} = character, %Item{} = item) do
    charm = Mobile.attribute_at_level(character, :charm, character.level)

    trunc(
      Item.cost_in_copper(item) * shop.cost_multiplier *
        (1 - (div(charm, 5) - 10) / 100)
    )
  end

  def sell_price(shop, %Character{} = character, %Item{} = item) do
    charm = Mobile.attribute_at_level(character, :charm, character.level)

    sell_price = trunc(Item.cost_in_copper(item) * (div(charm, 2) + 25) / 100)

    buy_price = buy_price(shop, character, item)

    # the lesser of the sell price or 90% of the buy price
    # to ensure that a player with enough charm can't buy and sell the same item
    # repeatedly for a profit
    min(sell_price, trunc(buy_price * 0.9))
  end

  def item_disclaimer(item, character) do
    cond do
      !Item.useable_by_character?(character, item) ->
        "<span class='dark-cyan'>(You can't use)</span>"

      Item.too_powerful_for_character?(character, item) ->
        "<span class='dark-cyan'>(Too powerful)</span>"

      :else ->
        ""
    end
  end
end
