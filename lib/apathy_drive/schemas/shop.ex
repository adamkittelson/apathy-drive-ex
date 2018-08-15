defmodule ApathyDrive.Shop do
  use ApathyDriveWeb, :model
  require Logger

  alias ApathyDrive.{Character, Item, ItemInstance, Room, Shop, ShopItem}

  schema "shops" do
    field(:cost_multiplier, :float)

    belongs_to(:room, Room)
    has_many(:shop_items, ShopItem)
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
    Logger.info("restocking #{room.name}")

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
      name = next_shop_item.item.name
      time = DateTime.diff(next_shop_item.next_restock_at, DateTime.utc_now(), :millisecond)
      Logger.info("next restock #{name} in #{time} milliseconds")

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

        shop_item = Map.put(original_shop_item, :count, count)

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
    trunc(
      Item.cost_in_copper(item) * shop.cost_multiplier *
        (1 - (div(character.charm, 5) - 10) / 100)
    )
  end

  def sell_price(shop, %Character{} = character, %Item{} = item) do
    sell_price = trunc(Item.cost_in_copper(item) * (div(character.charm, 2) + 25) / 100)
    buy_price = buy_price(shop, character, item)

    min(sell_price, trunc(buy_price * 0.9))
  end
end
