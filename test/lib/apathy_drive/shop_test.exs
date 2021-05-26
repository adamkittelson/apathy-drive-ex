defmodule ApathyDrive.ShopTest do
  use ExUnit.Case, async: true

  alias ApathyDrive.{Character, Item, Shop}

  describe "buying an item with a base price of 1 gold at a 2x cost multiplier" do
    setup [:an_item_with_a_base_price_of_1_gold, :a_shop_with_a_2x_cost_multiplier]

    test "can be bought for 192 copper if the seller has 70 charm", %{item: item, shop: shop} do
      assert 192 == Shop.buy_price(shop, %Character{charm: 70}, item)
    end

    test "can be bought for 200 copper if the seller has 50 charm", %{item: item, shop: shop} do
      assert 200 == Shop.buy_price(shop, %Character{charm: 50}, item)
    end

    test "can be bought for 210 copper if the seller has 25 charm", %{item: item, shop: shop} do
      assert 210 == Shop.buy_price(shop, %Character{charm: 25}, item)
    end
  end

  describe "buying an item with a base price of 5 gold at a 2.5x cost multiplier" do
    setup [:an_item_with_a_base_price_of_5_gold, :a_shop_with_a_2_5x_cost_multiplier]

    test "can be bought for 1200 copper if the seller has 70 charm", %{item: item, shop: shop} do
      assert 1200 == Shop.buy_price(shop, %Character{charm: 70}, item)
    end

    test "can be bought for 1250 copper if the seller has 50 charm", %{item: item, shop: shop} do
      assert 1250 == Shop.buy_price(shop, %Character{charm: 50}, item)
    end

    test "can be bought for 1312 copper if the seller has 25 charm", %{item: item, shop: shop} do
      assert 1312 == Shop.buy_price(shop, %Character{charm: 25}, item)
    end
  end

  describe "selling an item with a base price of 1 gold" do
    setup [:an_item_with_a_base_price_of_1_gold, :a_shop_with_a_2x_cost_multiplier]

    test "sells for 60 copper if the seller has 70 charm", %{item: item, shop: shop} do
      assert 60 == Shop.sell_price(shop, %Character{charm: 70}, item)
    end

    test "sells for 50 copper if the seller has 50 charm", %{item: item, shop: shop} do
      assert 50 == Shop.sell_price(shop, %Character{charm: 50}, item)
    end

    test "sells for 37 copper if the seller has 25 charm", %{item: item, shop: shop} do
      assert 37 == Shop.sell_price(shop, %Character{charm: 25}, item)
    end
  end

  describe "selling an item with a base price of 5 gold" do
    setup [:an_item_with_a_base_price_of_5_gold, :a_shop_with_a_2x_cost_multiplier]

    test "sells for 300 copper if the seller has 70 charm", %{item: item, shop: shop} do
      assert 300 == Shop.sell_price(shop, %Character{charm: 70}, item)
    end

    test "sells for 250 copper if the seller has 50 charm", %{item: item, shop: shop} do
      assert 250 == Shop.sell_price(shop, %Character{charm: 50}, item)
    end

    test "sells for 185 copper if the seller has 25 charm", %{item: item, shop: shop} do
      assert 185 == Shop.sell_price(shop, %Character{charm: 25}, item)
    end
  end

  test "an item cannot be sold for more than its purchase price because a sell" do
    item = %Item{quality_level: 20, quality: "unique"}
    shop = %Shop{cost_multiplier: 1}

    sell_price = Shop.sell_price(shop, %Character{charm: 500}, item)
    buy_price = Shop.buy_price(shop, %Character{charm: 500}, item)

    assert sell_price < buy_price
    assert sell_price == trunc(buy_price * 0.9)
  end

  defp an_item_with_a_base_price_of_1_gold(context) do
    item = %Item{quality_level: 1, quality: "normal"}
    Map.put(context, :item, item)
  end

  defp an_item_with_a_base_price_of_5_gold(context) do
    item = %Item{quality_level: 1, quality: "magic"}
    Map.put(context, :item, item)
  end

  defp a_shop_with_a_2x_cost_multiplier(context) do
    Map.put(context, :shop, %Shop{cost_multiplier: 2})
  end

  defp a_shop_with_a_2_5x_cost_multiplier(context) do
    Map.put(context, :shop, %Shop{cost_multiplier: 2.5})
  end
end
