defmodule ApathyDrive.ShopTest do
  use ExUnit.Case, async: true

  alias ApathyDrive.{Character, Item, Shop}

  describe "buying an item with a base price of 5 silver at a 2x cost multiplier" do
    setup [:an_item_with_a_base_price_of_5_silver, :a_shop_with_a_2x_cost_multiplier]

    test "can be bought for 96 copper if the seller has 70 charm", %{item: item, shop: shop} do
      assert 96 == Shop.buy_price(shop, %Character{charm: 70}, item)
    end

    test "can be bought for 100 copper if the seller has 50 charm", %{item: item, shop: shop} do
      assert 100 == Shop.buy_price(shop, %Character{charm: 50}, item)
    end

    test "can be bought for 105 copper if the seller has 25 charm", %{item: item, shop: shop} do
      assert 105 == Shop.buy_price(shop, %Character{charm: 25}, item)
    end
  end

  describe "buying an item with a base price of 4 gold at a 2.5x cost multiplier" do
    setup [:an_item_with_a_base_price_of_4_gold, :a_shop_with_a_2_5x_cost_multiplier]

    test "can be bought for 960 copper if the seller has 70 charm", %{item: item, shop: shop} do
      assert 960 == Shop.buy_price(shop, %Character{charm: 70}, item)
    end

    test "can be bought for 1000 copper if the seller has 50 charm", %{item: item, shop: shop} do
      assert 1000 == Shop.buy_price(shop, %Character{charm: 50}, item)
    end

    test "can be bought for 1050 copper if the seller has 25 charm", %{item: item, shop: shop} do
      assert 1050 == Shop.buy_price(shop, %Character{charm: 25}, item)
    end
  end

  describe "selling an item with a base price of 5 silver" do
    setup [:an_item_with_a_base_price_of_5_silver, :a_shop_with_a_2x_cost_multiplier]

    test "sells for 30 copper if the seller has 70 charm", %{item: item, shop: shop} do
      assert 30 == Shop.sell_price(shop, %Character{charm: 70}, item)
    end

    test "sells for 25 copper if the seller has 50 charm", %{item: item, shop: shop} do
      assert 25 == Shop.sell_price(shop, %Character{charm: 50}, item)
    end

    test "sells for 18 copper if the seller has 25 charm", %{item: item, shop: shop} do
      assert 18 == Shop.sell_price(shop, %Character{charm: 25}, item)
    end
  end

  describe "selling an item with a base price of 4 gold" do
    setup [:an_item_with_a_base_price_of_4_gold, :a_shop_with_a_2x_cost_multiplier]

    test "sells for 240 copper if the seller has 70 charm", %{item: item, shop: shop} do
      assert 240 == Shop.sell_price(shop, %Character{charm: 70}, item)
    end

    test "sells for 200 copper if the seller has 50 charm", %{item: item, shop: shop} do
      assert 200 == Shop.sell_price(shop, %Character{charm: 50}, item)
    end

    test "sells for 148 copper if the seller has 25 charm", %{item: item, shop: shop} do
      assert 148 == Shop.sell_price(shop, %Character{charm: 25}, item)
    end
  end

  test "an item cannot be sold for more than its purchase price because a sell" do
    item = %Item{cost_value: 1, cost_currency: "platinum piece"}
    shop = %Shop{cost_multiplier: 1}

    sell_price = Shop.sell_price(shop, %Character{charm: 500}, item)
    buy_price = Shop.buy_price(shop, %Character{charm: 500}, item)

    assert sell_price < buy_price
    assert sell_price == trunc(buy_price * 0.9)
  end

  defp an_item_with_a_base_price_of_5_silver(context) do
    Map.put(context, :item, %Item{cost_value: 5, cost_currency: "silver noble"})
  end

  defp an_item_with_a_base_price_of_4_gold(context) do
    Map.put(context, :item, %Item{cost_value: 4, cost_currency: "gold crown"})
  end

  defp a_shop_with_a_2x_cost_multiplier(context) do
    Map.put(context, :shop, %Shop{cost_multiplier: 2})
  end

  defp a_shop_with_a_2_5x_cost_multiplier(context) do
    Map.put(context, :shop, %Shop{cost_multiplier: 2.5})
  end
end
