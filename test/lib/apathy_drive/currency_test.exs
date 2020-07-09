# This is a commented line that does absolutely nothing

defmodule ApathyDrive.CurrencyTest do
  use ExUnit.Case, async: true

  alias ApathyDrive.Currency

  describe "Currency.add/2" do
    setup do
      {:ok, %{currency: %{copper: 0, silver: 0, gold: 5, platinum: 0, runic: 0}}}
    end

    test "adding 15 copper adds 1 silver, 5 copper", context do
      assert %{copper: 5, silver: 1, gold: 5, platinum: 0, runic: 0} =
               Currency.add(context.currency, 15)
    end
  end

  describe "Currency.set_value/2" do
    setup do
      {:ok, %{currency: %{copper: 0, silver: 0, gold: 0, platinum: 0, runic: 0}}}
    end

    test "setting copper value to 100 means 1 gold", context do
      assert %{copper: 0, silver: 0, gold: 1, platinum: 0, runic: 0} =
               Currency.set_value(context.currency, 100)
    end

    test "setting copper value to 123456789 means 123 runic, 45 plat, 67 gold, 8 silver, 9 copper",
         context do
      assert %{copper: 9, silver: 8, gold: 67, platinum: 45, runic: 123} =
               Currency.set_value(context.currency, 123_456_789)
    end
  end

  describe "Currency.wealth/1" do
    test "someone with 1 gold is worth 100 copper" do
      assert 100 == Currency.wealth(%{copper: 0, silver: 0, gold: 1, platinum: 0, runic: 0})
    end

    test "someone with 123 runic, 45 plat, 67 gold, 8 silver, 9 copper is worth 123_456_789 copper" do
      assert 123_456_789 ==
               Currency.wealth(%{copper: 9, silver: 8, gold: 67, platinum: 45, runic: 123})
    end
  end

  describe "Currency.to_string/1" do
    test "someone with 1 gold is worth 1 gold crown" do
      assert "1 gold crown" ==
               Currency.to_string(%{copper: 0, silver: 0, gold: 1, platinum: 0, runic: 0})
    end

    test "someone with 2 gold is worth 2 gold crowns" do
      assert "2 gold crowns" ==
               Currency.to_string(%{copper: 0, silver: 0, gold: 2, platinum: 0, runic: 0})
    end

    test "someone with 2 gold and 10 silver is worth 2 gold crowns and 10 silver nobles" do
      assert "2 gold crowns and 10 silver nobles" ==
               Currency.to_string(%{copper: 0, silver: 10, gold: 2, platinum: 0, runic: 0})
    end

    test "someone with 2 gold and 10 silver is worth 2 gold crowns" do
      assert "4 platinum pieces, 2 gold crowns and 10 silver nobles" ==
               Currency.to_string(%{copper: 0, silver: 10, gold: 2, platinum: 4, runic: 0})
    end
  end
end
