defmodule SpiritTest do
  use ExUnit.Case
  use ShouldI

  with "a spirit with no dodge" do
    setup context do
      Dict.put context, :spirit, %Spirit{skills: %{}}
    end

    should("have 0 dodge", context) do
      assert Spirit.skill(context.spirit, "dodge") == 0
    end
  end

  with "10 power invested in dodge" do
    setup context do
      Dict.put context, :spirit, %Spirit{skills: %{"dodge" => 10}}
    end

    should("have 4 dodge", context) do
      assert Spirit.skill(context.spirit, "dodge") == 4
    end
  end

  with "1000 devs spent on mage" do
    setup context do
      Dict.put context, :spirit, %Spirit{skills: %{"mage" => 1000}}
    end

    should("have 29 mage", context) do
      assert Spirit.skill(context.spirit, "mage") == 29
    end
  end

end
