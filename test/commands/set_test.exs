defmodule Commands.SetTest do
  use ExUnit.Case
  use ShouldI
  import ApathyDrive.Matchers

  with "a spirit that already has a name" do
    setup context do
      Dict.put(context, :spirit, %Spirit{name: "Adam",
                                         socket: %Phoenix.Socket{pid: self}})
    end

    should_add_to_scroll "<p>Not so fast, Adam, you already have a name.</p>" do
      spirit = Commands.Set.execute(context.spirit, ["name", "frank"])
      assert spirit.name == "Adam"
    end
  end

  with "a nameless spirit" do
    setup context do
      Dict.put(context, :spirit, %Spirit{name: nil,
                                         socket: %Phoenix.Socket{pid: self}})
    end

    should "not set the name if the new name is invalid", context do
      spirit = Commands.Set.execute(context.spirit, ["name", "$ephiroth"])
      assert_adds_to_scroll "<p>Your name must consist only of upper or lower case letters.</p>"
      assert spirit.name == nil
    end

    should "set the name if the new name is valid", context do
      spirit = Commands.Set.execute(context.spirit, ["name", "Adam"])
      assert_adds_to_scroll "<p>Your name has been set.</p>"
      assert spirit.name == "Adam"
    end

    should "capitalize the new name if it's lower case", context do
      spirit = Commands.Set.execute(context.spirit, ["name", "adam"])
      assert_adds_to_scroll "<p>Your name has been set.</p>"
      assert spirit.name == "Adam"
    end

  end

end
