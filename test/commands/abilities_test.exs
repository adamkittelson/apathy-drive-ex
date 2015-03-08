defmodule Commands.AbilitiesTest do
  use ExUnit.Case
  use ShouldI
  import ApathyDrive.Matchers

  with "a spirit" do
    setup context do
      Dict.put(context, :spirit, %Spirit{socket: %Phoenix.Socket{pid: self}})
    end

    should_add_to_scroll "<p>You must be possessing a monster to have abilities.</p>" do
      Commands.Abilities.execute(context.spirit, [])
    end
  end

end
