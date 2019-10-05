defmodule ApathyDrive.Scripts.GoToHell do
  alias ApathyDrive.{Mobile, Room}

  def execute(%Room{} = room, _mobile_ref, target_ref) do
    Room.update_mobile(room, target_ref, fn room, target ->
      message =
        "<p><span class='red'>FFFFFF</span><span class='white'>\\</span>  <span class='red'>AAAAAA</span><span class='white'>\\</span>  <span class='red'>TTTTTT</span><span class='white'>\\</span>  <span class='red'>AAAAAA</span><span class='white'>\\</span>  <span class='red'>LL</span><span class='white'>\\</span>      <span class='red'>III</span><span class='white'>\\</span>  <span class='red'>TTTTTT</span><span class='white'>\\</span>  <span class='red'>YY<span class='white'>\\</span> YY</span><span class='white'>\\</span></p>"

      Room.send_scroll(room, message)

      message =
        "<p><span class='red'>FFFFFF</span><span class='white'>\\ </span> <span class='red'>AA</span><span class='white'>\\\\</span><span class='red'>AA</span><span class='white'>\\  <span class='red'>TTTTTT</span><span class='white'>\\  <span class='red'>AA</span><span class='white'>\\\\</span><span class='red'>AA</span><span class='white'>\\  </span><span class='red'>LL</span><span class='white'>\\      <span class='red'>III</span><span class='white'>\\  <span class='red'>TTTTTT</span><span class='white'>\\  </span><span class='red'>YY<span class='white'>\\</span> YY</span><span class='white'>\\</span></p>"

      Room.send_scroll(room, message)

      message =
        "<p><span class='red'>FF</span><span class='white'>\\\\\\\\\\  <span class='red'>AA</span><span class='white'>\\ </span><span class='red'>AA</span><span class='white'>\\  \\\\<span class='red'>TT</span><span class='white'>\\\\\\  <span class='red'>AA</span><span class='white'>\\</span> <span class='red'>AA</span><span class='white'>\\  </span><span class='red'>LL</span><span class='white'>\\</span>      <span class='red'>III</span><span class='white'>\\  \\\\</span><span class='red'>TT</span><span class='white'>\\\\\\  </span><span class='red'>YY<span class='white'>\\</span> YY</span><span class='white'>\\</span></p>"

      Room.send_scroll(room, message)

      message =
        "<p><span class='red'>FFFFFF</span><span class='white'>\\</span><span class='red'>  AAAAAA</span><span class='white'>\\</span><span class='red'>    TT</span><span class='white'>\\</span><span class='red'>    AAAAAA</span><span class='white'>\\  </span><span class='red'>LL</span><span class='white'>\\      </span><span class='red'>III</span><span class='white'>\\    </span><span class='red'>TT</span><span class='white'>\\    </span><span class='red'>YYYYYY</span><span class='white'>\\</p>"

      Room.send_scroll(room, message)

      message =
        "<p><span class='red'>FF</span><span class='white'>\\\\\\\\\\  </span><span class='red'>AA</span><span class='white'>\\\\</span><span class='red'>AA</span><span class='white'>\\    </span><span class='red'>TT</span><span class='white'>\\    </span><span class='red'>AA</span><span class='white'>\\\\</span><span class='red'>AA</span><span class='white'>\\  </span><span class='red'>LL</span><span class='white'>\\      </span><span class='red'>III</span><span class='white'>\\    </span><span class='red'>TT</span><span class='white'>\\    \\\\</span><span class='red'>YY</span><span class='white'>\\\\\\</span></p>"

      Room.send_scroll(room, message)

      message =
        "<p><span class='red'>FF</span><span class='white'>\\      </span><span class='red'>AA</span><span class='white'>\\ </span><span class='red'>AA</span><span class='white'>\\    </span><span class='red'>TT</span><span class='white'>\\    </span><span class='red'>AA</span><span class='white'>\\ </span><span class='red'>AA</span><span class='white'>\\  </span><span class='red'>LLLLLL</span><span class='white'>\\  </span><span class='red'>III</span><span class='white'>\\    </span><span class='red'>TT</span><span class='white'>\\      </span><span class='red'>YY</span><span class='white'>\\</span></p>"

      Room.send_scroll(room, message)

      message =
        "<p><span class='red'>FF</span><span class='white'>\\      </span><span class='red'>AA</span><span class='white'>\\ </span><span class='red'>AA</span><span class='white'>\\    </span><span class='red'>TT</span><span class='white'>\\    </span><span class='red'>AA</span><span class='white'>\\ </span><span class='red'>AA</span><span class='white'>\\  </span><span class='red'>LLLLLL</span><span class='white'>\\  </span><span class='red'>III</span><span class='white'>\\    </span><span class='red'>TT</span><span class='white'>\\      </span><span class='red'>YY</span><span class='white'>\\</span></p>"

      Room.send_scroll(room, message)

      message =
        "<p><span class='white'>\\\\\\      \\\\\\ \\\\\\    \\\\\\    \\\\\\ \\\\\\  \\\\\\\\\\\\\\  \\\\\\\\    \\\\\\      \\\\\\<span></p>"

      Room.send_scroll(room, message)

      Mobile.die(target, room)
    end)
  end
end
