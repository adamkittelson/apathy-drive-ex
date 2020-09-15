defmodule ApathyDrive.Scripts.SeherSahham do
  alias ApathyDrive.{Currency, Mobile, Repo, Room}

  def ask(%Room{} = room, mobile_ref, _target_ref, "activate") do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      price_in_copper = 1_000_000

      if Currency.wealth(mobile) < price_in_copper do
        message =
          "<p>He says, <span class='dark-green'>\"I may be old, but I count quite well and you are short!\"</span></p>"

        Mobile.send_scroll(mobile, message)
      else
        char_currency = Currency.subtract(mobile, price_in_copper)

        mobile =
          mobile
          |> Ecto.Changeset.change(%{
            runic: char_currency.runic,
            platinum: char_currency.platinum,
            gold: char_currency.gold,
            silver: char_currency.silver,
            copper: char_currency.copper
          })
          |> Repo.update!()

        room = put_in(room.mobiles[mobile.ref], mobile)

        message = "<p>You rise through the air and pass through the barrier of water!</p>"

        Mobile.send_scroll(mobile, message)

        ApathyDrive.Scripts.Teleport.teleport(room, mobile, 21121)
      end
    end)
  end

  def ask(%Room{} = room, mobile_ref, _target_ref, "belongings") do
    Room.update_mobile(room, mobile_ref, fn _room, mobile ->
      message =
        "<p><span class='dark-green'>\"Ahh yes I had quite the collection of items. I would say I miss most\nthe huge black cauldron that I used to cook stew in. Alas, before I\nleft I gave it to a beautiful young witch at the time. She was such\na pretty young lass and everyone knows a witch needs a cauldron, so\nI was more then happy to help out. Though now if I had that cauldron\nI could make myself some fine stew on those days when I'm feeling down.\"</p>"

      Mobile.send_scroll(mobile, message)
    end)
  end

  def ask(%Room{} = room, mobile_ref, _target_ref, "fee") do
    Room.update_mobile(room, mobile_ref, fn _room, mobile ->
      message =
        "<p><span class='dark-green'>\"How nice of you to offer to help my poor old soul. When I left I\nknew times were getting tougher, though you look like you haven't\nsuffered much, so I'll only take a small amount. In fact I feel so nice\nI'll only charge you 10,000 gold. I know you would like to give me more\nbut I'm sure there are other charities you are looking to give your\nmoney to. If you have the money on you just ask me to <\/span><span class='green'>activate<\/span><span class='dark-green'> the portal\nand you will be through and there in the blink of an eye. By the \ndirt you wear I'd say you were from that small fishing village across\nthe river from my home, if you would like I could you even send you <\/span><span class='green'>home<\/span><span class='dark-green'>\nto your village, and since I'm sending you to such a backwater place I \nwouldn't even feel right about charging you.\"</p>"

      Mobile.send_scroll(mobile, message)
    end)
  end

  def ask(%Room{} = room, mobile_ref, _target_ref, "home") do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      message =
        "<p>Seher'Sahham points his finger at you and shouts, <span class='dark-green'>\"Away with you!\"</span></p>"

      Mobile.send_scroll(mobile, message)

      ApathyDrive.Scripts.Teleport.teleport(room, mobile, 144)
    end)
  end

  def ask(%Room{} = room, mobile_ref, _target_ref, "water portal") do
    Room.update_mobile(room, mobile_ref, fn _room, mobile ->
      message =
        "<p><span class='dark-green'>\"Ahhh yes the water portal. I'm not quite sure where it takes you\nbut it gets you there quickly none the less. It took me years to\nbuild it and I alone know the workings of its magic. I'm a reasonable\nman though and seeing as I haven't been home for over 500 years I'm sure\nmy wife has sold off all my </span><span class='green'>belongings<\/span><span class='dark-green'>. So for a small <\/span><span class='green'>fee<\/span><span class='dark-green'> to help\na poor old man out I'd be more then willing to help you pass through\nthe portal.\"</p>"

      Mobile.send_scroll(mobile, message)
    end)
  end

  def ask(%Room{} = room, mobile_ref, target_ref, _question) do
    target = room.mobiles[target_ref]

    Room.update_mobile(room, mobile_ref, fn _room, mobile ->
      Mobile.send_scroll(
        mobile,
        "<p><span class='dark-green'>#{target.name} has nothing to tell you!</span></p>"
      )
    end)
  end
end
