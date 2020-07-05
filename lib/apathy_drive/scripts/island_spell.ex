defmodule ApathyDrive.Scripts.IslandSpell do
  alias ApathyDrive.Room

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      if mobile.level <= 49 do
        mobile
      else
        case :rand.uniform(100) do
          1 ->
            teleport(room, mobile, 24694)

          2 ->
            teleport(room, mobile, 24899)

          3 ->
            teleport(room, mobile, 24504)

          4 ->
            teleport(room, mobile, 24614)

          5 ->
            teleport(room, mobile, 24819)

          6 ->
            teleport(room, mobile, 24924)

          7 ->
            teleport(room, mobile, 24981)

          8 ->
            teleport(room, mobile, 24419)

          9 ->
            teleport(room, mobile, 24446)

          10 ->
            teleport(room, mobile, 24745)

          11 ->
            teleport(room, mobile, 24498)

          12 ->
            teleport(room, mobile, 24657)

          13 ->
            teleport(room, mobile, 24397)

          14 ->
            teleport(room, mobile, 24519)

          15 ->
            teleport(room, mobile, 24436)

          16 ->
            teleport(room, mobile, 24536)

          17 ->
            teleport(room, mobile, 24446)

          18 ->
            teleport(room, mobile, 24656)

          19 ->
            teleport(room, mobile, 24467)

          20 ->
            teleport(room, mobile, 24797)

          21 ->
            teleport(room, mobile, 24435)

          22 ->
            teleport(room, mobile, 24446)

          23 ->
            teleport(room, mobile, 25015)

          24 ->
            teleport(room, mobile, 24425)

          25 ->
            teleport(room, mobile, 24473)

          26 ->
            teleport(room, mobile, 24935)

          27 ->
            teleport(room, mobile, 24468)

          28 ->
            teleport(room, mobile, 24398)

          29 ->
            teleport(room, mobile, 24786)

          30 ->
            teleport(room, mobile, 24399)

          31 ->
            teleport(room, mobile, 24386)

          32 ->
            teleport(room, mobile, 24726)

          33 ->
            teleport(room, mobile, 24601)

          34 ->
            teleport(room, mobile, 24397)

          35 ->
            teleport(room, mobile, 24403)

          36 ->
            teleport(room, mobile, 24440)

          37 ->
            teleport(room, mobile, 24463)

          38 ->
            teleport(room, mobile, 24388)

          39 ->
            teleport(room, mobile, 24922)

          40 ->
            teleport(room, mobile, 24438)

          41 ->
            teleport(room, mobile, 24602)

          42 ->
            teleport(room, mobile, 24727)

          43 ->
            teleport(room, mobile, 24803)

          45 ->
            teleport(room, mobile, 24959)

          46 ->
            teleport(room, mobile, 24823)

          47 ->
            teleport(room, mobile, 24694)

          48 ->
            teleport(room, mobile, 24493)

          49 ->
            teleport(room, mobile, 24651)

          50 ->
            teleport(room, mobile, 24557)

          51 ->
            teleport(room, mobile, 24671)

          52 ->
            teleport(room, mobile, 24977)

          53 ->
            teleport(room, mobile, 24943)

          54 ->
            teleport(room, mobile, 24842)

          55 ->
            teleport(room, mobile, 24721)

          56 ->
            teleport(room, mobile, 24636)

          57 ->
            teleport(room, mobile, 24817)

          58 ->
            teleport(room, mobile, 24712)

          59 ->
            teleport(room, mobile, 24701)

          60 ->
            teleport(room, mobile, 24809)

          61 ->
            teleport(room, mobile, 24887)

          62 ->
            teleport(room, mobile, 24918)

          63 ->
            teleport(room, mobile, 24829)

          64 ->
            teleport(room, mobile, 24993)

          65 ->
            teleport(room, mobile, 24899)

          66 ->
            teleport(room, mobile, 24803)

          67 ->
            teleport(room, mobile, 24920)

          68 ->
            teleport(room, mobile, 24793)

          69 ->
            teleport(room, mobile, 24995)

          70 ->
            teleport(room, mobile, 24985)

          71 ->
            teleport(room, mobile, 24894)

          72 ->
            teleport(room, mobile, 24843)

          73 ->
            teleport(room, mobile, 24995)

          74 ->
            teleport(room, mobile, 24857)

          75 ->
            teleport(room, mobile, 24992)

          76 ->
            teleport(room, mobile, 24919)

          77 ->
            teleport(room, mobile, 24836)

          78 ->
            teleport(room, mobile, 24947)

          79 ->
            teleport(room, mobile, 24987)

          80 ->
            teleport(room, mobile, 24978)

          81 ->
            teleport(room, mobile, 24974)

          82 ->
            teleport(room, mobile, 24953)

          83 ->
            teleport(room, mobile, 24832)

          84 ->
            teleport(room, mobile, 24711)

          85 ->
            teleport(room, mobile, 24550)

          86 ->
            teleport(room, mobile, 24597)

          87 ->
            teleport(room, mobile, 24749)

          88 ->
            teleport(room, mobile, 24862)

          89 ->
            teleport(room, mobile, 24904)

          90 ->
            teleport(room, mobile, 24993)

          91 ->
            teleport(room, mobile, 24982)

          92 ->
            teleport(room, mobile, 24941)

          93 ->
            teleport(room, mobile, 24810)

          94 ->
            teleport(room, mobile, 24499)

          95 ->
            teleport(room, mobile, 24578)

          96 ->
            teleport(room, mobile, 24725)

          97 ->
            teleport(room, mobile, 24808)

          98 ->
            teleport(room, mobile, 24897)

          99 ->
            teleport(room, mobile, 24388)

          100 ->
            teleport(room, mobile, 24943)
        end
      end
    end)
  end

  def teleport(%Room{} = room, mobile, room_id) do
    room_exit = %{
      "kind" => "Action",
      "destination" => room_id,
      "mover_message" =>
        "<span class='blue'>You vanish into thin air and reappear somewhere else!</span>",
      "from_message" => "<span class='blue'>{{Name}} vanishes into thin air!</span>",
      "to_message" => "<span class='blue'>{{Name}} appears out of thin air!</span>"
    }

    case ApathyDrive.Commands.Move.execute(room, mobile, room_exit) do
      %Room{} = room ->
        room

      {:error, :too_tired, room} ->
        room
    end
  end
end
