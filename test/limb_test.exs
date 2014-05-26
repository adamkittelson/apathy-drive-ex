defmodule LimbTest do
  use ExUnit.Case

  setup data do
    {:ok, character } = ApathyDrive.Entity.init

    case data[:character] do
      "open-handed" ->
        ApathyDrive.Entity.add_component(character,
                                         Components.Limbs,
                                         %{
                                           "left hand"  => %{"items" => []},
                                           "right hand" => %{"items" => []}
                                         })
     "wielding one-handed" ->
       ApathyDrive.Entity.add_component(character,
                                        Components.Limbs,
                                        %{
                                          "left hand"  => %{"items" => ["sword"]},
                                          "right hand" => %{"items" => []}
                                        })
     "dual-wielding" ->
       ApathyDrive.Entity.add_component(character,
                                        Components.Limbs,
                                        %{
                                          "left hand"  => %{"items" => ["sword"]},
                                          "right hand" => %{"items" => ["club"]}
                                        })

    end

    {:ok, club } = ApathyDrive.Entity.init
    ApathyDrive.Entity.add_component(club, Components.WornOn, %{"hand" => 1})
    ApathyDrive.Entity.add_component(club, Components.Slot, "weapon")
    ApathyDrive.Entity.add_component(club, Components.ID, "club")

    {:ok, sword } = ApathyDrive.Entity.init
    ApathyDrive.Entity.add_component(sword, Components.WornOn, %{"hand" => 1})
    ApathyDrive.Entity.add_component(sword, Components.Slot, "weapon")
    ApathyDrive.Entity.add_component(sword, Components.ID, "sword")

    {:ok, staff } = ApathyDrive.Entity.init
    ApathyDrive.Entity.add_component(staff, Components.WornOn, %{"hand" => 2})
    ApathyDrive.Entity.add_component(staff, Components.Slot, "weapon")
    ApathyDrive.Entity.add_component(staff, Components.ID, "staff")

    { :ok, %{character: character, club: club, sword: sword, staff: staff} }
  end

  @tag character: "open-handed"
  test "an entity with two hands can wield a one-handed weapon", data do
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"items" => []},
                                                         "right hand" => %{"items" => []}
                                                       }

    Components.Limbs.equip(data[:character], data[:club])

    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"items" => ["club"]},
                                                         "right hand" => %{"items" => []}
                                                        }
  end

  @tag character: "open-handed"
  test "an entity with two hands can wield two one-handed weapons", data do
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"items" => []},
                                                         "right hand" => %{"items" => []}
                                                       }

    Components.Limbs.equip(data[:character], data[:club])
    Components.Limbs.equip(data[:character], data[:sword])

    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"items" => ["club"]},
                                                         "right hand" => %{"items" => ["sword"]}
                                                        }
  end

  @tag character: "open-handed"
  test "an entity with two hands can wield a two-handed weapon", data do
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"items" => []},
                                                         "right hand" => %{"items" => []}
                                                       }

    Components.Limbs.equip(data[:character], data[:staff])

    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"items" => ["staff"]},
                                                         "right hand" => %{"items" => ["staff"]}
                                                        }
  end

  @tag character: "wielding one-handed"
  test "an entity wielding a one-handed weapon will remove it to wear a two handed weapon", data do
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"items" => ["sword"]},
                                                         "right hand" => %{"items" => []}
                                                       }

    Components.Limbs.equip(data[:character], data[:staff])

    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"items" => ["staff"]},
                                                         "right hand" => %{"items" => ["staff"]}
                                                        }
  end

  @tag character: "dual-wielding"
  test "an entity wielding two one-handed weapons will remove them to wear a two handed weapon", data do
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"items" => ["sword"]},
                                                         "right hand" => %{"items" => ["club"]}
                                                       }

    Components.Limbs.equip(data[:character], data[:staff])

    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"items" => ["staff"]},
                                                         "right hand" => %{"items" => ["staff"]}
                                                        }
  end
end
