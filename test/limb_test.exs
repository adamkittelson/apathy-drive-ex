defmodule LimbTest do
  use ExUnit.Case

  setup data do
    {:ok, character } = Entity.init

    case data[:character] do
      "open-handed" ->
        Entity.add_component(character,
                                         Components.Limbs,
                                         %{
                                           "left hand"  => %{"items" => []},
                                           "right hand" => %{"items" => []}
                                         })
     "wielding one-handed" ->
       Entity.add_component(character,
                                        Components.Limbs,
                                        %{
                                          "left hand"  => %{"items" => ["sword"]},
                                          "right hand" => %{"items" => []}
                                        })
     "dual-wielding" ->
       Entity.add_component(character,
                                        Components.Limbs,
                                        %{
                                          "left hand"  => %{"items" => ["sword"]},
                                          "right hand" => %{"items" => ["club"]}
                                        })
    "wielding two-handed" ->
       Entity.add_component(character,
                                        Components.Limbs,
                                        %{
                                          "left hand"  => %{"items" => ["staff"]},
                                          "right hand" => %{"items" => ["staff"]}
                                        })
    end

    {:ok, club } = Entity.init
    Entity.add_component(club, Components.WornOn, %{"hand" => 1})
    Entity.add_component(club, Components.Slot, "weapon")
    Entity.add_component(club, Components.ID, "club")

    {:ok, sword } = Entity.init
    Entity.add_component(sword, Components.WornOn, %{"hand" => 1})
    Entity.add_component(sword, Components.Slot, "weapon")
    Entity.add_component(sword, Components.ID, "sword")

    {:ok, staff } = Entity.init
    Entity.add_component(staff, Components.WornOn, %{"hand" => 2})
    Entity.add_component(staff, Components.Slot, "weapon")
    Entity.add_component(staff, Components.ID, "staff")

    {:ok, gloves } = Entity.init
    Entity.add_component(gloves, Components.WornOn, %{"hand" => 2})
    Entity.add_component(gloves, Components.Slot, "gauntlets")
    Entity.add_component(gloves, Components.ID, "gloves")

    { :ok, %{character: character, club: club, sword: sword, staff: staff, gloves: gloves} }
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

  @tag character: "wielding one-handed"
  test "an entity wielding a one-handed weapon will not remove it to wear hand armour", data do
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"items" => ["sword"]},
                                                         "right hand" => %{"items" => []}
                                                       }

    Components.Limbs.equip(data[:character], data[:gloves])

    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"items" => ["gloves","sword"]},
                                                         "right hand" => %{"items" => ["gloves"]}
                                                        }
  end

  @tag character: "dual-wielding"
  test "an entity wielding two one-handed weapons will not remove them to wear hand armour", data do
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"items" => ["sword"]},
                                                         "right hand" => %{"items" => ["club"]}
                                                       }

    Components.Limbs.equip(data[:character], data[:gloves])

    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"items" => ["gloves","sword"]},
                                                         "right hand" => %{"items" => ["gloves","club"]}
                                                        }
  end

  @tag character: "wielding two-handed"
  test "an entity wielding a two-handed weapon will not remove them to wear hand armour", data do
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"items" => ["staff"]},
                                                         "right hand" => %{"items" => ["staff"]}
                                                       }

    Components.Limbs.equip(data[:character], data[:gloves])

    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"items" => ["gloves","staff"]},
                                                         "right hand" => %{"items" => ["gloves","staff"]}
                                                        }
  end

  @tag character: "wielding two-handed"
  test "an entity wielding a two-handed weapon will remove it to wield a one-handed weapon", data do
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"items" => ["staff"]},
                                                         "right hand" => %{"items" => ["staff"]}
                                                       }

    Components.Limbs.equip(data[:character], data[:sword])

    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"items" => ["sword"]},
                                                         "right hand" => %{"items" => []}
                                                        }
  end
end
