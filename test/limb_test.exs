defmodule LimbTest do
  use ExUnit.Case

  setup data do
    {:ok, character } = Entity.init

    {:ok, club } = Entity.init
    Entity.add_component(club, Components.Module, %{worn_on: %{"hand" => 1}})
    Entity.add_component(club, Components.Slot, "weapon")
    Entity.add_component(club, Components.ID, 1)
    Entity.add_component(club, Components.Name, "club")
    Items.add(club)

    {:ok, sword } = Entity.init
    Entity.add_component(sword, Components.Module, %{worn_on: %{"hand" => 1}})
    Entity.add_component(sword, Components.Slot, "weapon")
    Entity.add_component(club, Components.ID, 2)
    Entity.add_component(club, Components.Name, "shortsword")
    Items.add(sword)

    {:ok, staff } = Entity.init
    Entity.add_component(staff, Components.Module, %{worn_on: %{"hand" => 2}})
    Entity.add_component(staff, Components.Slot, "weapon")
    Entity.add_component(club, Components.ID, 3)
    Entity.add_component(club, Components.Name, "quarterstaff")
    Items.add(staff)

    {:ok, gloves } = Entity.init
    Entity.add_component(gloves, Components.Module, %{worn_on: %{"hand" => 2}})
    Entity.add_component(gloves, Components.Slot, "gauntlets")
    Entity.add_component(club, Components.ID, 4)
    Entity.add_component(club, Components.Name, "padded gloves")
    Items.add(gloves)

    case data[:character] do
      "open-handed" ->
        Entity.add_component(character,
                             Components.Limbs,
                             %{
                               "left hand"  => %{"type" => "hand", "items" => []},
                               "right hand" => %{"type" => "hand", "items" => []}
                             })
     "wielding one-handed" ->
       Entity.add_component(character,
                            Components.Limbs,
                            %{
                              "left hand"  => %{"type" => "hand", "items" => [sword]},
                              "right hand" => %{"type" => "hand", "items" => []}
                            })
     "dual-wielding" ->
       Entity.add_component(character,
                            Components.Limbs,
                            %{
                              "left hand"  => %{"type" => "hand", "items" => [sword]},
                              "right hand" => %{"type" => "hand", "items" => [club]}
                            })
    "wielding two-handed" ->
       Entity.add_component(character,
                            Components.Limbs,
                            %{
                              "left hand"  => %{"type" => "hand", "items" => [staff]},
                              "right hand" => %{"type" => "hand", "items" => [staff]}
                            })
    end

    { :ok, %{character: character, club: club, sword: sword, staff: staff, gloves: gloves} }
  end

  @tag character: "open-handed"
  test "an entity with two hands can wield a one-handed weapon", data do
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"type" => "hand", "items" => []},
                                                         "right hand" => %{"type" => "hand", "items" => []}
                                                       }

    Components.Limbs.equip(data[:character], data[:club])

    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"type" => "hand", "items" => [data[:club]]},
                                                         "right hand" => %{"type" => "hand", "items" => []}
                                                        }
  end

  @tag character: "open-handed"
  test "an entity with two hands can wield two one-handed weapons", data do
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"type" => "hand", "items" => []},
                                                         "right hand" => %{"type" => "hand", "items" => []}
                                                       }
  
    Components.Limbs.equip(data[:character], data[:club])
    Components.Limbs.equip(data[:character], data[:sword])
  
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"type" => "hand", "items" => [data[:club]]},
                                                         "right hand" => %{"type" => "hand", "items" => [data[:sword]]}
                                                        }
  end
  
  @tag character: "open-handed"
  test "an entity with two hands can wield a two-handed weapon", data do
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"type" => "hand", "items" => []},
                                                         "right hand" => %{"type" => "hand", "items" => []}
                                                       }
  
    Components.Limbs.equip(data[:character], data[:staff])
  
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"type" => "hand", "items" => [data[:staff]]},
                                                         "right hand" => %{"type" => "hand", "items" => [data[:staff]]}
                                                        }
  end
  
  @tag character: "wielding one-handed"
  test "an entity wielding a one-handed weapon will remove it to wear a two handed weapon", data do
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"type" => "hand", "items" => [data[:sword]]},
                                                         "right hand" => %{"type" => "hand", "items" => []}
                                                       }
  
    Components.Limbs.equip(data[:character], data[:staff])
  
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"type" => "hand", "items" => [data[:staff]]},
                                                         "right hand" => %{"type" => "hand", "items" => [data[:staff]]}
                                                        }
  end
  
  @tag character: "dual-wielding"
  test "an entity wielding two one-handed weapons will remove them to wear a two handed weapon", data do
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"type" => "hand", "items" => [data[:sword]]},
                                                         "right hand" => %{"type" => "hand", "items" => [data[:club]]}
                                                       }
  
    Components.Limbs.equip(data[:character], data[:staff])
  
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"type" => "hand", "items" => [data[:staff]]},
                                                         "right hand" => %{"type" => "hand", "items" => [data[:staff]]}
                                                        }
  end
  
  @tag character: "wielding one-handed"
  test "an entity wielding a one-handed weapon will not remove it to wear hand armour", data do
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"type" => "hand", "items" => [data[:sword]]},
                                                         "right hand" => %{"type" => "hand", "items" => []}
                                                       }
  
    Components.Limbs.equip(data[:character], data[:gloves])
  
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"type" => "hand", "items" => [data[:gloves],data[:sword]]},
                                                         "right hand" => %{"type" => "hand", "items" => [data[:gloves]]}
                                                        }
  end
  
  @tag character: "dual-wielding"
  test "an entity wielding two one-handed weapons will not remove them to wear hand armour", data do
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"type" => "hand", "items" => [data[:sword]]},
                                                         "right hand" => %{"type" => "hand", "items" => [data[:club]]}
                                                       }
  
    Components.Limbs.equip(data[:character], data[:gloves])
  
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"type" => "hand", "items" => [data[:gloves],data[:sword]]},
                                                         "right hand" => %{"type" => "hand", "items" => [data[:gloves],data[:club]]}
                                                        }
  end
  
  @tag character: "wielding two-handed"
  test "an entity wielding a two-handed weapon will not remove them to wear hand armour", data do
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"type" => "hand", "items" => [data[:staff]]},
                                                         "right hand" => %{"type" => "hand", "items" => [data[:staff]]}
                                                       }
  
    Components.Limbs.equip(data[:character], data[:gloves])
  
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"type" => "hand", "items" => [data[:gloves],data[:staff]]},
                                                         "right hand" => %{"type" => "hand", "items" => [data[:gloves],data[:staff]]}
                                                        }
  end
  
  @tag character: "wielding two-handed"
  test "an entity wielding a two-handed weapon will remove it to wield a one-handed weapon", data do
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"type" => "hand", "items" => [data[:staff]]},
                                                         "right hand" => %{"type" => "hand", "items" => [data[:staff]]}
                                                       }
  
    Components.Limbs.equip(data[:character], data[:sword])
  
    assert Components.Limbs.value(data[:character]) == %{
                                                         "left hand"  => %{"type" => "hand", "items" => [data[:sword]]},
                                                         "right hand" => %{"type" => "hand", "items" => []}
                                                        }
  end
end
