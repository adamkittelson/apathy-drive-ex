defmodule ApathyDrive.ElementalLores do
  alias ApathyDrive.Repo

  @lores %{
    "earth" => %{
      name: "earth",
      level: 3,
      damage_types: [
        %{
          kind: "magical",
          damage_type: "Crushing"
        },
        %{
          kind: "magical",
          damage_type: "Impact"
        }
      ]
    },
    "ice" => %{
      name: "ice",
      level: 6,
      damage_types: [
        %{
          kind: "magical",
          damage_type: "Impact"
        },
        %{
          kind: "magical",
          damage_type: "Cold"
        }
      ]
    },
    "fire" => %{
      name: "fire",
      level: 9,
      damage_types: [
        %{
          kind: "magical",
          damage_type: "Fire"
        },
        %{
          kind: "magical",
          damage_type: "Stress"
        }
      ]
    },
    "wind" => %{
      name: "wind",
      level: 12,
      damage_types: [
        %{
          kind: "magical",
          damage_type: "Impact"
        },
        %{
          kind: "magical",
          damage_type: "Vacuum"
        }
      ]
    },
    "electricity" => %{
      name: "electricity",
      level: 15,
      damage_types: [
        %{
          kind: "magical",
          damage_type: "Electricity"
        },
        %{
          kind: "magical",
          damage_type: "Fire"
        },
        %{
          kind: "magical",
          damage_type: "Impact"
        }
      ]
    },
    "vacid" => %{
      name: "vacid",
      level: 18,
      damage_types: [
        %{
          kind: "magical",
          damage_type: "Cold"
        },
        %{
          kind: "magical",
          damage_type: "Impact"
        },
        %{
          kind: "magical",
          damage_type: "Stress"
        }
      ]
    },
    "chaos" => %{
      name: "chaos",
      level: 21,
      damage_types: [
        %{
          kind: "magical",
          damage_type: "Stress"
        },
        %{
          kind: "magical",
          damage_type: "Disruption"
        },
        %{
          kind: "magical",
          damage_type: "Fire"
        }
      ]
    },
    "plasma" => %{
      name: "plasma",
      level: 24,
      damage_types: [
        %{
          kind: "magical",
          damage_type: "Plasma"
        },
        %{
          kind: "magical",
          damage_type: "Fire"
        },
        %{
          kind: "magical",
          damage_type: "Electricity"
        }
      ]
    },
    "aether" => %{
      name: "aether",
      level: 27,
      damage_types: [
        %{
          kind: "magical",
          damage_type: "Aether"
        },
        %{
          kind: "magical",
          damage_type: "Stress"
        }
      ]
    },
    "nexus" => %{
      name: "nexus",
      level: 30,
      damage_types: [
        %{
          kind: "magical",
          damage_type: "Disruption"
        },
        %{
          kind: "magical",
          damage_type: "Aether"
        },
        %{
          kind: "magical",
          damage_type: "Stress"
        }
      ]
    }
  }

  def lores do
    @lores
  end

  def lore(lore) do
    if lore = @lores[lore] do
      update_in(lore.damage_types, fn damage_types ->
        Enum.map(damage_types, fn damage_type ->
          Map.put(
            damage_type,
            :damage_type_id,
            Repo.get_by(ApathyDrive.DamageType, name: damage_type.damage_type).id
          )
        end)
      end)
    end
  end
end
