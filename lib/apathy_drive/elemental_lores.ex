defmodule ApathyDrive.ElementalLores do
  @lores %{
    "earth" => %{
      name: "earth",
      level: 1,
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
      level: 5,
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
      level: 10,
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
      level: 15,
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
      level: 20,
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
      level: 25,
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
      level: 30,
      damage_types: [
        %{
          kind: "magical",
          damage_type: "Stress"
        },
        %{
          kind: "magical",
          damage_type: "Disruption"
        }
      ]
    },
    "plasma" => %{
      name: "plasma",
      level: 35,
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
      level: 40,
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
      level: 50,
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
end
