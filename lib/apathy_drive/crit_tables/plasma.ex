defmodule ApathyDrive.CritTables.Plasma do

  def name do
    "plasma"
  end

  def damage_type do
    "magical"
  end

  def crits do
    %{
      "A"=>[
        %{
          :user_message=>"Your attack causes tiny bubbles of plasma to crackle about {{target}}.",
          :target_message=>"{{user}}'s attack causes tiny bubbles of plasma to crackle about you.",
          :spectator_message=>"{{user}}'s attack causes tiny bubbles of plasma to crackle about {{target}}.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"{{target}} is spun about as plasma from your attack strikes {{target:him/her/it}}.",
          :target_message=>"You are spun about as plasma from {{user}}'s attack strikes you.",
          :spectator_message=>"{{target}} is spun about as plasma from {{user}}'s attack strikes {{target:him/her/it}}.",
          :effects=>%{
            :stun=>1.0,
            :damage=>0.08
          }
        },
        %{
          :user_message=>"Your plasma blast unbalances {{target}}, disorienting {{target:him/her/it}}.",
          :target_message=>"{{user}}'s plasma blast unbalances you, disorienting you.",
          :spectator_message=>"{{user}}'s plasma blast unbalances {{target}}, disorienting {{target:him/her/it}}.",
          :effects=>%{
            :damage=>0.17,
            :stun=>2.0
          }
        },
        %{
          :user_message=>"Your attack showers {{target}} with burning plasma.",
          :target_message=>"{{user}}'s attack showers you with burning plasma.",
          :spectator_message=>"{{user}}'s attack showers {{target}} with burning plasma.",
          :effects=>%{
            :damage=>0.16
          }
        },
        %{
          :user_message=>"{{target}} is distracted by the rain of plasma from your attack.",
          :target_message=>"You are distracted by the rain of plasma from {{user}}'s attack.",
          :spectator_message=>"{{target}} is distracted by the rain of plasma from {{user}}'s attack.",
          :effects=>%{
            :damage=>0.23,
            :stun=>2.0
          }
        },
        %{
          :user_message=>"A crackling blast of plasma from your attack strikes {{target}}.",
          :target_message=>"A crackling blast of plasma from {{user}}'s attack strikes you.",
          :spectator_message=>"A crackling blast of plasma from {{user}}'s attack strikes {{target}}.",
          :effects=>%{
            :stun=>1.0,
            :damage=>0.23
          }
        },
        %{
          :user_message=>"You deliver a powerful blow to {{target}} with your plasma attack.",
          :target_message=>"{{user}} delivers a powerful blow to you with {{user:his/her/its}} plasma attack.",
          :spectator_message=>"{{user}} delivers a powerful blow to {{target}} with {{user:his/her/its}} plasma attack.",
          :effects=>%{
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>1.0
              }
            ],
            :damage=>0.23
          }
        },
        %{
          :user_message=>"Your powerful plasma blast shatters {{target}}'s arm, rendering it useless.",
          :target_message=>"{{user}}'s powerful plasma blast shatters your arm, rendering it useless.",
          :spectator_message=>"{{user}}'s powerful plasma blast shatters {{target}}'s arm, rendering it useless.",
          :effects=>%{
            :damage=>0.38,
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>3.0
              }
            ]
          }
        },
        %{
          :user_message=>"Plasma from your attack strikes {{target}} in the back.",
          :target_message=>"Plasma from {{user}}'s attack strikes you in the back.",
          :spectator_message=>"Plasma from {{user}}'s attack strikes {{target}} in the back.",
          :effects=>%{
            :damage=>0.16,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-5,
                :duration=>20.0
              }
            ],
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>1.0
              }
            ]
          }
        },
        %{
          :user_message=>"You deliver a weak plasma strike to {{target}}'s abdomen.",
          :target_message=>"{{user}} delivers a weak plasma strike to your abdomen.",
          :spectator_message=>"{{user}} delivers a weak plasma strike to {{target}}'s abdomen.",
          :effects=>%{
            :damage=>0.31,
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>1.0
              },
              %{
                :skill=>"block",
                :amount=>-100,
                :duration=>1.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} is knocked down by the brutality of your plasma blast.",
          :target_message=>"You are knocked down by the brutality of {{user}}'s plasma blast.",
          :spectator_message=>"{{target}} is knocked down by the brutality of {{user}}'s plasma blast.",
          :effects=>%{
            :stun=>2.0,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-25,
                :duration=>20.0
              }
            ],
            :damage=>0.23
          }
        },
        %{
          :user_message=>"Your plamsa attack crushes {{target}}'s hip.",
          :target_message=>"{{user}}'s plamsa attack crushes your hip.",
          :spectator_message=>"{{user}}'s plamsa attack crushes {{target}}'s hip.",
          :effects=>%{
            :stun=>3.0,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-25,
                :duration=>20.0
              }
            ],
            :damage=>0.38
          }
        },
        %{
          :user_message=>"Your plasma strike severely burns {{target}}'s head and neck.",
          :target_message=>"{{user}}'s plasma strike severely burns your head and neck.",
          :spectator_message=>"{{user}}'s plasma strike severely burns {{target}}'s head and neck.",
          :effects=>%{
            :damage=>0.38,
            :stun=>10.0
          }
        },
        %{
          :user_message=>"You deliver a powerful plasma strike to {{target}}'s head, severely injuring {{target:him/her/it}}.",
          :target_message=>"{{user}} delivers a powerful plasma strike to your head, severely injuring you.",
          :spectator_message=>"{{user}} delivers a powerful plasma strike to {{target}}'s head, severely injuring {{target:him/her/it}}.",
          :effects=>%{
            :damage=>0.46,
            :stun=>12.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>6.0
              },
              %{
                :skill=>"block",
                :amount=>-100,
                :duration=>6.0
              }
            ]
          }
        }
      ],
      "B"=>[
        %{
          :user_message=>"Your attack shines of bright plasma momentarily, then fizzles out.",
          :target_message=>"{{user}}'s attack shines of bright plasma momentarily, then fizzles out.",
          :spectator_message=>"{{user}}'s attack shines of bright plasma momentarily, then fizzles out.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"Your plasma blast unbalances {{target}}.",
          :target_message=>"{{user}}'s plasma blast unbalances you.",
          :spectator_message=>"{{user}}'s plasma blast unbalances {{target}}.",
          :effects=>%{
            :damage=>0.08,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"Your plasma attack inflicts minor burns on {{target}}.",
          :target_message=>"{{user}}'s plasma attack inflicts minor burns on you.",
          :spectator_message=>"{{user}}'s plasma attack inflicts minor burns on {{target}}.",
          :effects=>%{
            :stun=>1.0,
            :damage=>0.16,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-10,
                :duration=>1.0
              },
              %{
                :skill=>"block",
                :amount=>-10,
                :duration=>1.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} is blasted backward by the force of your plasma attack.",
          :target_message=>"You are blasted backward by the force of {{user}}'s plasma attack.",
          :spectator_message=>"{{target}} is blasted backward by the force of {{user}}'s plasma attack.",
          :effects=>%{
            :damage=>0.14,
            :stun=>2.0
          }
        },
        %{
          :user_message=>"You stagger {{target}} with the strength of your plasma attack.",
          :target_message=>"{{user}} staggers you with the strength of {{user:his/her/its}} plasma attack.",
          :spectator_message=>"{{user}} staggers {{target}} with the strength of {{user:his/her/its}} plasma attack.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>6.0
            },
            :damage=>0.23,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"You deliver a plasma blast to {{target}}'s back, spinning {{target:him/her/it}} about.",
          :target_message=>"{{user}} delivers a plasma blast to your back, spinning you about.",
          :spectator_message=>"{{user}} delivers a plasma blast to {{target}}'s back, spinning {{target:him/her/it}} about.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>5.0
            },
            :damage=>0.23
          }
        },
        %{
          :user_message=>"Your plasma attack badly burns {{target}}.",
          :target_message=>"{{user}}'s plasma attack badly burns you.",
          :spectator_message=>"{{user}}'s plasma attack badly burns {{target}}.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>6.0
            },
            :damage=>0.23,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>1.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-5,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"Plasma from your attack shatters {{target}}'s shoulder, setting that portion of his body aflame!",
          :target_message=>"Plasma from {{user}}'s attack shatters your shoulder, setting that portion of his body aflame!",
          :spectator_message=>"Plasma from {{user}}'s attack shatters {{target}}'s shoulder, setting that portion of his body aflame!",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.07,
              :duration=>5.0
            },
            :damage=>0.31,
            :stun=>3.0,
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ]
          }
        },
        %{
          :user_message=>"You deliver a solid plasma strike to {{target}}'s back.",
          :target_message=>"{{user}} delivers a solid plasma strike to your back.",
          :spectator_message=>"{{user}} delivers a solid plasma strike to {{target}}'s back.",
          :effects=>%{
            :damage=>0.22,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>1.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-5,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your plasma blast strikes {{target}} on {{target:his/her/its}} feet, toppling {{target:him/her/it}}.",
          :target_message=>"{{user}}'s plasma blast strikes you on your feet, toppling you.",
          :spectator_message=>"{{user}}'s plasma blast strikes {{target}} on {{target:his/her/its}} feet, toppling {{target:him/her/it}}.",
          :effects=>%{
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>2.0
              },
              %{
                :skill=>"block",
                :amount=>-100,
                :duration=>2.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.04,
              :duration=>5.0
            },
            :damage=>0.23
          }
        },
        %{
          :user_message=>"You deliver a fierce plasma strike to {{target}}'s calf, boiling flesh and muscle.",
          :target_message=>"{{user}} delivers a fierce plasma strike to your calf, boiling flesh and muscle.",
          :spectator_message=>"{{user}} delivers a fierce plasma strike to {{target}}'s calf, boiling flesh and muscle.",
          :effects=>%{
            :stun=>3.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-50,
                :duration=>10.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-25,
                :duration=>20.0
              }
            ],
            :damage=>0.38
          }
        },
        %{
          :user_message=>"Your attack strikes {{target}} in the temple, severely burning the side of {{target:his/her/its}} face and robbing {{target:him/her/it}} of hair.",
          :target_message=>"{{user}}'s attack strikes you in the temple, severely burning the side of your face and robbing you of hair.",
          :spectator_message=>"{{user}}'s attack strikes {{target}} in the temple, severely burning the side of {{target:his/her/its}} face and robbing {{target:him/her/it}} of hair.",
          :effects=>%{
            :damage=>0.46,
            :stun=>8.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-60,
                :duration=>8.0
              },
              %{
                :skill=>"block",
                :amount=>-60,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your plasma blast burns {{target}}'s face and fills {{target:his/her/its}} lungs with smoke.",
          :target_message=>"{{user}}'s plasma blast burns your face and fills your lungs with smoke.",
          :spectator_message=>"{{user}}'s plasma blast burns {{target}}'s face and fills {{target:his/her/its}} lungs with smoke.",
          :effects=>%{
            :damage=>0.46,
            :stun=>12.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>12.0
              }
            ]
          }
        },
        %{
          :user_message=>"You cause severe third degree burns on most of {{target}}'s body with your attack.",
          :target_message=>"{{user}} causes severe third degree burns on most of your body with {{user:his/her/its}} attack.",
          :spectator_message=>"{{user}} causes severe third degree burns on most of {{target}}'s body with {{user:his/her/its}} attack.",
          :effects=>%{
            :damage=>0.61,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-100,
                :duration=>10.0
              }
            ]
          }
        }
      ],
      "C"=>[
        %{
          :user_message=>"{{target}} is unbalanced by your plasma attack.",
          :target_message=>"You are unbalanced by {{user}}'s plasma attack.",
          :spectator_message=>"{{target}} is unbalanced by {{user}}'s plasma attack.",
          :effects=>%{
            :damage=>0.08,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"Plasma from your attack crackles with the energy of heat and electricity as it strikes {{target}}.",
          :target_message=>"Plasma from {{user}}'s attack crackles with the energy of heat and electricity as it strikes you.",
          :spectator_message=>"Plasma from {{user}}'s attack crackles with the energy of heat and electricity as it strikes {{target}}.",
          :effects=>%{
            :damage=>0.13,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>1.0
              },
              %{
                :skill=>"block",
                :amount=>-100,
                :duration=>1.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} staggers under the impact of your plasma blast.",
          :target_message=>"You stagger under the impact of {{user}}'s plasma blast.",
          :spectator_message=>"{{target}} staggers under the impact of {{user}}'s plasma blast.",
          :effects=>%{
            :damage=>0.23,
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>7.0
            },
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-40,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}}'s flesh sizzles as plasma from your attack strikes {{target:him/her/it}}.",
          :target_message=>"Your flesh sizzles as plasma from {{user}}'s attack strikes you.",
          :spectator_message=>"{{target}}'s flesh sizzles as plasma from {{user}}'s attack strikes {{target:him/her/it}}.",
          :effects=>%{
            :damage=>0.23,
            :stun=>1.0,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-50,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} is thrown violently back as plasma from your attack strikes {{target:him/her/it}}.",
          :target_message=>"You are thrown violently back as plasma from {{user}}'s attack strikes you.",
          :spectator_message=>"{{target}} is thrown violently back as plasma from {{user}}'s attack strikes {{target:him/her/it}}.",
          :effects=>%{
            :stun=>2.0,
            :damage=>0.31
          }
        },
        %{
          :user_message=>"Plasma from your attack strikes {{target}} in the leg, setting it aflame!",
          :target_message=>"Plasma from {{user}}'s attack strikes you in the leg, setting it aflame!",
          :spectator_message=>"Plasma from {{user}}'s attack strikes {{target}} in the leg, setting it aflame!",
          :effects=>%{
            :stun=>2.0,
            :damage=>0.23
          }
        },
        %{
          :user_message=>"You deliver a fierce plasma strike to {{target}}'s chest, rending both of {{target:his/her/its}} arms!",
          :target_message=>"{{user}} delivers a fierce plasma strike to your chest, rending both of your arms!",
          :spectator_message=>"{{user}} delivers a fierce plasma strike to {{target}}'s chest, rending both of {{target:his/her/its}} arms!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              },
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :stun=>3.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-90,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"Both of {{target}}'s arms are broken by your fierce plasma strike!",
          :target_message=>"Both of your arms are broken by {{user}}'s fierce plasma strike!",
          :spectator_message=>"Both of {{target}}'s arms are broken by {{user}}'s fierce plasma strike!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              },
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :stun=>3.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-90,
                :duration=>5.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your fierce plasma strike knocks {{target}} forcefully to the ground.",
          :target_message=>"{{user}}'s fierce plasma strike knocks you forcefully to the ground.",
          :spectator_message=>"{{user}}'s fierce plasma strike knocks {{target}} forcefully to the ground.",
          :effects=>%{
            :stun=>2.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-15,
                :duration=>5.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.04,
              :duration=>5.0
            },
            :damage=>0.23
          }
        },
        %{
          :user_message=>"A fierce blast of plasma from your attack breaks {{target}}'s thigh.",
          :target_message=>"A fierce blast of plasma from {{user}}'s attack breaks your thigh.",
          :spectator_message=>"A fierce blast of plasma from {{user}}'s attack breaks {{target}}'s thigh.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"leg"
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-40,
                :duration=>10.0
              }
            ],
            :stun=>3.0,
            :damage=>0.31
          }
        },
        %{
          :user_message=>"Your plasma blast twists {{target}} about so violently that {{target:his/her/its}} hip is shattered!",
          :target_message=>"{{user}}'s plasma blast twists you about so violently that your hip is shattered!",
          :spectator_message=>"{{user}}'s plasma blast twists {{target}} about so violently that {{target:his/her/its}} hip is shattered!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"leg"
              }
            ],
            :damage_over_time=>%{
              :damage=>0.07,
              :duration=>6.0
            },
            :damage=>0.38,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-60,
                :duration=>5.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}}'s feet are engulfed by a plasma blast from your attack, rendering {{target}} temporarily immobile.",
          :target_message=>"Your feet are engulfed by a plasma blast from {{user}}'s attack, rendering you temporarily immobile.",
          :spectator_message=>"{{target}}'s feet are engulfed by a plasma blast from {{user}}'s attack, rendering {{target}} temporarily immobile.",
          :effects=>%{
            :stun=>9.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>9.0
              },
              %{
                :skill=>"blcok",
                :amount=>-60,
                :duration=>9.0
              },
              %{
                :skill=>"attack",
                :amount=>-60,
                :duration=>10.0
              }
            ],
            :damage=>0.38
          }
        },
        %{
          :user_message=>"A brilliant ball of plasma from your attack crackles through {{target}}'s eye. {{target:his/her/its}} brain is vaporized instantly, and {{target:his/her/its}} head explodes!",
          :target_message=>"A brilliant ball of plasma from {{user}}'s attack crackles through your eye. Your brain is vaporized instantly, and your head explodes!",
          :spectator_message=>"A brilliant ball of plasma from {{user}}'s attack crackles through {{target}}'s eye. {{target:his/her/its}} brain is vaporized instantly, and {{target:his/her/its}} head explodes!",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "D"=>[
        %{
          :user_message=>"{{target}} is put on the defensive as your plasma blast unbalances {{target:him/her/it}}.",
          :target_message=>"You are put on the defensive as {{user}}'s plasma blast unbalances you.",
          :spectator_message=>"{{target}} is put on the defensive as {{user}}'s plasma blast unbalances {{target:him/her/it}}.",
          :effects=>%{
            :damage=>0.13,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"Your plasma blast glances across the side of {{target}}'s face, singeing it.",
          :target_message=>"{{user}}'s plasma blast glances across the side of your face, singeing it.",
          :spectator_message=>"{{user}}'s plasma blast glances across the side of {{target}}'s face, singeing it.",
          :effects=>%{
            :damage=>0.11,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"A burst of hot plasma from your attack explodes on {{target}}'s chest, throwing {{target:him/her/it}} backward.",
          :target_message=>"A burst of hot plasma from {{user}}'s attack explodes on your chest, throwing you backward.",
          :spectator_message=>"A burst of hot plasma from {{user}}'s attack explodes on {{target}}'s chest, throwing {{target:him/her/it}} backward.",
          :effects=>%{
            :damage=>0.21,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>5.0
            },
            :stun=>1.0
          }
        },
        %{
          :user_message=>"Your plasma blast strikes {{target}} visciously on the side, spinning {{target:him/her/it}} about.",
          :target_message=>"{{user}}'s plasma blast strikes you visciously on the side, spinning you about.",
          :spectator_message=>"{{user}}'s plasma blast strikes {{target}} visciously on the side, spinning {{target:him/her/it}} about.",
          :effects=>%{
            :stun=>1.0,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>5.0
            },
            :damage=>0.32
          }
        },
        %{
          :user_message=>"Superheated plasma from your attack badly burns {{target}} on the chest and arms.",
          :target_message=>"Superheated plasma from {{user}}'s attack badly burns you on the chest and arms.",
          :spectator_message=>"Superheated plasma from {{user}}'s attack badly burns {{target}} on the chest and arms.",
          :effects=>%{
            :stun=>1.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-30,
                :duration=>10.0
              }
            ],
            :damage=>0.2
          }
        },
        %{
          :user_message=>"An incredibly hot blast of plasma strikes {{target}} in the arm, instantly cooking the muscle and renedering it without function.",
          :target_message=>"An incredibly hot blast of plasma strikes you in the arm, instantly cooking the muscle and renedering it without function.",
          :spectator_message=>"An incredibly hot blast of plasma strikes {{target}} in the arm, instantly cooking the muscle and renedering it without function.",
          :effects=>%{
            :damage=>0.28,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"A brilliant ball of plasma impacts {{target}}'s skull, instantly vaporizing {{target:his/her/its}} brain. The cooked brain matter escapes violently from {{target}}'s eardrums in a brilliant display!",
          :target_message=>"A brilliant ball of plasma impacts your skull, instantly vaporizing your brain. The cooked brain matter escapes violently from your eardrums in a brilliant display!",
          :spectator_message=>"A brilliant ball of plasma impacts {{target}}'s skull, instantly vaporizing {{target:his/her/its}} brain. The cooked brain matter escapes violently from {{target}}'s eardrums in a brilliant display!",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"You deliver a focused plasma attack to the center of {{target}}'s chest, knocking {{target:him/her/it}} to the ground.",
          :target_message=>"{{user}} delivers a focused plasma attack to the center of your chest, knocking you to the ground.",
          :spectator_message=>"{{user}} delivers a focused plasma attack to the center of {{target}}'s chest, knocking {{target:him/her/it}} to the ground.",
          :effects=>%{
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>2.0
              }
            ],
            :damage=>0.3
          }
        },
        %{
          :user_message=>"The force of your plasma blast breaks {{target}}'s arm!",
          :target_message=>"The force of {{user}}'s plasma blast breaks your arm!",
          :spectator_message=>"The force of {{user}}'s plasma blast breaks {{target}}'s arm!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :stun=>2.0,
            :damage=>0.2,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-30,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your plasma blast strikes {{target}} in the gut. {{target:he/she/it}} coughs up blood and other inappropriate secretions.",
          :target_message=>"{{user}}'s plasma blast strikes you in the gut. You cough up blood and other inappropriate secretions.",
          :spectator_message=>"{{user}}'s plasma blast strikes {{target}} in the gut. {{target:he/she/it}} coughs up blood and other inappropriate secretions.",
          :effects=>%{
            :stun=>4.0,
            :damage=>0.4
          }
        },
        %{
          :user_message=>"{{target}}'s jaw is DESTROYED as your plasma blast impacts {{target:his/her/its}} face.  Red hot sparks and bone fragments shoot in all directions!",
          :target_message=>"Your jaw is DESTROYED as {{user}}'s plasma blast impacts your face.  Red hot sparks and bone fragments shoot in all directions!",
          :spectator_message=>"{{target}}'s jaw is DESTROYED as {{user}}'s plasma blast impacts {{target:his/her/its}} face.  Red hot sparks and bone fragments shoot in all directions!",
          :effects=>%{
            :damage=>0.7,
            :stun=>3.0,
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-25,
                :duration=>10.0
              },
              %{
                :stat=>"agility",
                :amount=>-30,
                :duration=>10.0
              }
            ],
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-80,
                :duration=>3.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} receive severe internal burns while inhaling white-hot plasma from your attack!",
          :target_message=>"You receives severe internal burns while inhaling white-hot plasma from {{user}}'s attack!",
          :spectator_message=>"{{target}} receives severe internal burns while inhaling white-hot plasma from {{user}}'s attack!",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.08,
              :duration=>10.0
            },
            :damage=>0.3,
            :stun=>2.0
          }
        },
        %{
          :user_message=>"A brilliant ball of electric plasma from your attack impacts {{target}} squarely in the abdomen. {{target:he/she/it}} explode violently, showering steaming viscera on all present.",
          :target_message=>"A brilliant ball of electric plasma from {{user}}'s attack impacts you squarely in the abdomen. You explodes violently, showering steaming viscera on all present.",
          :spectator_message=>"A brilliant ball of electric plasma from {{user}}'s attack impacts {{target}} squarely in the abdomen. {{target:he/she/it}} explodes violently, showering steaming viscera on all present.",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "E"=>[
        %{
          :user_message=>"ZAP!  A plasma ball impacts {{target}}'s chest.",
          :target_message=>"ZAP!  A plasma ball impacts your chest.",
          :spectator_message=>"ZAP!  A plasma ball impacts {{target}}'s chest.",
          :effects=>%{
            :stun=>1.0,
            :damage=>0.08
          }
        },
        %{
          :user_message=>"Your powerful plasma blast sends {{target}} reeling.",
          :target_message=>"{{user}}'s powerful plasma blast sends you reeling.",
          :spectator_message=>"{{user}}'s powerful plasma blast sends {{target}} reeling.",
          :effects=>%{
            :stun=>1.0,
            :damage=>0.15
          }
        },
        %{
          :user_message=>"A brilliant ball of plasma strikes {{target}} in the leg.",
          :target_message=>"A brilliant ball of plasma strikes you in the leg.",
          :spectator_message=>"A brilliant ball of plasma strikes {{target}} in the leg.",
          :effects=>%{
            :stun=>1.0,
            :damage=>0.29
          }
        },
        %{
          :user_message=>"A powerful blast of plasma from your attack forces {{target}} to drop all that {{target:he/she/it}} are carrying.",
          :target_message=>"A powerful blast of plasma from {{user}}'s attack forces you to drop all that you ares carrying.",
          :spectator_message=>"A powerful blast of plasma from {{user}}'s attack forces {{target}} to drop all that {{target:he/she/it}} ares carrying.",
          :effects=>%{
            :stun=>1.0,
            :damage=>0.33,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>1.0
              }
            ]
          }
        },
        %{
          :user_message=>"The impact of your plasma blast lifts {{target}} bodily from the ground, throwing {{target:him/her/it}} back several feet.",
          :target_message=>"The impact of {{user}}'s plasma blast lifts you bodily from the ground, throwing you back several feet.",
          :spectator_message=>"The impact of {{user}}'s plasma blast lifts {{target}} bodily from the ground, throwing {{target:him/her/it}} back several feet.",
          :effects=>%{
            :stun=>2.0,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>5.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-20,
                :duration=>10.0
              }
            ],
            :damage=>0.33
          }
        },
        %{
          :user_message=>"A precise strike delivered by you to {{target}}'s forehead knocks {{target:him/her/it}} over violently.",
          :target_message=>"A precise strike delivered by {{user}} to your forehead knocks you over violently.",
          :spectator_message=>"A precise strike delivered by {{user}} to {{target}}'s forehead knocks {{target:him/her/it}} over violently.",
          :effects=>%{
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-35,
                :duration=>10.0
              }
            ],
            :damage=>0.33
          }
        },
        %{
          :user_message=>"You deliver direct and surgically precise plasma strike to {{target}}'s head.  It explodes violently, stinging all present with red-hot fragments of flesh and bone.",
          :target_message=>"{{user}} delivers direct and surgically precise plasma strike to your head.  It explodes violently, stinging all present with red-hot fragments of flesh and bone.",
          :spectator_message=>"{{user}} delivers direct and surgically precise plasma strike to {{target}}'s head.  It explodes violently, stinging all present with red-hot fragments of flesh and bone.",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"You deliver a powerful plasma strike to {{target}}'s shoulder, rendering it useless.",
          :target_message=>"{{user}} delivers a powerful plasma strike to your shoulder, rendering it useless.",
          :spectator_message=>"{{user}} delivers a powerful plasma strike to {{target}}'s shoulder, rendering it useless.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :stun=>1.0
          }
        },
        %{
          :user_message=>"{{target}} are thrown violently to the ground when your plasma strike hits {{target:him/her/it}} squarely in the chest.",
          :target_message=>"You ares thrown violently to the ground when {{user}}'s plasma strike hits you squarely in the chest.",
          :spectator_message=>"{{target}} ares thrown violently to the ground when {{user}}'s plasma strike hits {{target:him/her/it}} squarely in the chest.",
          :effects=>%{
            :damage=>0.41,
            :stun=>10.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-45,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your plasma attack strikes {{target}} in the back, setting {{target:him/her/it}} temporarily aflame!",
          :target_message=>"{{user}}'s plasma attack strikes you in the back, setting you temporarily aflame!",
          :spectator_message=>"{{user}}'s plasma attack strikes {{target}} in the back, setting {{target:him/her/it}} temporarily aflame!",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>6.0
            },
            :damage=>0.41
          }
        },
        %{
          :user_message=>"Your attack rains white-hot plasma on {{target}}'s face, flaying the flesh from the bone!",
          :target_message=>"{{user}}'s attack rains white-hot plasma on your face, flaying the flesh from the bone!",
          :spectator_message=>"{{user}}'s attack rains white-hot plasma on {{target}}'s face, flaying the flesh from the bone!",
          :effects=>%{
            :stun=>6.0,
            :damage=>0.43,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>6.0
            }
          }
        },
        %{
          :user_message=>"A HUGE plasma blast strikes {{target}} in the chest.  The incredibly intense heat cooks {{target}} almost instantly, and {{target:he/she/it}} fall to the ground, lifeless.",
          :target_message=>"A HUGE plasma blast strikes you in the chest.  The incredibly intense heat cooks you almost instantly, and you falls to the ground, lifeless.",
          :spectator_message=>"A HUGE plasma blast strikes {{target}} in the chest.  The incredibly intense heat cooks {{target}} almost instantly, and {{target:he/she/it}} falls to the ground, lifeless.",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"Much to {{target}}'s chagrin, your plasma attack instantly melts {{target:him/her/it}} into a puddle of steaming goo.  Surpirsingly, a janitor appears out of nowhere to mop up the mess.",
          :target_message=>"Much to your chagrin, {{user}}'s plasma attack instantly melts you into a puddle of steaming goo.  Surpirsingly, a janitor appears out of nowhere to mop up the mess.",
          :spectator_message=>"Much to {{target}}'s chagrin, {{user}}'s plasma attack instantly melts {{target:him/her/it}} into a puddle of steaming goo.  Surpirsingly, a janitor appears out of nowhere to mop up the mess.",
          :effects=>%{
            :kill=>true
          }
        }
      ]
    }
  end

end
