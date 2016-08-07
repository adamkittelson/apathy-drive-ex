defmodule ApathyDrive.CritTables.Impact do

  def name do
    "impact"
  end

  def damage_type do
    "physical"
  end

  def crits do
    %{
      "A"=>[
        %{
          :user_message=>"{{target}} is pushed roughly, but no damage is done.",
          :target_message=>"You are pushed roughly, but no damage is done.",
          :spectator_message=>"{{target}} is pushed roughly, but no damage is done.",
          :effects=>%{
            damage: 0.0
          }
        },
        %{
          :user_message=>"You bruise {{target}} with your attack.",
          :target_message=>"{{user}} bruises you with {{user:his/her/its}} attack.",
          :spectator_message=>"{{user}} bruises {{target}} with {{user:his/her/its}} attack.",
          :effects=>%{
            :damage=>1.01
          }
        },
        %{
          :user_message=>"You smash into {{target}} painfully.",
          :target_message=>"{{user}} smashes into you painfully.",
          :spectator_message=>"{{user}} smashes into {{target}} painfully.",
          :effects=>%{
            :damage=>0.02
          }
        },
        %{
          :user_message=>"You knock {{target}} to the ground.",
          :target_message=>"{{user}} knocks you to the ground.",
          :spectator_message=>"{{user}} knocks {{target}} to the ground.",
          :effects=>%{
            :damage=>0.04,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-5,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You slam into {{target}} forcefully.",
          :target_message=>"{{user}} slams into you forcefully.",
          :spectator_message=>"{{user}} slams into {{target}} forcefully.",
          :effects=>%{
            :damage=>0.05,
            :skill_mod=>[
              %{
                :skill=>"attack",
                :amount=>-5,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You knock the wind from {{target}}'s lungs.",
          :target_message=>"{{user}} knocks the wind from your lungs.",
          :spectator_message=>"{{user}} knocks the wind from {{target}}'s lungs.",
          :effects=>%{
            :damage=>0.06,
            :damage_over_time=>%{
              :damage=>0.01,
              :duration=>3.0
            }
          }
        },
        %{
          :user_message=>"You crush {{target}}'s chest with your attack.",
          :target_message=>"{{user}} crushes your chest with {{user:his/her/its}} attack.",
          :spectator_message=>"{{user}} crushes {{target}}'s chest with {{user:his/her/its}} attack.",
          :effects=>%{
            :damage=>0.06,
            :skill_mod=>[
              %{
                :skill=>"attack",
                :amount=>-15,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"You hit {{target}} so hard that it leaves an imprint!",
          :target_message=>"{{user}} hits you so hard that it leaves an imprint!",
          :spectator_message=>"{{user}} hits {{target}} so hard that it leaves an imprint!",
          :effects=>%{
            :damage=>0.05,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"Your hit flattens {{target}}.",
          :target_message=>"{{user}}'s hit flattens you.",
          :spectator_message=>"{{user}}'s hit flattens {{target}}.",
          :effects=>%{
            :damage=>0.08,
            :skill_mod=>[
              %{
                :skill=>"attack",
                :amount=>-15,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You break {{target}}'s ribs with an astounding blow.",
          :target_message=>"{{user}} breaks your ribs with an astounding blow.",
          :spectator_message=>"{{user}} breaks {{target}}'s ribs with an astounding blow.",
          :effects=>%{
            :damage=>0.25,
            :stun=>5.0,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"{{target}} is knocked aside by your blow.",
          :target_message=>"You are knocked aside by {{user}}'s blow.",
          :spectator_message=>"{{target}} is knocked aside by {{user}}'s blow.",
          :effects=>%{
            :damage=>0.1,
            :skill_mod=>[
              %{
                :skill=>"attack",
                :amount=>-20,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} staggers dizzily from a punishing blow to the head.",
          :target_message=>"You stagger dizzily from a punishing blow to the head.",
          :spectator_message=>"{{target}} staggers dizzily from a punishing blow to the head.",
          :effects=>%{
            :damage=>0.12,
            :stun=>2.0
          }
        },
        %{
          :user_message=>"{{target}} spits out some loose teeth after a jarring hit to the mouth.",
          :target_message=>"You spit out some loose teeth after a jarring hit to the mouth.",
          :spectator_message=>"{{target}} spits out some loose teeth after a jarring hit to the mouth.",
          :effects=>%{
            :damage=>0.15,
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>3.0
            },
            :skill_mod=>[
              %{
                :skill=>"attack",
                :amount=>-10,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} falls down, as if crushed by a great weight.",
          :target_message=>"You fall down, as if crushed by a great weight.",
          :spectator_message=>"{{target}} falls down, as if crushed by a great weight.",
          :effects=>%{
            :damage=>0.17,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-20,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}}'s side caves in from the force of your hit.",
          :target_message=>"Your side caves in from the force of {{user}}'s hit.",
          :spectator_message=>"{{target}}'s side caves in from the force of {{user}}'s hit.",
          :effects=>%{
            :damage=>0.2,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>3.0
            },
            :stun=>2.0,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-15,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your blow sends {{target}} flying back 20 feet to a rough landing.",
          :target_message=>"{{user}}'s blow sends you flying back 20 feet to a rough landing.",
          :spectator_message=>"{{user}}'s blow sends {{target}} flying back 20 feet to a rough landing.",
          :effects=>%{
            :damage=>0.22,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>5.0
            },
            :stun=>3.0,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-20,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} feels like crushed jelly from your blow.",
          :target_message=>"You feel like crushed jelly from {{user}}'s blow.",
          :spectator_message=>"{{target}} feels like crushed jelly from {{user}}'s blow.",
          :effects=>%{
            :damage=>0.3,
            :stun=>5.0,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"A large weight crushes {{target}}'s arms and paralyzes them.",
          :target_message=>"A large weight crushes your arms and paralyzes them.",
          :spectator_message=>"A large weight crushes {{target}}'s arms and paralyzes them.",
          :effects=>%{
            :damage=>0.25,
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :stun=>3.0
          }
        }
      ],
      "B"=>[
        %{
          :user_message=>"{{target}} sways from the hit but no damage is done.",
          :target_message=>"You sway from the hit but no damage is done.",
          :spectator_message=>"{{target}} sways from the hit but no damage is done.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"{{target}} is unbalanced from your attack.",
          :target_message=>"You are unbalanced from {{user}}'s attack.",
          :spectator_message=>"{{target}} is unbalanced from {{user}}'s attack.",
          :effects=>%{
            :damage=>0.03,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"{{target}} is severely unbalanced by your attack.",
          :target_message=>"You are severely unbalanced by {{user}}'s attack.",
          :spectator_message=>"{{target}} is severely unbalanced by {{user}}'s attack.",
          :effects=>%{
            :damage=>0.07,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"{{target}} is severely unbalanced by your attack.",
          :target_message=>"You are severely unbalanced by {{user}}'s attack.",
          :spectator_message=>"{{target}} is severely unbalanced by {{user}}'s attack.",
          :effects=>%{
            :damage=>0.12,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"{{target}} is severely unbalanced by your attack.",
          :target_message=>"You are severely unbalanced by {{user}}'s attack.",
          :spectator_message=>"{{target}} is severely unbalanced by {{user}}'s attack.",
          :effects=>%{
            :damage=>0.15,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"{{target}} is spun about by the ferocity of your attack.",
          :target_message=>"You are spun about by the ferocity of {{user}}'s attack.",
          :spectator_message=>"{{target}} is spun about by the ferocity of {{user}}'s attack.",
          :effects=>%{
            :damage=>0.15,
            :stun=>2.0
          }
        },
        %{
          :user_message=>"You stagger {{target}} with your attack.",
          :target_message=>"{{user}} staggers you with {{user:his/her/its}} attack.",
          :spectator_message=>"{{user}} staggers {{target}} with {{user:his/her/its}} attack.",
          :effects=>%{
            :damage=>0.15,
            :stun=>1.0,
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
            ]
          }
        },
        %{
          :user_message=>"Your blast breaks {{target}}'s shoulder.",
          :target_message=>"{{user}}'s blast breaks your shoulder.",
          :spectator_message=>"{{user}}'s blast breaks {{target}}'s shoulder.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :damage=>0.3,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"You deliver a fierce blow to {{target}}'s back.",
          :target_message=>"{{user}} delivers a fierce blow to your back.",
          :spectator_message=>"{{user}} delivers a fierce blow to {{target}}'s back.",
          :effects=>%{
            :stun=>1.0,
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
            :damage=>0.18
          }
        },
        %{
          :user_message=>"{{target}} is knocked down.",
          :target_message=>"You are knocked down.",
          :spectator_message=>"{{target}} is knocked down.",
          :effects=>%{
            :damage=>0.15,
            :stun=>2.0,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-75,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"You strike {{target}}'s calf mightily.",
          :target_message=>"{{user}} strikes your calf mightily.",
          :spectator_message=>"{{user}} strikes {{target}}'s calf mightily.",
          :effects=>%{
            :damage=>0.3,
            :stun=>3.0,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-25,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"You deliver a mighty blow to {{target}}'s head.",
          :target_message=>"{{user}} delivers a mighty blow to your head.",
          :spectator_message=>"{{user}} delivers a mighty blow to {{target}}'s head.",
          :effects=>%{
            :damage=>0.37,
            :stun=>10.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>20.0
              },
              %{
                :skill=>"block",
                :amount=>-100,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your attack strikes {{target}} in the head, knocking {{target:him/her/it}} flat!",
          :target_message=>"{{user}}'s attack strikes you in the head, knocking you flat!",
          :spectator_message=>"{{user}}'s attack strikes {{target}} in the head, knocking {{target:him/her/it}} flat!",
          :effects=>%{
            :damage=>0.37,
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>3.0
            }
          }
        },
        %{
          :user_message=>"You deliver a mightly whack to {{target}}'s neck, temporarily paralyzing {{target:him/her/it}}!",
          :target_message=>"{{user}} delivers a mightly whack to your neck, temporarily paralyzing you!",
          :spectator_message=>"{{user}} delivers a mightly whack to {{target}}'s neck, temporarily paralyzing {{target:him/her/it}}!",
          :effects=>%{
            :damage=>0.45,
            :stun=>10.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>20.0
              }
            ]
          }
        }
      ],
      "C"=>[
        %{
          :user_message=>"{{target}} is unbalanced by your attack.",
          :target_message=>"You are unbalanced by {{user}}'s attack.",
          :spectator_message=>"{{target}} is unbalanced by {{user}}'s attack.",
          :effects=>%{
            :damage=>0.05,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"{{target}} is unbalanced by your attack.",
          :target_message=>"You are unbalanced by {{user}}'s attack.",
          :spectator_message=>"{{target}} is unbalanced by {{user}}'s attack.",
          :effects=>%{
            :damage=>0.12,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"{{target}} is severely unbalanced by your attack.",
          :target_message=>"You are severely unbalanced by {{user}}'s attack.",
          :spectator_message=>"{{target}} is severely unbalanced by {{user}}'s attack.",
          :effects=>%{
            :damage=>0.15,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"{{target}} is severely unbalanced by your attack.",
          :target_message=>"You are severely unbalanced by {{user}}'s attack.",
          :spectator_message=>"{{target}} is severely unbalanced by {{user}}'s attack.",
          :effects=>%{
            :damage=>0.18,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"Your attack strikes {{target}} firmly, spinning {{target:him/her/it}}.",
          :target_message=>"{{user}}'s attack strikes you firmly, spinning you.",
          :spectator_message=>"{{user}}'s attack strikes {{target}} firmly, spinning {{target:him/her/it}}.",
          :effects=>%{
            :damage=>0.18,
            :stun=>2.0
          }
        },
        %{
          :user_message=>"Your mighty attack stuns {{target}}.",
          :target_message=>"{{user}}'s mighty attack stuns you.",
          :spectator_message=>"{{user}}'s mighty attack stuns {{target}}.",
          :effects=>%{
            :stun=>1.0,
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
            :damage=>0.15
          }
        },
        %{
          :user_message=>"Incredibly, your blow breaks both of {{target}}'s arms!",
          :target_message=>"Incredibly, {{user}}'s blow breaks both of your arms!",
          :spectator_message=>"Incredibly, {{user}}'s blow breaks both of {{target}}'s arms!",
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
                :stat=>"agility",
                :amount=>-45,
                :duration=>40.0
              }
            ]
          }
        },
        %{
          :user_message=>"You deliver a mighty blow to {{target}}'s back.",
          :target_message=>"{{user}} delivers a mighty blow to your back.",
          :spectator_message=>"{{user}} delivers a mighty blow to {{target}}'s back.",
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
            ]
          }
        },
        %{
          :user_message=>"Your blow breaks {{target}}'s collar bone.",
          :target_message=>"{{user}}'s blow breaks your collar bone.",
          :spectator_message=>"{{user}}'s blow breaks {{target}}'s collar bone.",
          :effects=>%{
            :damage=>0.22,
            :stun=>1.0,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-50,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your attack breaks {{target}}'s hip. {{target:he/she/it}} cries out in pain.",
          :target_message=>"{{user}}'s attack breaks your hip. You cry out in pain.",
          :spectator_message=>"{{user}}'s attack breaks {{target}}'s hip. {{target:he/she/it}} cries out in pain.",
          :effects=>%{
            :damage=>0.3,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>4.0
              },
              %{
                :skill=>"attack",
                :amount=>-60,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your mighty blow shatters {{target}}'s knee.",
          :target_message=>"{{user}}'s mighty blow shatters your knee.",
          :spectator_message=>"{{user}}'s mighty blow shatters {{target}}'s knee.",
          :effects=>%{
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-30,
                :duration=>20.0
              }
            ],
            :damage=>0.3,
            :stun=>9.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>18.0
              },
              %{
                :skill=>"block",
                :amount=>-100,
                :duration=>18.0
              }
            ]
          }
        },
        %{
          :user_message=>"You deliver a fierce blow to {{target}}'s chest, breaking several ribs.",
          :target_message=>"{{user}} delivers a fierce blow to your chest, breaking several ribs.",
          :spectator_message=>"{{user}} delivers a fierce blow to {{target}}'s chest, breaking several ribs.",
          :effects=>%{
            :damage=>0.75
          }
        },
        %{
          :user_message=>"A fierce blow from you to {{target}}'s head crushes {{target:his/her/its}} skull, sending tiny brain and bone fragments flying!",
          :target_message=>"A fierce blow from {{user}} to your head crushes your skull, sending tiny brain and bone fragments flying!",
          :spectator_message=>"A fierce blow from {{user}} to {{target}}'s head crushes {{target:his/her/its}} skull, sending tiny brain and bone fragments flying!",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "D"=>[
        %{
          :user_message=>"{{target}} is unbalanced by your attack.",
          :target_message=>"You are unbalanced by {{user}}'s attack.",
          :spectator_message=>"{{target}} is unbalanced by {{user}}'s attack.",
          :effects=>%{
            :damage=>0.06,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"{{target}} is unbalanced by your attack.",
          :target_message=>"You are unbalanced by {{user}}'s attack.",
          :spectator_message=>"{{target}} is unbalanced by {{user}}'s attack.",
          :effects=>%{
            :damage=>0.15,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"{{target}} staggers, severely unbalanced by your attack.",
          :target_message=>"You stagger, severely unbalanced by {{user}}'s attack.",
          :spectator_message=>"{{target}} staggers, severely unbalanced by {{user}}'s attack.",
          :effects=>%{
            :damage=>0.15,
            :stun=>1.0,
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
          :user_message=>"{{target}} staggers under the might of your attack.",
          :target_message=>"You stagger under the might of {{user}}'s attack.",
          :spectator_message=>"{{target}} staggers under the might of {{user}}'s attack.",
          :effects=>%{
            :damage=>0.18,
            :stun=>2.0
          }
        },
        %{
          :user_message=>"You deliver a fierce blow to {{target}}'s side.",
          :target_message=>"{{user}} delivers a fierce blow to your side.",
          :spectator_message=>"{{user}} delivers a fierce blow to {{target}}'s side.",
          :effects=>%{
            :damage=>0.2,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} is knocked back a good five feet from your attack.",
          :target_message=>"You are knocked back a good five feet from {{user}}'s attack.",
          :spectator_message=>"{{target}} is knocked back a good five feet from {{user}}'s attack.",
          :effects=>%{
            :damage=>0.25,
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
          :user_message=>"You severely brain {{target}}, sending {{target:him/her/it}} quickly to the ground.",
          :target_message=>"{{user}} severely brains you, sending you quickly to the ground.",
          :spectator_message=>"{{user}} severely brains {{target}}, sending {{target:him/her/it}} quickly to the ground.",
          :effects=>%{
            :stun=>10.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>20.0
              },
              %{
                :skill=>"block",
                :amount=>-100,
                :duration=>20.0
              }
            ],
            :damage=>0.3
          }
        },
        %{
          :user_message=>"{{target}} is knocked to the ground.",
          :target_message=>"You are knocked to the ground.",
          :spectator_message=>"{{target}} is knocked to the ground.",
          :effects=>%{
            :damage=>0.22,
            :stun=>2.0
          }
        },
        %{
          :user_message=>"Your blow breaks {{target}}'s arm.",
          :target_message=>"{{user}}'s blow breaks your arm.",
          :spectator_message=>"{{user}}'s blow breaks {{target}}'s arm.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :damage=>0.15,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"You deliver a severe strike to {{target}}'s abdomen, causing {{target:him/her/it}} to spit up much blood.",
          :target_message=>"{{user}} delivers a severe strike to your abdomen, causing you to spit up much blood.",
          :spectator_message=>"{{user}} delivers a severe strike to {{target}}'s abdomen, causing {{target:him/her/it}} to spit up much blood.",
          :effects=>%{
            :stun=>12.0,
            :damage=>0.3
          }
        },
        %{
          :user_message=>"Your mighty attack shatters {{target}}'s jaw, sending teeth and bone flying!",
          :target_message=>"{{user}}'s mighty attack shatters your jaw, sending teeth and bone flying!",
          :spectator_message=>"{{user}}'s mighty attack shatters {{target}}'s jaw, sending teeth and bone flying!",
          :effects=>%{
            :damage=>0.75,
            :stun=>3.0,
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-25,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You deliver a mighty strike to {{target}}'s side, driving bone into {{target:his/her/its}} kidneys...OW!",
          :target_message=>"{{user}} delivers a mighty strike to your side, driving bone into your kidneys...OW!",
          :spectator_message=>"{{user}} delivers a mighty strike to {{target}}'s side, driving bone into {{target:his/her/its}} kidneys...OW!",
          :effects=>%{
            :damage=>0.37,
            :damage_over_time=>%{
              :damage=>0.25,
              :duration=>40.0
            }
          }
        },
        %{
          :user_message=>"With a fierce blow to the chest, you cause {{target}}'s heart and lungs to explode!",
          :target_message=>"With a fierce blow to the chest, {{user}} causes your heart and lungs to explode!",
          :spectator_message=>"With a fierce blow to the chest, {{user}} causes {{target}}'s heart and lungs to explode!",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "E"=>[
        %{
          :user_message=>"{{target}} reels from your mighty attack.",
          :target_message=>"You reel from {{user}}'s mighty attack.",
          :spectator_message=>"{{target}} reels from {{user}}'s mighty attack.",
          :effects=>%{
            :damage=>0.22,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"{{target}} is sent flying by your attack, landing 10 feet from the point of impact.",
          :target_message=>"You are sent flying by {{user}}'s attack, landing 10 feet from the point of impact.",
          :spectator_message=>"{{target}} is sent flying by {{user}}'s attack, landing 10 feet from the point of impact.",
          :effects=>%{
            :damage=>0.3,
            :stun=>2.0
          }
        },
        %{
          :user_message=>"{{target}} staggers under the might of your attack!",
          :target_message=>"You stagger under the might of {{user}}'s attack!",
          :spectator_message=>"{{target}} staggers under the might of {{user}}'s attack!",
          :effects=>%{
            :damage=>0.3,
            :stun=>1.0,
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
          :user_message=>"You knock {{target}} forcefully to the ground.",
          :target_message=>"{{user}} knocks you forcefully to the ground.",
          :spectator_message=>"{{user}} knocks {{target}} forcefully to the ground.",
          :effects=>%{
            :damage=>0.3,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>4.0
              },
              %{
                :skill=>"block",
                :amount=>-100,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} kisses the pavement hard.",
          :target_message=>"You kiss the pavement hard.",
          :spectator_message=>"{{target}} kisses the pavement hard.",
          :effects=>%{
            :damage=>0.3,
            :stun=>3.0,
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
        },
        %{
          :user_message=>"In a spectacular display, {{target}}'s skull shatters into thousands of particles!",
          :target_message=>"In a spectacular display, your skull shatters into thousands of particles!",
          :spectator_message=>"In a spectacular display, {{target}}'s skull shatters into thousands of particles!",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"You deliver a strong blast to {{target}}'s arm.",
          :target_message=>"{{user}} delivers a strong blast to your arm.",
          :spectator_message=>"{{user}} delivers a strong blast to {{target}}'s arm.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ]
          }
        },
        %{
          :user_message=>"Your mighty blow shatters {{target}}'s collar bones and both shoulders!",
          :target_message=>"{{user}}'s mighty blow shatters your collar bones and both shoulders!",
          :spectator_message=>"{{user}}'s mighty blow shatters {{target}}'s collar bones and both shoulders!",
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
            :damage=>0.37
          }
        },
        %{
          :user_message=>"You strike mightily in the side, crushing a variety of important organs.",
          :target_message=>"{{user}} strikes mightily in the side, crushing a variety of important organs.",
          :spectator_message=>"{{user}} strikes mightily in the side, crushing a variety of important organs.",
          :effects=>%{
            :damage=>0.45,
            :damage_over_time=>%{
              :damage=>0.15,
              :duration=>15.0
            }
          }
        },
        %{
          :user_message=>"Your mighty attack strikes {{target}} in the chest, forcing several ribs through one of {{target:his/her/its}} lungs!",
          :target_message=>"{{user}}'s mighty attack strikes you in the chest, forcing several ribs through one of your lungs!",
          :spectator_message=>"{{user}}'s mighty attack strikes {{target}} in the chest, forcing several ribs through one of {{target:his/her/its}} lungs!",
          :effects=>%{
            :damage=>0.4,
            :damage_over_time=>%{
              :damage=>0.2,
              :duration=>15.0
            }
          }
        },
        %{
          :user_message=>"{{target}}'s skull is effortlessly crushed like an overripe melon under the might of your attack.",
          :target_message=>"Your skull is effortlessly crushed like an overripe melon under the might of {{user}}'s attack.",
          :spectator_message=>"{{target}}'s skull is effortlessly crushed like an overripe melon under the might of {{user}}'s attack.",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"S P L A T T ! ! ! ! ! ! !",
          :target_message=>"S P L A T T ! ! ! ! ! ! !",
          :spectator_message=>"S P L A T T ! ! ! ! ! ! !",
          :effects=>%{
            :kill=>true
          }
        }
      ]
    }
  end

end
