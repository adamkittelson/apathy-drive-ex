defmodule ApathyDrive.CritTables.Fire do

  def name do
    "fire"
  end

  def damage_type do
    "magical"
  end

  def crits do
    %{
      "A"=>[
        %{
          :user_message=>"You burn {{target}}.",
          :target_message=>"{{user}} burns you.",
          :spectator_message=>"{{user}} burns {{target}}.",
          :effects=>%{
            :damage=>0.01
          }
        },
        %{
          :user_message=>"You give {{target}} a stinging burn.",
          :target_message=>"{{user}} gives you a stinging burn.",
          :spectator_message=>"{{user}} gives {{target}} a stinging burn.",
          :effects=>%{
            :damage=>0.02,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-30,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"You burn {{target}} painfully.",
          :target_message=>"{{user}} burns you painfully.",
          :spectator_message=>"{{user}} burns {{target}} painfully.",
          :effects=>%{
            :damage=>0.03,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-60,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"You burn {{target}} badly, unbalancing {{target:him/her/it}}.",
          :target_message=>"{{user}} burns you badly, unbalancing you.",
          :spectator_message=>"{{user}} burns {{target}} badly, unbalancing {{target:him/her/it}}.",
          :effects=>%{
            :damage=>0.04,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-20,
                :duration=>4.0
              },
              %{
                :skill=>"dodge",
                :amount=>-20,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your flame burns {{target}}'s flesh.",
          :target_message=>"{{user}}'s flame burns your flesh.",
          :spectator_message=>"{{user}}'s flame burns {{target}}'s flesh.",
          :effects=>%{
            :damage=>0.06,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"You flame {{target}}, giving {{target:him/her/it}} a few nasty burns.",
          :target_message=>"{{user}} flames you, giving you a few nasty burns.",
          :spectator_message=>"{{user}} flames {{target}}, giving {{target:him/her/it}} a few nasty burns.",
          :effects=>%{
            :damage=>0.07,
            :stun=>1.0,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>1.0
            }
          }
        },
        %{
          :user_message=>"You burn {{target}}, making {{target:him/her/it}} cringe in stinging pain.",
          :target_message=>"{{user}} burns you, making you cringe in stinging pain.",
          :spectator_message=>"{{user}} burns {{target}}, making {{target:him/her/it}} cringe in stinging pain.",
          :effects=>%{
            :damage=>0.08,
            :stun=>2.0,
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>2.0
            }
          }
        },
        %{
          :user_message=>"You blacken {{target}}'s skin with yellow flame.",
          :target_message=>"{{user}} blackens your skin with yellow flame.",
          :spectator_message=>"{{user}} blackens {{target}}'s skin with yellow flame.",
          :effects=>%{
            :damage=>0.07,
            :stun=>1.0,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-10,
                :duration=>40.0
              }
            ]
          }
        },
        %{
          :user_message=>"You ignite {{target}} with a blast of crackling flame!",
          :target_message=>"{{user}} ignites you with a blast of crackling flame!",
          :spectator_message=>"{{user}} ignites {{target}} with a blast of crackling flame!",
          :effects=>%{
            :stun=>5.0,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"You flame {{target}}, inflicting a horrible welting burn.",
          :target_message=>"{{user}} flames you, inflicting a horrible welting burn.",
          :spectator_message=>"{{user}} flames {{target}}, inflicting a horrible welting burn.",
          :effects=>%{
            :stun=>1.0,
            :damage=>0.13,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-10,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"You shower {{target}} with burning embers and bright flame.",
          :target_message=>"{{user}} showers you with burning embers and bright flame.",
          :spectator_message=>"{{user}} showers {{target}} with burning embers and bright flame.",
          :effects=>%{
            :stun=>2.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-25,
                :duration=>14.0
              }
            ],
            :damage=>0.1,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>3.0
            }
          }
        },
        %{
          :user_message=>"Your fire broils flesh off {{target}}'s arm.",
          :target_message=>"{{user}}'s fire broils flesh off your arm.",
          :spectator_message=>"{{user}}'s fire broils flesh off {{target}}'s arm.",
          :effects=>%{
            :stun=>3.0,
            :damage=>0.13,
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ]
          }
        },
        %{
          :user_message=>"You sting {{target}} all over with angry flames.",
          :target_message=>"{{user}} stings you all over with angry flames.",
          :spectator_message=>"{{user}} stings {{target}} all over with angry flames.",
          :effects=>%{
            :stun=>1.0,
            :damage=>0.1,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>3.0
            },
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
          :user_message=>"You cook {{target}}'s lower body with a wash of fire.",
          :target_message=>"{{user}} cooks your lower body with a wash of fire.",
          :spectator_message=>"{{user}} cooks {{target}}'s lower body with a wash of fire.",
          :effects=>%{
            :damage=>0.5,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-40,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your searing flames tear flesh from {{target}}'s face.",
          :target_message=>"{{user}}'s searing flames tear flesh from your face.",
          :spectator_message=>"{{user}}'s searing flames tear flesh from {{target}}'s face.",
          :effects=>%{
            :stun=>6.0,
            :damage=>0.36,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-40,
                :duration=>40.0
              }
            ]
          }
        },
        %{
          :user_message=>"You blast {{target}} with white fire!",
          :target_message=>"{{user}} blasts you with white fire!",
          :spectator_message=>"{{user}} blasts {{target}} with white fire!",
          :effects=>%{
            :stun=>5.0,
            :damage=>0.33,
            :damage_over_time=>%{
              :damage=>0.09,
              :duration=>4.0
            },
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-35,
                :duration=>20.0
              },
              %{
                :skill=>"dodge",
                :amount=>-35,
                :duration=>20.0
              }
            ]
          }
        }
      ],
      "B"=>[
        %{
          :user_message=>"You inflict ugly burns on {{target}}.",
          :target_message=>"{{user}} inflicts ugly burns on you.",
          :spectator_message=>"{{user}} inflicts ugly burns on {{target}}.",
          :effects=>%{
            :damage=>0.05
          }
        },
        %{
          :user_message=>"You singe {{target}} with fearful flames.",
          :target_message=>"{{user}} singes you with fearful flames.",
          :spectator_message=>"{{user}} singes {{target}} with fearful flames.",
          :effects=>%{
            :stun=>1.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-10,
                :duration=>10.0
              }
            ],
            :damage=>0.06
          }
        },
        %{
          :user_message=>"You light a small, but scorching fire on {{target}}.",
          :target_message=>"{{user}} lights a small, but scorching fire on you.",
          :spectator_message=>"{{user}} lights a small, but scorching fire on {{target}}.",
          :effects=>%{
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>6.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.04,
              :duration=>3.0
            }
          }
        },
        %{
          :user_message=>"Veins in {{target}}'s eyes bulge grotesquely in the heat from your attack.",
          :target_message=>"Veins in your eyes bulge grotesquely in the heat from {{user}}'s attack.",
          :spectator_message=>"Veins in {{target}}'s eyes bulge grotesquely in the heat from {{user}}'s attack.",
          :effects=>%{
            :stun=>1.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-35,
                :duration=>10.0
              }
            ],
            :damage=>0.07
          }
        },
        %{
          :user_message=>"You bake bits of flesh off {{target}}.",
          :target_message=>"{{user}} bakes bits of flesh off you.",
          :spectator_message=>"{{user}} bakes bits of flesh off {{target}}.",
          :effects=>%{
            :stun=>1.0,
            :damage=>0.12
          }
        },
        %{
          :user_message=>"You flame {{target}}, giving {{target:him/her/it}} many painful burns.",
          :target_message=>"{{user}} flames you, giving you many painful burns.",
          :spectator_message=>"{{user}} flames {{target}}, giving {{target:him/her/it}} many painful burns.",
          :effects=>%{
            :stun=>2.0,
            :damage=>0.1,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>3.0
            }
          }
        },
        %{
          :user_message=>"You force {{target}} to cry out when blue flame burns {{target:him/her/it}}.",
          :target_message=>"{{user}} forces you to cry out when blue flame burns you.",
          :spectator_message=>"{{user}} forces {{target}} to cry out when blue flame burns {{target:him/her/it}}.",
          :effects=>%{
            :stun=>2.0,
            :damage=>0.2,
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-15,
                :duration=>40.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your flame boils the flesh away from the bone on {{target}}'s legs!",
          :target_message=>"{{user}}'s flame boils the flesh away from the bone on your legs!",
          :spectator_message=>"{{user}}'s flame boils the flesh away from the bone on {{target}}'s legs!",
          :effects=>%{
            :stun=>9.0,
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"leg"
              },
              %{
                :kind=>"cripple",
                :limb=>"leg"
              }
            ]
          }
        },
        %{
          :user_message=>"You fire up {{target}}'s face with glowing sparks.",
          :target_message=>"{{user}} fires up your face with glowing sparks.",
          :spectator_message=>"{{user}} fires up {{target}}'s face with glowing sparks.",
          :effects=>%{
            :stun=>3.0,
            :damage_over_time=>%{
              :damage=>0.06,
              :duration=>5.0
            },
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-15,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"You burn holes in {{target}}'s skin, and there is a smell of cooking kidneys.",
          :target_message=>"{{user}} burns holes in your skin, and there is a smell of cooking kidneys.",
          :spectator_message=>"{{user}} burns holes in {{target}}'s skin, and there is a smell of cooking kidneys.",
          :effects=>%{
            :stun=>1.0,
            :damage=>0.07,
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>4.0
            }
          }
        },
        %{
          :user_message=>"You flame {{target}}, who struggles to smother the searing danger.",
          :target_message=>"{{user}} flames you, who struggles to smother the searing danger.",
          :spectator_message=>"{{user}} flames {{target}}, who struggles to smother the searing danger.",
          :effects=>%{
            :stun=>5.0,
            :damage_over_time=>%{
              :damage=>0.07,
              :duration=>4.0
            },
            :skill_mod=>[
              %{
                :skill=>"block",
                :amount=>-40,
                :duration=>14.0
              }
            ]
          }
        },
        %{
          :user_message=>"You horribly burn {{target}} with relentless smoking fires!",
          :target_message=>"{{user}} horribly burns you with relentless smoking fires!",
          :spectator_message=>"{{user}} horribly burns {{target}} with relentless smoking fires!",
          :effects=>%{
            :stun=>3.0,
            :damage_over_time=>%{
              :damage=>0.17,
              :duration=>3.0
            },
            :damage=>0.23,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-35,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"You blast much flesh off {{target:him/her/it}} ribs with phosphorescsnt fire!",
          :target_message=>"{{user}} blasts much flesh off you ribs with phosphorescsnt fire!",
          :spectator_message=>"{{user}} blasts much flesh off {{target:him/her/it}} ribs with phosphorescsnt fire!",
          :effects=>%{
            :stun=>9.0,
            :damage=>0.88,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-20,
                :duration=>34.0
              }
            ]
          }
        }
      ],
      "C"=>[
        %{
          :user_message=>"You scorch {{target}}, leaving stinging, smoking burns.",
          :target_message=>"{{user}} scorches you, leaving stinging, smoking burns.",
          :spectator_message=>"{{user}} scorches {{target}}, leaving stinging, smoking burns.",
          :effects=>%{
            :damage=>0.15
          }
        },
        %{
          :user_message=>"Your bright yellow fires lick {{target}}.",
          :target_message=>"{{user}}'s bright yellow fires lick you.",
          :spectator_message=>"{{user}}'s bright yellow fires lick {{target}}.",
          :effects=>%{
            :stun=>1.0,
            :damage=>0.1,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-10,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"You scorch {{target}} all over with roaring red fires.",
          :target_message=>"{{user}} scorches you all over with roaring red fires.",
          :spectator_message=>"{{user}} scorches {{target}} all over with roaring red fires.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>5.0
            },
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-25,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"You ignite {{target}}'s neck, and the blood in {{target:him/her/it}} largest veins boils.",
          :target_message=>"{{user}} ignites your neck, and the blood in you largest veins boils.",
          :spectator_message=>"{{user}} ignites {{target}}'s neck, and the blood in {{target:him/her/it}} largest veins boils.",
          :effects=>%{
            :stun=>3.0,
            :damage_over_time=>%{
              :damage=>0.07,
              :duration=>4.0
            },
            :damage=>0.11
          }
        },
        %{
          :user_message=>"You blast {{target}} with furious fire.",
          :target_message=>"{{user}} blasts you with furious fire.",
          :spectator_message=>"{{user}} blasts {{target}} with furious fire.",
          :effects=>%{
            :stun=>1.0,
            :damage=>0.43,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-20,
                :duration=>14.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your quick flames boil blood in {{target}}'s arms.",
          :target_message=>"{{user}}'s quick flames boil blood in your arms.",
          :spectator_message=>"{{user}}'s quick flames boil blood in {{target}}'s arms.",
          :effects=>%{
            :stun=>4.0,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>7.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-30,
                :duration=>28.0
              },
              %{
                :skill=>"parry",
                :amount=>-30,
                :duration=>28.0
              }
            ]
          }
        },
        %{
          :user_message=>"You flame {{target}}, leaving streaks of charred skin.",
          :target_message=>"{{user}} flames you, leaving streaks of charred skin.",
          :spectator_message=>"{{user}} flames {{target}}, leaving streaks of charred skin.",
          :effects=>%{
            :stun=>2.0,
            :damage=>0.39,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-40,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"You make a walking rare steak of {{target}} with a column of orange flame.",
          :target_message=>"{{user}} makes a walking rare steak of you with a column of orange flame.",
          :spectator_message=>"{{user}} makes a walking rare steak of {{target}} with a column of orange flame.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :damage_over_time=>%{
              :damage=>0.12,
              :duration=>5.0
            },
            :stun=>5.0
          }
        },
        %{
          :user_message=>"You bathe {{target}} in black smoke and hissing white embers.",
          :target_message=>"{{user}} bathes you in black smoke and hissing white embers.",
          :spectator_message=>"{{user}} bathes {{target}} in black smoke and hissing white embers.",
          :effects=>%{
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-30,
                :duration=>36.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.06,
              :duration=>7.0
            },
            :stun=>4.0
          }
        },
        %{
          :user_message=>"You light up {{target}}, who throws {{target}}self to the floor in panic!",
          :target_message=>"{{user}} lights up you, who throws youself to the floor in panic!",
          :spectator_message=>"{{user}} lights up {{target}}, who throws {{target}}self to the floor in panic!",
          :effects=>%{
            :stun=>5.0,
            :damage_over_time=>%{
              :damage=>0.13,
              :duration=>6.0
            },
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-55,
                :duration=>14.0
              }
            ]
          }
        },
        %{
          :user_message=>"You burns {{target}} in the face, charring flesh and boiling {{target}}'s eyeballs!",
          :target_message=>"{{user}} burns you in the face, charring flesh and boiling your eyeballs!",
          :spectator_message=>"{{user}} burns {{target}} in the face, charring flesh and boiling {{target}}'s eyeballs!",
          :effects=>%{
            :stun=>11.0,
            :damage_over_time=>%{
              :damage=>0.09,
              :duration=>3.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-50,
                :duration=>100.0
              }
            ]
          }
        },
        %{
          :user_message=>"You knock {{target}} on {{target:his/her/its}} back with a blast of fierce fire!",
          :target_message=>"{{user}} knocks you on your back with a blast of fierce fire!",
          :spectator_message=>"{{user}} knocks {{target}} on {{target:his/her/its}} back with a blast of fierce fire!",
          :effects=>%{
            :stun=>3.0,
            :damage=>1.01,
            :damage_over_time=>%{
              :damage=>0.21,
              :duration=>3.0
            }
          }
        },
        %{
          :user_message=>"Your raging winds of blue-green flames leave {{target}} a boiling, blackened mess!",
          :target_message=>"{{user}}'s raging winds of blue-green flames leave you a boiling, blackened mess!",
          :spectator_message=>"{{user}}'s raging winds of blue-green flames leave {{target}} a boiling, blackened mess!",
          :effects=>%{
            :stun=>25.0,
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"random"
              }
            ],
            :damage_over_time=>%{
              :damage=>0.09,
              :duration=>19.0
            },
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-40,
                :duration=>50.0
              }
            ]
          }
        }
      ],
      "D"=>[
        %{
          :user_message=>"You burn {{target}}, who shrieks in pain.",
          :target_message=>"{{user}} burns you, who shrieks in pain.",
          :spectator_message=>"{{user}} burns {{target}}, who shrieks in pain.",
          :effects=>%{
            :stun=>1.0,
            :damage=>0.13
          }
        },
        %{
          :user_message=>"You burn {{target}} with bright flames that boil flesh from {{target}}'s hands.",
          :target_message=>"{{user}} burns you with bright flames that boil flesh from your hands.",
          :spectator_message=>"{{user}} burns {{target}} with bright flames that boil flesh from {{target}}'s hands.",
          :effects=>%{
            :stun=>2.0,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>8.0
            },
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-35,
                :duration=>16.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your glowing flames catch {{target}} and burn wildly.",
          :target_message=>"{{user}}'s glowing flames catch you and burn wildly.",
          :spectator_message=>"{{user}}'s glowing flames catch {{target}} and burn wildly.",
          :effects=>%{
            :stun=>5.0,
            :damage_over_time=>%{
              :damage=>0.09,
              :duration=>5.0
            },
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-10,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"You curdle much of {{target}}'s flesh with blinding fire.",
          :target_message=>"{{user}} curdles much of your flesh with blinding fire.",
          :spectator_message=>"{{user}} curdles much of {{target}}'s flesh with blinding fire.",
          :effects=>%{
            :stun=>2.0,
            :damage=>0.53,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-30,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your yellow fires melt flesh away from {{target}}'s bones.",
          :target_message=>"{{user}}'s yellow fires melt flesh away from your bones.",
          :spectator_message=>"{{user}}'s yellow fires melt flesh away from {{target}}'s bones.",
          :effects=>%{
            :stun=>4.0,
            :damage_over_time=>%{
              :damage=>0.21,
              :duration=>4.0
            },
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-45,
                :duration=>30.0
              }
            ]
          }
        },
        %{
          :user_message=>"You fry {{target}} on one side in a fit of fire.",
          :target_message=>"{{user}} fries you on one side in a fit of fire.",
          :spectator_message=>"{{user}} fries {{target}} on one side in a fit of fire.",
          :effects=>%{
            :stun=>3.0,
            :damage=>0.33,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>15.0
            }
          }
        },
        %{
          :user_message=>"You melt glowing skin from {{target}}'s contorted face.",
          :target_message=>"{{user}} melts glowing skin from your contorted face.",
          :spectator_message=>"{{user}} melts glowing skin from {{target}}'s contorted face.",
          :effects=>%{
            :stun=>6.0,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>6.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-35,
                :duration=>34.0
              }
            ]
          }
        },
        %{
          :user_message=>"You light {{target}} up, who shines and smokes and glows before CRUMBLING like a smoked cigar.",
          :target_message=>"{{user}} lights you up, who shines and smokes and glows before CRUMBLING like a smoked cigar.",
          :spectator_message=>"{{user}} lights {{target}} up, who shines and smokes and glows before CRUMBLING like a smoked cigar.",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"You send snakes of flame hissing over {{target}}'s boiling body.",
          :target_message=>"{{user}} sends snakes of flame hissing over your boiling body.",
          :spectator_message=>"{{user}} sends snakes of flame hissing over {{target}}'s boiling body.",
          :effects=>%{
            :stun=>4.0,
            :damage=>0.52,
            :damage_over_time=>%{
              :damage=>0.09,
              :duration=>4.0
            }
          }
        },
        %{
          :user_message=>"You ignite {{target}}, causing {{target:him/her/it}} to scream in agony and struggle to extinguish the scorching fire.",
          :target_message=>"{{user}} ignites you, causing you to scream in agony and struggle to extinguish the scorching fire.",
          :spectator_message=>"{{user}} ignites {{target}}, causing {{target:him/her/it}} to scream in agony and struggle to extinguish the scorching fire.",
          :effects=>%{
            :stun=>6.0,
            :damage_over_time=>%{
              :damage=>0.14,
              :duration=>6.0
            },
            :skill_mod=>[
              %{
                :skill=>"block",
                :amount=>-50,
                :duration=>16.0
              }
            ]
          }
        },
        %{
          :user_message=>"You engulf {{target}} in a cloud of raging smoke and orange fire!",
          :target_message=>"{{user}} engulves you in a cloud of raging smoke and orange fire!",
          :spectator_message=>"{{user}} engulves {{target}} in a cloud of raging smoke and orange fire!",
          :effects=>%{
            :stun=>9.0,
            :damage_over_time=>%{
              :damage=>0.19,
              :duration=>9.0
            },
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-40,
                :duration=>22.0
              },
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>22.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your blasts of intense fire throw {{target}} to the ground, blackened and smoking!",
          :target_message=>"{{user}}'s blasts of intense fire throw you to the ground, blackened and smoking!",
          :spectator_message=>"{{user}}'s blasts of intense fire throw {{target}} to the ground, blackened and smoking!",
          :effects=>%{
            :stun=>7.0,
            :damage=>2.12,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-45,
                :duration=>30.0
              }
            ]
          }
        },
        %{
          :user_message=>"You cook {{target}} in a relentless roaring inferno! Well done.",
          :target_message=>"{{user}} cooks you in a relentless roaring inferno! Well done.",
          :spectator_message=>"{{user}} cooks {{target}} in a relentless roaring inferno! Well done.",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "E"=>[
        %{
          :user_message=>"You scorch {{target}} with 3rd degree burns all over.",
          :target_message=>"{{user}} scorches you with 3rd degree burns all over.",
          :spectator_message=>"{{user}} scorches {{target}} with 3rd degree burns all over.",
          :effects=>%{
            :stun=>2.0,
            :damage=>0.32,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-15,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"You cover {{target}} with mad orange fires and swirling smoke.",
          :target_message=>"{{user}} covers you with mad orange fires and swirling smoke.",
          :spectator_message=>"{{user}} covers {{target}} with mad orange fires and swirling smoke.",
          :effects=>%{
            :stun=>3.0,
            :damage_over_time=>%{
              :damage=>0.15,
              :duration=>4.0
            },
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-10,
                :duration=>40.0
              }
            ]
          }
        },
        %{
          :user_message=>"You so brutally torch {{target}} that {{target:him/her/it}} screams in cowering pain.",
          :target_message=>"{{user}} so brutally torches you that you screams in cowering pain.",
          :spectator_message=>"{{user}} so brutally torches {{target}} that {{target:him/her/it}} screams in cowering pain.",
          :effects=>%{
            :stun=>5.0,
            :damage=>0.39,
            :damage_over_time=>%{
              :damage=>0.07,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"Your searing flames rip past {{target}}, stealing and charring flesh!",
          :target_message=>"{{user}}'s searing flames rip past you, stealing and charring flesh!",
          :spectator_message=>"{{user}}'s searing flames rip past {{target}}, stealing and charring flesh!",
          :effects=>%{
            :stun=>3.0,
            :damage=>0.66
          }
        },
        %{
          :user_message=>"You burn {{target}} with mighty white flames! {{target}} retreats in shock.",
          :target_message=>"{{user}} burns you with mighty white flames! you retreats in shock.",
          :spectator_message=>"{{user}} burns {{target}} with mighty white flames! {{target}} retreats in shock.",
          :effects=>%{
            :stun=>5.0,
            :damage=>0.4,
            :damage_over_time=>%{
              :damage=>0.13,
              :duration=>3.0
            },
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-15,
                :duration=>22.0
              }
            ],
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-30,
                :duration=>18.0
              }
            ]
          }
        },
        %{
          :user_message=>"You fry {{target}}'s skin in a fury of smoke and fire!",
          :target_message=>"{{user}} fries your skin in a fury of smoke and fire!",
          :spectator_message=>"{{user}} fries {{target}}'s skin in a fury of smoke and fire!",
          :effects=>%{
            :stun=>7.0,
            :damage_over_time=>%{
              :damage=>0.18,
              :duration=>6.0
            },
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-35,
                :duration=>20.0
              },
              %{
                :skill=>"parry",
                :amount=>-35,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} swallows your wicked flames, coughing up smoke and bits of burnt vitals!",
          :target_message=>"You swallow {{user}}'s wicked flames, coughing up smoke and bits of burnt vitals!",
          :spectator_message=>"{{target}} swallows {{user}}'s wicked flames, coughing up smoke and bits of burnt vitals!",
          :effects=>%{
            :stun=>11.0,
            :damage_over_time=>%{
              :damage=>0.11,
              :duration=>7.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-20,
                :duration=>50.0
              }
            ]
          }
        },
        %{
          :user_message=>"You assault {{target}} with blinding fires that leave {{target:him/her/it}} a burning lump of bones and charcoal!",
          :target_message=>"{{user}} assaults you with blinding fires that leave you a burning lump of bones and charcoal!",
          :spectator_message=>"{{user}} assaults {{target}} with blinding fires that leave {{target:him/her/it}} a burning lump of bones and charcoal!",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"You immolate {{target}}, who runs around screaming and burning, trailing dense smoke behind!",
          :target_message=>"{{user}} immolates you, who runs around screaming and burning, trailing dense smoke behind!",
          :spectator_message=>"{{user}} immolates {{target}}, who runs around screaming and burning, trailing dense smoke behind!",
          :effects=>%{
            :stun=>9.0,
            :damage_over_time=>%{
              :damage=>0.16,
              :duration=>9.0
            },
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
          :user_message=>"Your bright blue conflageration boils {{target}} with the hissing of steam and the scent of burnt organs!",
          :target_message=>"{{user}}'s bright blue conflageration boils you with the hissing of steam and the scent of burnt organs!",
          :spectator_message=>"{{user}}'s bright blue conflageration boils {{target}} with the hissing of steam and the scent of burnt organs!",
          :effects=>%{
            :stun=>5.0,
            :damage=>1.3,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-50,
                :duration=>50.0
              }
            ]
          }
        },
        %{
          :user_message=>"You cook {{target}} all over with flames that cook away vital fluids and rend flesh!",
          :target_message=>"{{user}} cooks you all over with flames that cook away vital fluids and rend flesh!",
          :spectator_message=>"{{user}} cooks {{target}} all over with flames that cook away vital fluids and rend flesh!",
          :effects=>%{
            :stun=>15.0,
            :damage_over_time=>%{
              :damage=>0.35,
              :duration=>10.0
            },
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-50,
                :duration=>40.0
              }
            ]
          }
        },
        %{
          :user_message=>"You reduce {{target}} to a blackened, smoking skeleton, sprawled and twisted on the ground!",
          :target_message=>"{{user}} reduces you to a blackened, smoking skeleton, sprawled and twisted on the ground!",
          :spectator_message=>"{{user}} reduces {{target}} to a blackened, smoking skeleton, sprawled and twisted on the ground!",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"You incinerate {{target}} in an inferno of blinding white fire!",
          :target_message=>"{{user}} incinerates you in an inferno of blinding white fire!",
          :spectator_message=>"{{user}} incinerates {{target}} in an inferno of blinding white fire!",
          :effects=>%{
            :kill=>true
          }
        }
      ]
    }
  end

end
