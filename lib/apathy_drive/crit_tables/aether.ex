defmodule ApathyDrive.CritTables.Aether do

  def name do
    "aether"
  end

  def damage_type do
    "magical"
  end

  def crits do
    %{
      "A"=>[
        %{
          :user_message => "HA! You fumble your attack!",
          :target_message => "HA! {{user}} fumbles {{user:his/her/its}} attack!",
          :spectator_message => "HA! {{user}} fumbles {{user:his/her/its}} attack!",
          :effects => %{ :damage => 0.0 }
        },
        %{
          :user_message => "You release a powerful beam of light which tears right through {{target}}.",
          :target_message => "{{user}} releases a powerful beam of light which tears right through you.",
          :spectator_message => "{{user}} releases a powerful beam of light which tears right through {{target}}.",
          :effects=>%{
            :damage => 1.0,
            :damage_over_time => %{
              :damage => 0.1,
              :duration => 5.0
            }
          }
        },
        %{
          :user_message =>"The earth begins to spin violently, sending {{target}} into pure shock! you cackle nearby",
          :target_message=>"The earth begins to spin violently, sending you into pure shock! {{user}} cackles nearby",
          :spectator_message=>"The earth begins to spin violently, sending {{target}} into pure shock! {{user}} cackles nearby",
          :effects=>%{
            :stun=>2.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-10,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"You raise your hands into the air. The ground around {{target}} rises above {{target:him/her/it}} and comes crashing down on {{target:his/her/its}} head.",
          :target_message=>"{{user}} raises {{user:his/her/its}} hands into the air. The ground around you rises above you and comes crashing down on your head.",
          :spectator_message=>"{{user}} raises {{user:his/her/its}} hands into the air. The ground around {{target}} rises above {{target:him/her/it}} and comes crashing down on {{target:his/her/its}} head.",
          :effects=>%{
            :damage=>0.15,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"A large amount of visible energy is released by you in {{target}}'s direction.",
          :target_message=>"A large amount of visible energy is released by {{user}} in your direction.",
          :spectator_message=>"A large amount of visible energy is released by {{user}} in {{target}}'s direction.",
          :effects=>%{
            :damage=>0.2,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-5,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"With a possessed look in your eyes, the air around {{target}} heats to unbearable temperatures.",
          :target_message=>"With a possessed look in {{user}}'s eyes, the air around you heats to unbearable temperatures.",
          :spectator_message=>"With a possessed look in {{user}}'s eyes, the air around {{target}} heats to unbearable temperatures.",
          :effects=>%{
            :damage=>0.23,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-10,
                :duration=>10.0
              },
              %{
                :skill=>"parry",
                :amount=>-10,
                :duration=>14.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-20,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"With energy beams spewing from your body, you let loose a massive volley of attacks.",
          :target_message=>"With energy beams spewing from {{user:his/her/its}} body, {{user}} lets loose a massive volley of attacks.",
          :spectator_message=>"With energy beams spewing from {{user:his/her/its}} body, {{user}} lets loose a massive volley of attacks.",
          :effects=>%{
            :damage=>0.05,
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>2.0
            }
          }
        },
        %{
          :user_message=>"You open up a inter dimensional rift, releasing enough energy to severely handicap {{target}}.",
          :target_message=>"{{user}} opens up a inter dimensional rift, releasing enough energy to severely handicap you.",
          :spectator_message=>"{{user}} opens up a inter dimensional rift, releasing enough energy to severely handicap {{target}}.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"non_fatal"
              }
            ]
          }
        },
        %{
          :user_message=>"You open up a MAJOR inter dimensional rift, releasing enough energy to GROTESQUELY impair {{target}}.",
          :target_message=>"{{user}} opens up a MAJOR inter dimensional rift, releasing enough energy to GROTESQUELY impair you.",
          :spectator_message=>"{{user}} opens up a MAJOR inter dimensional rift, releasing enough energy to GROTESQUELY impair {{target}}.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"non_fatal"
              },
              %{
                :kind=>"cripple",
                :limb=>"non_fatal"
              }
            ]
          }
        },
        %{
          :user_message=>"With magical and frightening pin-point accuracy, you throw your weapon directly through {{target}}'s body. It then returns to you like a boomerang.",
          :target_message=>"With magical and frightening pin-point accuracy, {{user}} throws {{user:his/her/its}} weapon directly through your body. It then returns to {{user}} like a boomerang.",
          :spectator_message=>"With magical and frightening pin-point accuracy, {{user}} throws {{user:his/her/its}} weapon directly through {{target}}'s body. It then returns to {{user}} like a boomerang.",
          :effects=>%{
            :damage=>0.15,
            :damage_over_time=>%{
              :damage=>0.06,
              :duration=>3.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-25,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"A powerful energy field appears around you. {{target}} tries to attack, but is only beaten back by the energy of the field!",
          :target_message=>"A powerful energy field appears around {{user}}. You tries to attack, but are only beaten back by the energy of the field!",
          :spectator_message=>"A powerful energy field appears around {{user}}. {{target}} tries to attack, but is only beaten back by the energy of the field!",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>6.0
            }
          }
        },
        %{
          :user_message=>"With a large volley of energy from you, {{target}} is stunned for quite some time.",
          :target_message=>"With a large volley of energy from {{user}}, you are stunned for quite some time.",
          :spectator_message=>"With a large volley of energy from {{user}}, {{target}} is stunned for quite some time.",
          :effects=>%{
            :stun=>6.0
          }
        },
        %{
          :user_message=>"To {{target:his/her/its}} utter terror, you magically dissolve several of {{target}}'s favorite appendages!",
          :target_message=>"To your utter terror, {{user}} magically dissolves several of your favorite appendages!",
          :spectator_message=>"To {{target:his/her/its}} utter terror, {{user}} magically dissolves several of {{target}}'s favorite appendages!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"arm"
              },
              %{
                :kind=>"sever",
                :limb=>"arm"
              },
              %{
                :kind=>"cripple",
                :limb=>"non_fatal"
              }
            ]
          }
        },
        %{
          :user_message=>"You show off your incredible aether abilities by putting on a magical fireworks display.",
          :target_message=>"{{user}} shows off {{user:his/her/its}} incredible aether abilities by putting on a magical fireworks display.",
          :spectator_message=>"{{user}} shows off {{user:his/her/its}} incredible aether abilities by putting on a magical fireworks display.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You disappear, only to reappear momentarily INSIDE {{target}}'s body! The mess created is quite disgusting.",
          :target_message=>"{{user}} disappears, only to reappear momentarily INSIDE your body! The mess created is quite disgusting.",
          :spectator_message=>"{{user}} disappears, only to reappear momentarily INSIDE {{target}}'s body! The mess created is quite disgusting.",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "B"=>[
        %{
          :user_message=>"Your aether attack misses {{target}} completely!",
          :target_message=>"{{user}}'s aether attack misses you completely!",
          :spectator_message=>"{{user}}'s aether attack misses {{target}} completely!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You release a large energy blast from your weapon in {{target}}'s direction.",
          :target_message=>"{{user}} releases a large energy blast from {{user:his/her/its}} weapon in your direction.",
          :spectator_message=>"{{user}} releases a large energy blast from {{user:his/her/its}} weapon in {{target}}'s direction.",
          :effects=>%{
            :damage=>0.15
          }
        },
        %{
          :user_message=>"With energy blazing from your body, you charge {{target}} at lightning speed.",
          :target_message=>"With energy blazing from {{user}}'s body, {{user}} charges you at lightning speed.",
          :spectator_message=>"With energy blazing from {{user}}'s body, {{user}} charges {{target}} at lightning speed.",
          :effects=>%{
            :damage=>0.15,
            :damage_over_time=>%{
              :damage=>0.04,
              :duration=>2.0
            },
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-20,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"You teleport behind {{target}}. The maneuver gets you a few solid hits.",
          :target_message=>"{{user}} teleports behind you. The maneuver gets {{user:him/her/it}} a few solid hits.",
          :spectator_message=>"{{user}} teleports behind {{target}}. The maneuver gets {{user:him/her/it}} a few solid hits.",
          :effects=>%{
            :damage=>0.11,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-50,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"You stab your weapon into the ground, magically causing {{target}} to go into massive convulsions.",
          :target_message=>"{{user}} stabs {{user:his/her/its}} weapon into the ground, magically causing you to go into massive convulsions.",
          :spectator_message=>"{{user}} stabs {{user:his/her/its}} weapon into the ground, magically causing {{target}} to go into massive convulsions.",
          :effects=>%{
            :stun=>5.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-35,
                :duration=>16.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.01,
              :duration=>30.0
            }
          }
        },
        %{
          :user_message=>"A massive amount of light is released by you, causing {{target}} to go blind!",
          :target_message=>"A massive amount of light is released by {{user}}, causing you to go blind!",
          :spectator_message=>"A massive amount of light is released by {{user}}, causing {{target}} to go blind!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"{{target}} is momentarily paralyzed by your magical abilities.",
          :target_message=>"You are momentarily paralyzed by {{user}}'s magical abilities.",
          :spectator_message=>"{{target}} is momentarily paralyzed by {{user}}'s magical abilities.",
          :effects=>%{
            :stun=>4.0
          }
        },
        %{
          :user_message=>"With god-like ease you let loose an energy ball which cripples {{target}}.",
          :target_message=>"With god-like ease {{user}} lets loose an energy ball which cripples you.",
          :spectator_message=>"With god-like ease {{user}} lets loose an energy ball which cripples {{target}}.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              },
              %{
                :kind=>"cripple",
                :limb=>"non_fatal"
              }
            ]
          }
        },
        %{
          :user_message=>"You empower {{target}} with a powerful inter dimensional attack, severely weakening {{target:him/her/it}}.",
          :target_message=>"{{user}} empowers you with a powerful inter dimensional attack, severely weakening you.",
          :spectator_message=>"{{user}} empowers {{target}} with a powerful inter dimensional attack, severely weakening {{target:him/her/it}}.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You open up a dimensional hole INSIDE {{target}}'s stomach. The hole slowly but surely works away at {{target:his/her/its}} innards.",
          :target_message=>"{{user}} opens up a dimensional hole INSIDE your stomach. The hole slowly but surely works away at your innards.",
          :spectator_message=>"{{user}} opens up a dimensional hole INSIDE {{target}}'s stomach. The hole slowly but surely works away at {{target:his/her/its}} innards.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>200.0
            }
          }
        },
        %{
          :user_message=>"With the wave of your hand you disintegrate {{target}}'s entire inventory!",
          :target_message=>"With the wave of {{user:his/her/its}} hand {{user}} disintegrates your entire inventory!",
          :spectator_message=>"With the wave of {{user:his/her/its}} hand {{user}} disintegrates {{target}}'s entire inventory!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"Your massive energy attack knocks {{target}} back into another room",
          :target_message=>"{{user}}'s massive energy attack knocks you back into another room",
          :spectator_message=>"{{user}}'s massive energy attack knocks {{target}} back into another room",
          :effects=>%{
            :damage=>0.6
          }
        },
        %{
          :user_message=>"You cause {{target}}'s own arm to stab {{target}}self before it falls off.",
          :target_message=>"{{user}} causes your own arm to stab youself before it falls off.",
          :spectator_message=>"{{user}} causes {{target}}'s own arm to stab {{target}}self before it falls off.",
          :effects=>%{
            :damage=>0.3,
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"arm"
              }
            ],
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
          :user_message=>"With an incredible blast of energy, you render {{target}} very helpless.",
          :target_message=>"With an incredible blast of energy, {{user}} renders you very helpless.",
          :spectator_message=>"With an incredible blast of energy, {{user}} renders {{target}} very helpless.",
          :effects=>%{
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-70,
                :duration=>6.0
              },
              %{
                :skill=>"dodge",
                :amount=>-80,
                :duration=>4.0
              },
              %{
                :skill=>"parry",
                :amount=>-80,
                :duration=>8.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-40,
                :duration=>6.0
              },
              %{
                :stat=>"willpower",
                :amount=>-40,
                :duration=>4.0
              },
              %{
                :stat=>"intellect",
                :amount=>-50,
                :duration=>3.0
              }
            ]
          }
        },
        %{
          :user_message=>"The environment itself becomes hostile to {{target}}. It turns into a large blob like abomination which tears off some of {{target:his/her/its}} limbs.",
          :target_message=>"The environment itself becomes hostile to you. It turns into a large blob like abomination which tears off some of your limbs.",
          :spectator_message=>"The environment itself becomes hostile to {{target}}. It turns into a large blob like abomination which tears off some of {{target:his/her/its}} limbs.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              },
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              },
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              }
            ]
          }
        },
        %{
          :user_message=>"With a magical blade you pulled out of thin air, you relieve {{target}} of {{target:his/her/its}} head.",
          :target_message=>"With a magical blade {{user:he/she/it}} pulled out of thin air, {{user}} relieves you of your head.",
          :spectator_message=>"With a magical blade {{user:he/she/it}} pulled out of thin air, {{user}} relieves {{target}} of {{target:his/her/its}} head.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"head"
              }
            ]
          }
        },
        %{
          :user_message=>"Bending into a very dramatic pose, you blast {{target}} with enough energy to severely mutilate {{target:him/her/it}}.",
          :target_message=>"Bending into a very dramatic pose, {{user}} blasts you with enough energy to severely mutilate you.",
          :spectator_message=>"Bending into a very dramatic pose, {{user}} blasts {{target}} with enough energy to severely mutilate {{target:him/her/it}}.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              },
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              },
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              },
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              },
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              }
            ]
          }
        }
      ],
      "C"=>[
        %{
          :user_message=>"You create some small explosions in the distance, but they have little effect on {{target}}.",
          :target_message=>"{{user}} creates some small explosions in the distance, but they have little effect on you.",
          :spectator_message=>"{{user}} creates some small explosions in the distance, but they have little effect on {{target}}.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You bend light around {{target}} to make it very bright around {{target:him/her/it}}!",
          :target_message=>"{{user}} bends light around you to make it very bright around you!",
          :spectator_message=>"{{user}} bends light around {{target}} to make it very bright around {{target:him/her/it}}!",
          :effects=>%{
            :damage=>0.05
          }
        },
        %{
          :user_message=>"You bend light around {{target}} to make it incredibly dark around {{target:him/her/it}}!",
          :target_message=>"{{user}} bends light around you to make it incredibly dark around you!",
          :spectator_message=>"{{user}} bends light around {{target}} to make it incredibly dark around {{target:him/her/it}}!",
          :effects=>%{
            :damage=>0.06
          }
        },
        %{
          :user_message=>"You pound your weapon against the ground, so shaking {{target}} that {{target:his/her/its}} entire inventory falls to the ground.",
          :target_message=>"{{user}} pounds {{user:his/her/its}} weapon against the ground, so shaking you that your entire inventory falls to the ground.",
          :spectator_message=>"{{user}} pounds {{user:his/her/its}} weapon against the ground, so shaking {{target}} that {{target:his/her/its}} entire inventory falls to the ground.",
          :effects=>%{
            :damage=>0.12
          }
        },
        %{
          :user_message=>"You disturb reality inside {{target}}'s brain, severely impairing {{target:his/her/its}} intelligence.",
          :target_message=>"{{user}} disturbs reality inside your brain, severely impairing your intelligence.",
          :spectator_message=>"{{user}} disturbs reality inside {{target}}'s brain, severely impairing {{target:his/her/its}} intelligence.",
          :effects=>%{
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-50,
                :duration=>8.0
              },
              %{
                :stat=>"willpower",
                :amount=>-50,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"Bending reality at your will, you rid {{target}} of {{target:his/her/its}} armour.",
          :target_message=>"Bending reality at {{user:his/her/its}} will, {{user}} rids you of your armour.",
          :spectator_message=>"Bending reality at {{user:his/her/its}} will, {{user}} rids {{target}} of {{target:his/her/its}} armour.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"The sky grows dark as you pump {{target}} full of energy blasts.",
          :target_message=>"The sky grows dark as {{user}} pumps you full of energy blasts.",
          :spectator_message=>"The sky grows dark as {{user}} pumps {{target}} full of energy blasts.",
          :effects=>%{
            :damage=>0.6,
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"non_fatal"
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} realizes with horror that your control of the realities has robbed {{target:him/her/it}} of all {{target:his/her/its}} money.",
          :target_message=>"You realize with horror that {{user}}'s control of the realities has robbed you of all your money.",
          :spectator_message=>"{{target}} realizes with horror that {{user}}'s control of the realities has robbed {{target:him/her/it}} of all {{target:his/her/its}} money.",
          :effects=>%{
            :damage=>0.05
          }
        },
        %{
          :user_message=>"You drain part of {{target}} life force, keeping it for youself.",
          :target_message=>"{{user}} drains part of you life force, keeping it for {{user}}self.",
          :spectator_message=>"{{user}} drains part of {{target}} life force, keeping it for {{user}}self.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You cause the reality around {{target}} to spin furiously, leaving {{target:him/her/it}} helpless.",
          :target_message=>"{{user}} causes the reality around you to spin furiously, leaving you helpless.",
          :spectator_message=>"{{user}} causes the reality around {{target}} to spin furiously, leaving {{target:him/her/it}} helpless.",
          :effects=>%{
            :stun=>8.0,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-40,
                :duration=>30.0
              },
              %{
                :skill=>"attack",
                :amount=>-30,
                :duration=>26.0
              }
            ]
          }
        },
        %{
          :user_message=>"You alter your own molecular state to grow to an enormous size. It is now easy for you to trounce {{target}}.",
          :target_message=>"{{user}} alters {{user:his/her/its}} own molecular state to grow to an enormous size. It is now easy for {{user:him/her/it}} to trounce you.",
          :spectator_message=>"{{user}} alters {{user:his/her/its}} own molecular state to grow to an enormous size. It is now easy for {{user:him/her/it}} to trounce {{target}}.",
          :effects=>%{
            :damage=>0.3,
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>10.0
            }
          }
        },
        %{
          :user_message=>"With unsurpassable power you slam {{target}} to the ground without even touching {{target:him/her/it}}!",
          :target_message=>"With unsurpassable power {{user}} slams you to the ground without even touching you!",
          :spectator_message=>"With unsurpassable power {{user}} slams {{target}} to the ground without even touching {{target:him/her/it}}!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"non_fatal"
              },
              %{
                :kind=>"cripple",
                :limb=>"non_fatal"
              },
              %{
                :kind=>"cripple",
                :limb=>"non_fatal"
              },
              %{
                :kind=>"cripple",
                :limb=>"non_fatal"
              }
            ]
          }
        },
        %{
          :user_message=>"You use your aether abilities to make everyone smile. WHEEE!",
          :target_message=>"{{user}} uses {{user:his/her/its}} aether abilities to make everyone smile. WHEEE!",
          :spectator_message=>"{{user}} uses {{user:his/her/its}} aether abilities to make everyone smile. WHEEE!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You open {{target}}'s stomach with an energy blade. The wound may never heal!",
          :target_message=>"{{user}} opens your stomach with an energy blade. The wound may never heal!",
          :spectator_message=>"{{user}} opens {{target}}'s stomach with an energy blade. The wound may never heal!",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>400.0
            }
          }
        },
        %{
          :user_message=>"A black hole is created by you! It makes very quick work of {{target}}.",
          :target_message=>"A black hole is created by {{user}}! It makes very quick work of you.",
          :spectator_message=>"A black hole is created by {{user}}! It makes very quick work of {{target}}.",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "D"=>[
        %{
          :user_message=>"Your attempt to wreak havoc on {{target}} fails!",
          :target_message=>"{{user}}'s attempt to wreak havoc on you fails!",
          :spectator_message=>"{{user}}'s attempt to wreak havoc on {{target}} fails!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You temporarily remove all thoughts from {{target}}'s mind.",
          :target_message=>"{{user}} temporarily removes all thoughts from your mind.",
          :spectator_message=>"{{user}} temporarily removes all thoughts from {{target}}'s mind.",
          :effects=>%{
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-100,
                :duration=>24.0
              },
              %{
                :skill=>"attack",
                :amount=>-75,
                :duration=>22.0
              },
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>22.0
              }
            ],
            :damage=>0.06
          }
        },
        %{
          :user_message=>"With pure power flowing from your body, you stab {{target}}, pumping enough energy through {{target:him/her/it}} to raise the dead.",
          :target_message=>"With pure power flowing from {{user:his/her/its}} body, {{user}} stabs you, pumping enough energy through you to raise the dead.",
          :spectator_message=>"With pure power flowing from {{user:his/her/its}} body, {{user}} stabs {{target}}, pumping enough energy through {{target:him/her/it}} to raise the dead.",
          :effects=>%{
            :damage=>0.35
          }
        },
        %{
          :user_message=>"You release all H20 from {{target}}'s body, rendering {{target:him/her/it}} stunned and badly injured.",
          :target_message=>"{{user}} releases all H20 from your body, rendering you stunned and badly injured.",
          :spectator_message=>"{{user}} releases all H20 from {{target}}'s body, rendering {{target:him/her/it}} stunned and badly injured.",
          :effects=>%{
            :damage=>0.4,
            :stun=>2.0
          }
        },
        %{
          :user_message=>"The gravitational pull around {{target}} suddenly becomes unbearably strong. You laugh nearby.",
          :target_message=>"The gravitational pull around you suddenly becomes unbearably strong. {{user}} laughs nearby.",
          :spectator_message=>"The gravitational pull around {{target}} suddenly becomes unbearably strong. {{user}} laughs nearby.",
          :effects=>%{
            :damage=>0.1,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"With a mere thought you seriously throw off {{target}}'s equilibrium.",
          :target_message=>"With a mere thought {{user}} seriously throws off your equilibrium.",
          :spectator_message=>"With a mere thought {{user}} seriously throws off {{target}}'s equilibrium.",
          :effects=>%{
            :damage=>0.16,
            :stun=>8.0,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-50,
                :duration=>30.0
              },
              %{
                :skill=>"parry",
                :amount=>-60,
                :duration=>40.0
              },
              %{
                :skill=>"attack",
                :amount=>-100,
                :duration=>20.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-50,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"A blast of energy from you instantaneously changes much of {{target}}'s blood into a dangerous toxin!",
          :target_message=>"A blast of energy from {{user}} instantaneously changes much of your blood into a dangerous toxin!",
          :spectator_message=>"A blast of energy from {{user}} instantaneously changes much of {{target}}'s blood into a dangerous toxin!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"Spewing energy fields, you wave your weapon in the air. An unworldly creature suddenly appears and attacks {{target}}!",
          :target_message=>"Spewing energy fields, {{user}} waves {{user:his/her/its}} weapon in the air. An unworldly creature suddenly appears and attacks you!",
          :spectator_message=>"Spewing energy fields, {{user}} waves {{user:his/her/its}} weapon in the air. An unworldly creature suddenly appears and attacks {{target}}!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"After shrinking {{target:him/her/it}} to the size of a thimble, you squash {{target}} like a bug.",
          :target_message=>"After shrinking you to the size of a thimble, {{user}} squashes you like a bug.",
          :spectator_message=>"After shrinking {{target:him/her/it}} to the size of a thimble, {{user}} squashes {{target}} like a bug.",
          :effects=>%{
            :damage=>0.99
          }
        },
        %{
          :user_message=>"You raise your hand into the air, sending {{target}} flying far.",
          :target_message=>"{{user}} raises {{user:his/her/its}} hand into the air, sending you flying far.",
          :spectator_message=>"{{user}} raises {{user:his/her/its}} hand into the air, sending {{target}} flying far.",
          :effects=>%{
            :damage=>0.6,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>10.0
            },
            :stun=>6.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-15,
                :duration=>40.0
              }
            ]
          }
        },
        %{
          :user_message=>"You drag several of {{target}}'s limbs into parallel dimensions.",
          :target_message=>"{{user}} drags several of your limbs into parallel dimensions.",
          :spectator_message=>"{{user}} drags several of {{target}}'s limbs into parallel dimensions.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              },
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              },
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              },
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              }
            ],
            :damage=>0.05
          }
        },
        %{
          :user_message=>"{{target}}'s body temperature is raised to dangerously high levels by your energy fields.",
          :target_message=>"Your body temperature is raised to dangerously high levels by {{user}}'s energy fields.",
          :spectator_message=>"{{target}}'s body temperature is raised to dangerously high levels by {{user}}'s energy fields.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>100.0
            }
          }
        },
        %{
          :user_message=>"You take control of the natural elements in your relentless attacks on {{target}}!",
          :target_message=>"{{user}} takes control of the natural elements in {{user:his/her/its}} relentless attacks on you!",
          :spectator_message=>"{{user}} takes control of the natural elements in {{user:his/her/its}} relentless attacks on {{target}}!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"{{target}} is totally removed from all planes of existence by you. No corpse is left.",
          :target_message=>"You are totally removed from all planes of existence by {{user}}. No corpse is left.",
          :spectator_message=>"{{target}} is totally removed from all planes of existence by {{user}}. No corpse is left.",
          :effects=>%{

          }
        }
      ],
      "E"=>[
        %{
          :user_message=>"HA! your feeble attempt to cause {{target}} physical harm fails miserably!",
          :target_message=>"HA! {{user}}'s feeble attempt to cause you physical harm fails miserably!",
          :spectator_message=>"HA! {{user}}'s feeble attempt to cause {{target}} physical harm fails miserably!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"{{target}} is blasted by you with energies from the ethereal plane.",
          :target_message=>"You are blasted by {{user}} with energies from the ethereal plane.",
          :spectator_message=>"{{target}} is blasted by {{user}} with energies from the ethereal plane.",
          :effects=>%{
            :damage=>0.22,
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
          :user_message=>"You use your powers to violently rattle {{target}} for several minutes.",
          :target_message=>"{{user}} uses {{user:his/her/its}} powers to violently rattle you for several minutes.",
          :spectator_message=>"{{user}} uses {{user:his/her/its}} powers to violently rattle {{target}} for several minutes.",
          :effects=>%{
            :stun=>2.0,
            :damage=>0.1,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-40,
                :duration=>30.0
              },
              %{
                :skill=>"dodge",
                :amount=>-40,
                :duration=>100.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-25,
                :duration=>30.0
              }
            ]
          }
        },
        %{
          :user_message=>"The ground suddenly swallows {{target}} up. After a bit, {{target:he/she/it}} is spit high into the air, landing with a loud THUD.",
          :target_message=>"The ground suddenly swallows you up. After a bit, you are spit high into the air, landing with a loud THUD.",
          :spectator_message=>"The ground suddenly swallows {{target}} up. After a bit, {{target:he/she/it}} is spit high into the air, landing with a loud THUD.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>15.0
            },
            :damage=>0.2,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>30.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"charm",
                :amount=>-60,
                :duration=>60.0
              }
            ]
          }
        },
        %{
          :user_message=>"You rip a hole in the air with your weapon. From this hole comes a large behemoth of a beast who attacks you!",
          :target_message=>"{{user}} rips a hole in the air with {{user:his/her/its}} weapon. From this hole comes a large behemoth of a beast who attacks {{user}}!",
          :spectator_message=>"{{user}} rips a hole in the air with {{user:his/her/its}} weapon. From this hole comes a large behemoth of a beast who attacks {{user}}!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You paralyze {{target}} with magical energies so you can easily stab {{target:him/her/it}} innumerable of times.",
          :target_message=>"{{user}} paralyzes you with magical energies so {{user:he/she/it}} can easily stab you innumerable of times.",
          :spectator_message=>"{{user}} paralyzes {{target}} with magical energies so {{user:he/she/it}} can easily stab {{target:him/her/it}} innumerable of times.",
          :effects=>%{
            :stun=>15.0
          }
        },
        %{
          :user_message=>"Totally under your magical control, {{target}} is raised into the air and dropped hard to the ground hundreds of times.",
          :target_message=>"Totally under {{user}}'s magical control, you are raised into the air and dropped hard to the ground hundreds of times.",
          :spectator_message=>"Totally under {{user}}'s magical control, {{target}} is raised into the air and dropped hard to the ground hundreds of times.",
          :effects=>%{
            :damage=>3.0
          }
        },
        %{
          :user_message=>"You make {{target}} a space-time continuum anomaly, causing {{target:him/her/it}} great harm for as long as life is a part of {{target:him/her/it}}.",
          :target_message=>"{{user}} makes you a space-time continuum anomaly, causing you great harm for as long as life is a part of you.",
          :spectator_message=>"{{user}} makes {{target}} a space-time continuum anomaly, causing {{target:him/her/it}} great harm for as long as life is a part of {{target:him/her/it}}.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.2,
              :duration=>50.0
            }
          }
        },
        %{
          :user_message=>"You raise your hands and {{target}}'s body parts like the waters of the Sea of Reeds.",
          :target_message=>"{{user}} raises {{user:his/her/its}} hands and your body parts like the waters of the Sea of Reeds.",
          :spectator_message=>"{{user}} raises {{user:his/her/its}} hands and {{target}}'s body parts like the waters of the Sea of Reeds.",
          :effects=>%{
            :damage=>6.0
          }
        },
        %{
          :user_message=>"The sudden occurrence of a large temporal disturbance casts large shadows of doubt on {{target}}'s survival.",
          :target_message=>"The sudden occurrence of a large temporal disturbance casts large shadows of doubt on your survival.",
          :spectator_message=>"The sudden occurrence of a large temporal disturbance casts large shadows of doubt on {{target}}'s survival.",
          :effects=>%{
            :stun=>10.0,
            :damage=>1.0,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-50,
                :duration=>30.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} is banished to another dimension by you!!",
          :target_message=>"You are banished to another dimension by {{user}}!!",
          :spectator_message=>"{{target}} is banished to another dimension by {{user}}!!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"The sky turns dark and thunder crashes as you $V your masterful control of the world to brutally mutilate {{target}}.",
          :target_message=>"The sky turns dark and thunder crashes as {{user}} $V {{user:his/her/its}} masterful control of the world to brutally mutilate you.",
          :spectator_message=>"The sky turns dark and thunder crashes as {{user}} $V {{user:his/her/its}} masterful control of the world to brutally mutilate {{target}}.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              },
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              },
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              },
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              },
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              },
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              },
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              },
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              }
            ]
          }
        },
        %{
          :user_message=>"The ground opens up into a large abyss underneath {{target}} at your will. {{target}} falls to the depths of hell.",
          :target_message=>"The ground opens up into a large abyss underneath you at {{user}}'s will. You fall to the depths of hell.",
          :spectator_message=>"The ground opens up into a large abyss underneath {{target}} at {{user}}'s will. {{target}} falls to the depths of hell.",
          :effects=>%{
            :kill=>true
          }
        }
      ]
    }  end

end
