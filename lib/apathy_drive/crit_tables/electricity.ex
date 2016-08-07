defmodule ApathyDrive.CritTables.Electricity do

  def name do
    "electricity"
  end

  def damage_type do
    "magical"
  end

  def crits do
    %{
      "A"=>[
        %{
          :user_message=>"You startle {{target}} with an electrical shock.",
          :target_message=>"{{user}} startles you with an electrical shock.",
          :spectator_message=>"{{user}} startles {{target}} with an electrical shock.",
          :effects=>%{
            :damage=>0.01
          }
        },
        %{
          :user_message=>"You shock {{target}}, who becomes stiff momentarily.",
          :target_message=>"{{user}} shocks you, who becomes stiff momentarily.",
          :spectator_message=>"{{user}} shocks {{target}}, who becomes stiff momentarily.",
          :effects=>%{
            :damage=>0.03
          }
        },
        %{
          :user_message=>"You jolt {{target}} with electricity.",
          :target_message=>"{{user}} jolts you with electricity.",
          :spectator_message=>"{{user}} jolts {{target}} with electricity.",
          :effects=>%{
            :stun=>1.0,
            :damage=>0.02,
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
          :user_message=>"You shock {{target}} nastily.",
          :target_message=>"{{user}} shocks you nastily.",
          :spectator_message=>"{{user}} shocks {{target}} nastily.",
          :effects=>%{
            :damage=>0.04,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-15,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your electrical current jolts through {{target}}.",
          :target_message=>"{{user}}'s electrical current jolts through you.",
          :spectator_message=>"{{user}}'s electrical current jolts through {{target}}.",
          :effects=>%{
            :stun=>1.0,
            :damage=>0.05,
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
          :user_message=>"You fiercely shock {{target}}.",
          :target_message=>"{{user}} fiercely shocks you.",
          :spectator_message=>"{{user}} fiercely shocks {{target}}.",
          :effects=>%{
            :stun=>2.0,
            :damage=>0.07,
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
          :user_message=>"You shock {{target}}, who contorts wildly in pain.",
          :target_message=>"{{user}} shocks you, who contorts wildly in pain.",
          :spectator_message=>"{{user}} shocks {{target}}, who contorts wildly in pain.",
          :effects=>%{
            :damage=>0.08,
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-20,
                :duration=>10.0
              }
            ],
            :skill_mod=>[
              %{
                :skill=>"block",
                :amount=>-20,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}}'s eyes bulge and {{target:his/her/its}} limbs flail as you shock {{target:him/her/it}}.",
          :target_message=>"Your eyes bulge and your limbs flail as {{user}} shocks you.",
          :spectator_message=>"{{target}}'s eyes bulge and {{target:his/her/its}} limbs flail as {{user}} shocks {{target:him/her/it}}.",
          :effects=>%{
            :stun=>6.0,
            :damage=>0.15,
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
          :user_message=>"You wickedly shock {{target}}.",
          :target_message=>"{{user}} wickedly shocks you.",
          :spectator_message=>"{{user}} wickedly shocks {{target}}.",
          :effects=>%{
            :stun=>3.0,
            :damage=>0.09,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-25,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"You pump many volts into {{target}}, disrupting {{target:his/her/its}} nervous system.",
          :target_message=>"{{user}} pumps many volts into you, disrupting your nervous system.",
          :spectator_message=>"{{user}} pumps many volts into {{target}}, disrupting {{target:his/her/its}} nervous system.",
          :effects=>%{
            :stun=>4.0,
            :damage=>0.15,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-25,
                :duration=>16.0
              }
            ]
          }
        },
        %{
          :user_message=>"You relentlessly shock {{target}}, who falls down twitching.",
          :target_message=>"{{user}} relentlessly shocks you, who falls down twitching.",
          :spectator_message=>"{{user}} relentlessly shocks {{target}}, who falls down twitching.",
          :effects=>%{
            :stun=>5.0,
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>7.0
            },
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-20,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your electricity racks {{target}}, who will just babble for awhile.",
          :target_message=>"{{user}}'s electricity racks you, who will just babble for awhile.",
          :spectator_message=>"{{user}}'s electricity racks {{target}}, who will just babble for awhile.",
          :effects=>%{
            :stun=>7.0,
            :damage=>0.2,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-30,
                :duration=>20.0
              },
              %{
                :skill=>"parry",
                :amount=>-30,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"You rewire {{target}}'s brain a little with a powerful jolt.",
          :target_message=>"{{user}} rewires your brain a little with a powerful jolt.",
          :spectator_message=>"{{user}} rewires {{target}}'s brain a little with a powerful jolt.",
          :effects=>%{
            :stun=>10.0,
            :damage=>0.32,
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-25,
                :duration=>24.0
              },
              %{
                :stat=>"agility",
                :amount=>-25,
                :duration=>20.0
              }
            ]
          }
        }
      ],
      "B"=>[
        %{
          :user_message=>"You give {{target}} a nasty shock.",
          :target_message=>"{{user}} gives you a nasty shock.",
          :spectator_message=>"{{user}} gives {{target}} a nasty shock.",
          :effects=>%{
            :damage=>0.02,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-10,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"You jolt {{target}} with potent electricity.",
          :target_message=>"{{user}} jolts you with potent electricity.",
          :spectator_message=>"{{user}} jolts {{target}} with potent electricity.",
          :effects=>%{
            :damage=>0.04,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-10,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} stiffens as you juice {{target:him/her/it}} with electricity.",
          :target_message=>"You stiffens as {{user}} juices you with electricity.",
          :spectator_message=>"{{target}} stiffens as {{user}} juices {{target:him/her/it}} with electricity.",
          :effects=>%{
            :stun=>1.0,
            :damage=>0.05,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-5,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"You zap {{target}}, whose heart stops for a moment.",
          :target_message=>"{{user}} zaps you, whose heart stops for a moment.",
          :spectator_message=>"{{user}} zaps {{target}}, whose heart stops for a moment.",
          :effects=>%{
            :stun=>1.0,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>2.0
            },
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-20,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"You stun {{target}} with electrical power.",
          :target_message=>"{{user}} stuns you with electrical power.",
          :spectator_message=>"{{user}} stuns {{target}} with electrical power.",
          :effects=>%{
            :stun=>3.0,
            :damage=>0.05,
            :skill_mod=>[
              %{
                :skill=>nil,
                :amount=>25,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"You nastily shock {{target}}, causing {{target:him/her/it}} to shake violently.",
          :target_message=>"{{user}} nastily shocks you, causing you to shake violently.",
          :spectator_message=>"{{user}} nastily shocks {{target}}, causing {{target:him/her/it}} to shake violently.",
          :effects=>%{
            :damage=>0.09,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-25,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"You jolt {{target}}, making {{target:him/her/it}} shriek and flail.",
          :target_message=>"{{user}} jolts you, making you shriek and flail.",
          :spectator_message=>"{{user}} jolts {{target}}, making {{target:him/her/it}} shriek and flail.",
          :effects=>%{
            :damage=>0.15,
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
          :user_message=>"When you shock {{target}}, {{target:him/her/it}} limbs stiffen so roughly {{target:him/her/it}} joints are unsocketed.",
          :target_message=>"When {{user}} shocks you, you limbs stiffen so roughly you joints are unsocketed.",
          :spectator_message=>"When {{user}} shocks {{target}}, {{target:him/her/it}} limbs stiffen so roughly {{target:him/her/it}} joints are unsocketed.",
          :effects=>%{
            :stun=>5.0,
            :damage=>0.26,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-15,
                :duration=>20.0
              }
            ],
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ]
          }
        },
        %{
          :user_message=>"You knock {{target}} to the ground with electrical energy.",
          :target_message=>"{{user}} knocks you to the ground with electrical energy.",
          :spectator_message=>"{{user}} knocks {{target}} to the ground with electrical energy.",
          :effects=>%{
            :stun=>3.0,
            :damage=>0.18,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-25,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"You jolt {{target}} with a disruptive stream of energy.",
          :target_message=>"{{user}} jolts you with a disruptive stream of energy.",
          :spectator_message=>"{{user}} jolts {{target}} with a disruptive stream of energy.",
          :effects=>%{
            :stun=>4.0,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>7.0
            },
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-30,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"You WICKEDLY shock {{target}}, who plops down to the floor!",
          :target_message=>"{{user}} WICKEDLY shocks you, who plops down to the floor!",
          :spectator_message=>"{{user}} WICKEDLY shocks {{target}}, who plops down to the floor!",
          :effects=>%{
            :stun=>8.0,
            :damage=>0.27,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-20,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"You send streams of electricity through {{target}} that shake {{target:him/her/it}}, then drop him to the ground!",
          :target_message=>"{{user}} sends streams of electricity through you that shake you, then drop him to the ground!",
          :spectator_message=>"{{user}} sends streams of electricity through {{target}} that shake {{target:him/her/it}}, then drop him to the ground!",
          :effects=>%{
            :stun=>9.0,
            :damage=>0.34,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-40,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"You light up {{target}} with electrical power that leaves {{target:him/her/it}} writhing on the ground!",
          :target_message=>"{{user}} lights up you with electrical power that leaves you writhing on the ground!",
          :spectator_message=>"{{user}} lights up {{target}} with electrical power that leaves {{target:him/her/it}} writhing on the ground!",
          :effects=>%{
            :stun=>15.0,
            :damage=>0.75,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-35,
                :duration=>30.0
              }
            ]
          }
        }
      ],
      "C"=>[
        %{
          :user_message=>"You shock {{target}} into a momentary stupor.",
          :target_message=>"{{user}} shocks you into a momentary stupor.",
          :spectator_message=>"{{user}} shocks {{target}} into a momentary stupor.",
          :effects=>%{
            :damage=>0.02
          }
        },
        %{
          :user_message=>"You shake {{target}} up with powerful electricity.",
          :target_message=>"{{user}} shakes you up with powerful electricity.",
          :spectator_message=>"{{user}} shakes {{target}} up with powerful electricity.",
          :effects=>%{
            :damage=>0.06,
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
          :user_message=>"{{target}} convulses wildly with the force of your electricity.",
          :target_message=>"You convulse wildly with the force of {{user}}'s electricity.",
          :spectator_message=>"{{target}} convulses wildly with the force of {{user}}'s electricity.",
          :effects=>%{
            :damage=>0.1,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-25,
                :duration=>14.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your electrical onslaught makes {{target}} dance grotesquely, smoking.",
          :target_message=>"{{user}}'s electrical onslaught makes you dance grotesquely, smoking.",
          :spectator_message=>"{{user}}'s electrical onslaught makes {{target}} dance grotesquely, smoking.",
          :effects=>%{
            :stun=>3.0,
            :damage=>0.13,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-45,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"You shoot frightening electrical force through {{target}}, who then goes limp.",
          :target_message=>"{{user}} shoots frightening electrical force through you, who then goes limp.",
          :spectator_message=>"{{user}} shoots frightening electrical force through {{target}}, who then goes limp.",
          :effects=>%{
            :stun=>5.0,
            :damage=>0.15,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-15,
                :duration=>30.0
              }
            ]
          }
        },
        %{
          :user_message=>"You ZAP {{target}}, who jumps up involuntarily and falls in a daze.",
          :target_message=>"{{user}} ZAPs you, who jumps up involuntarily and falls in a daze.",
          :spectator_message=>"{{user}} ZAPs {{target}}, who jumps up involuntarily and falls in a daze.",
          :effects=>%{
            :stun=>6.0,
            :damage=>0.21,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-35,
                :duration=>40.0
              },
              %{
                :skill=>"dodge",
                :amount=>-35,
                :duration=>30.0
              }
            ]
          }
        },
        %{
          :user_message=>"You shock {{target}} until {{target}}'s whole body is stiff as a board.",
          :target_message=>"{{user}} shocks you until your whole body is stiff as a board.",
          :spectator_message=>"{{user}} shocks {{target}} until {{target}}'s whole body is stiff as a board.",
          :effects=>%{
            :stun=>8.0,
            :damage=>0.25,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-55,
                :duration=>30.0
              },
              %{
                :skill=>"dodge",
                :amount=>-55,
                :duration=>30.0
              }
            ]
          }
        },
        %{
          :user_message=>"You jolt {{target}} until smoke comes out of {{target:him/her/it}} hair, standing on end!",
          :target_message=>"{{user}} jolts you until smoke comes out of you hair, standing on end!",
          :spectator_message=>"{{user}} jolts {{target}} until smoke comes out of {{target:him/her/it}} hair, standing on end!",
          :effects=>%{
            :stun=>10.0,
            :damage=>0.39,
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-25,
                :duration=>60.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your lightning courses all aound {{target}}, making {{target:him/her/it}} cry out in agony!",
          :target_message=>"{{user}}'s lightning courses all aound you, making you cry out in agony!",
          :spectator_message=>"{{user}}'s lightning courses all aound {{target}}, making {{target:him/her/it}} cry out in agony!",
          :effects=>%{
            :stun=>3.0,
            :damage=>0.3,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-50,
                :duration=>36.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your crackling electricity blackens and boils {{target}}'s abdominal guts!",
          :target_message=>"{{user}}'s crackling electricity blackens and boils your abdominal guts!",
          :spectator_message=>"{{user}}'s crackling electricity blackens and boils {{target}}'s abdominal guts!",
          :effects=>%{
            :stun=>8.0,
            :damage=>0.54,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-45,
                :duration=>40.0
              }
            ]
          }
        },
        %{
          :user_message=>"You rock {{target}} with blinding purple lightning, leaving black, smoking rips in {{target}}'s flesh!",
          :target_message=>"{{user}} rocks you with blinding purple lightning, leaving black, smoking rips in your flesh!",
          :spectator_message=>"{{user}} rocks {{target}} with blinding purple lightning, leaving black, smoking rips in {{target}}'s flesh!",
          :effects=>%{
            :stun=>6.0,
            :damage_over_time=>%{
              :damage=>0.27,
              :duration=>3.0
            }
          }
        },
        %{
          :user_message=>"Your thundering electrical assault knocks {{target}} to the floor, smoking!",
          :target_message=>"{{user}}'s thundering electrical assault knocks you to the floor, smoking!",
          :spectator_message=>"{{user}}'s thundering electrical assault knocks {{target}} to the floor, smoking!",
          :effects=>%{
            :stun=>10.0,
            :damage=>0.9,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-70,
                :duration=>50.0
              }
            ]
          }
        },
        %{
          :user_message=>"You cause {{target}} to convulse in a lethal seizure with stunning lightning!",
          :target_message=>"{{user}} causes you to convulse in a lethal seizure with stunning lightning!",
          :spectator_message=>"{{user}} causes {{target}} to convulse in a lethal seizure with stunning lightning!",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "D"=>[
        %{
          :user_message=>"{{target}} is jolted by your electrical attack.",
          :target_message=>"You are jolted by {{user}}'s electrical attack.",
          :spectator_message=>"{{target}} is jolted by {{user}}'s electrical attack.",
          :effects=>%{
            :damage=>0.05
          }
        },
        %{
          :user_message=>"You cause {{target}} to convulse, and smoke rises from {{target:his/her/its}} head.",
          :target_message=>"{{user}} causes you to convulse, and smoke rises from your head.",
          :spectator_message=>"{{user}} causes {{target}} to convulse, and smoke rises from {{target:his/her/its}} head.",
          :effects=>%{
            :damage=>0.07,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-10,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"A lightning bolt from your attack slams into {{target}}'s chest, causing {{target:his/her/its}} eyes to widen and {{target:his/her/its}} body to shake convulsively.",
          :target_message=>"A lightning bolt from {{user}}'s attack slams into your chest, causing your eyes to widen and your body to shake convulsively.",
          :spectator_message=>"A lightning bolt from {{user}}'s attack slams into {{target}}'s chest, causing {{target:his/her/its}} eyes to widen and {{target:his/her/its}} body to shake convulsively.",
          :effects=>%{
            :damage=>0.12,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-20,
                :duration=>6.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"intelligence",
                :amount=>-10,
                :duration=>12.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}}'s eyes roll back into {{target:his/her/its}} head as a bolt from your attack strikes {{target}} directly in the crotch (OUCH!)",
          :target_message=>"Your eyes roll back into your head as a bolt from {{user}}'s attack strikes you directly in the crotch (OUCH!)",
          :spectator_message=>"{{target}}'s eyes roll back into {{target:his/her/its}} head as a bolt from {{user}}'s attack strikes {{target}} directly in the crotch (OUCH!)",
          :effects=>%{
            :damage=>0.17,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-20,
                :duration=>8.0
              }
            ],
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-30,
                :duration=>8.0
              },
              %{
                :skill=>"dodge",
                :amount=>-30,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"Tiny lightning bolts are seen in {{target}}'s mouth as a bolt from your attack strikes {{target:him/her/it}} in the torso.",
          :target_message=>"Tiny lightning bolts are seen in your mouth as a bolt from {{user}}'s attack strikes you in the torso.",
          :spectator_message=>"Tiny lightning bolts are seen in {{target}}'s mouth as a bolt from {{user}}'s attack strikes {{target:him/her/it}} in the torso.",
          :effects=>%{
            :damage=>0.28,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-40,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"Lightning bolts from your attack dance about {{target}}'s body, and {{target:him/her/it}} falls backward from the impact.",
          :target_message=>"Lightning bolts from {{user}}'s attack dance about your body, and you fall backward from the impact.",
          :spectator_message=>"Lightning bolts from {{user}}'s attack dance about {{target}}'s body, and {{target:him/her/it}} falls backward from the impact.",
          :effects=>%{
            :damage=>0.3,
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-45,
                :duration=>10.0
              },
              %{
                :skill=>"parry",
                :amount=>-20,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You strike {{target}} directly in the head with an electrical bolt, causing smoke to rise from {{target:his/her/its}} ears.",
          :target_message=>"{{user}} strikes you directly in the head with an electrical bolt, causing smoke to rise from your ears.",
          :spectator_message=>"{{user}} strikes {{target}} directly in the head with an electrical bolt, causing smoke to rise from {{target:his/her/its}} ears.",
          :effects=>%{
            :damage=>0.6,
            :stun=>7.0
          }
        },
        %{
          :user_message=>"You strike {{target}} squarely in the chest with a powerful lightning bolt, causing both of {{target:his/her/its}} arms to pop off with alarming velocity!",
          :target_message=>"{{user}} strikes you squarely in the chest with a powerful lightning bolt, causing both of your arms to pop off with alarming velocity!",
          :spectator_message=>"{{user}} strikes {{target}} squarely in the chest with a powerful lightning bolt, causing both of {{target:his/her/its}} arms to pop off with alarming velocity!",
          :effects=>%{
            :damage=>0.5,
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"arm"
              },
              %{
                :kind=>"sever",
                :limb=>"arm"
              }
            ],
            :stun=>8.0
          }
        },
        %{
          :user_message=>"You strike {{target}} in the arm with a blast of lightning, causing the muscles to contract violently, ripping them apart.",
          :target_message=>"{{user}} strikes you in the arm with a blast of lightning, causing the muscles to contract violently, ripping them apart.",
          :spectator_message=>"{{user}} strikes {{target}} in the arm with a blast of lightning, causing the muscles to contract violently, ripping them apart.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :damage=>0.5,
            :stun=>3.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-50,
                :duration=>20.0
              },
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} is thrown back several feet as your electrical attack strikes {{target:him/her/it}} squarely in the chest.",
          :target_message=>"You are thrown back several feet as {{user}}'s electrical attack strikes you squarely in the chest.",
          :spectator_message=>"{{target}} is thrown back several feet as {{user}}'s electrical attack strikes {{target:him/her/it}} squarely in the chest.",
          :effects=>%{
            :damage=>0.52,
            :stun=>4.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-60,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} is knocked momentarily unconscious when a bolt of electricity from your attack strikes {{target:him/her/it}} in the head.",
          :target_message=>"You are knocked momentarily unconscious when a bolt of electricity from {{user}}'s attack strikes you in the head.",
          :spectator_message=>"{{target}} is knocked momentarily unconscious when a bolt of electricity from {{user}}'s attack strikes {{target:him/her/it}} in the head.",
          :effects=>%{
            :damage=>0.2,
            :stun=>10.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-60,
                :duration=>20.0
              },
              %{
                :skill=>"dodge",
                :amount=>-60,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} collapses to the ground as electrical convulsions caused by your attack cause the muscles in {{target:his/her/its}} leg to shred.",
          :target_message=>"You collapse to the ground as electrical convulsions caused by {{user}}'s attack cause the muscles in your leg to shred.",
          :spectator_message=>"{{target}} collapses to the ground as electrical convulsions caused by {{user}}'s attack cause the muscles in {{target:his/her/its}} leg to shred.",
          :effects=>%{
            :damage=>0.65,
            :stun=>4.0,
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"leg"
              }
            ],
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>8.0
              },
              %{
                :skill=>"dodge",
                :amount=>-70,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}}'s eyeballs shoot violently from their sockets as a lightning bolt from your attack strikes {{target:him/her/it}} in the head!",
          :target_message=>"Your eyeballs shoot violently from their sockets as a lightning bolt from {{user}}'s attack strikes you in the head!",
          :spectator_message=>"{{target}}'s eyeballs shoot violently from their sockets as a lightning bolt from {{user}}'s attack strikes {{target:him/her/it}} in the head!",
          :effects=>%{
            :damage=>0.7,
            :stun=>10.0
          }
        },
        %{
          :user_message=>"{{target}}'s heart explodes violently in {{target:his/her/its}} chest as a violent rain of lightning bolts from your attack strikes {{target:him/her/it}}. {{Target:he/she/it}} falls to the ground, lifeless, blood issuing from {{target:his/her/its}} mouth.",
          :target_message=>"Your heart explodes violently in your chest as a violent rain of lightning bolts from {{user}}'s attack strikes you. You fall to the ground, lifeless, blood issuing from your mouth.",
          :spectator_message=>"{{target}}'s heart explodes violently in {{target:his/her/its}} chest as a violent rain of lightning bolts from {{user}}'s attack strikes {{target:him/her/it}}. {{Target:he/she/it}} falls to the ground, lifeless, blood issuing from {{target:his/her/its}} mouth.",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "E"=>[
        %{
          :user_message=>"{{target}} receives a mighty jolt from your attack.",
          :target_message=>"You receive a mighty jolt from {{user}}'s attack.",
          :spectator_message=>"{{target}} receives a mighty jolt from {{user}}'s attack.",
          :effects=>%{
            :damage=>0.05,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-20,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} is momentarily stunned as a bolt from your attack strikes {{target:him/her/it}} firmly.",
          :target_message=>"You are momentarily stunned as a bolt from {{user}}'s attack strikes you firmly.",
          :spectator_message=>"{{target}} is momentarily stunned as a bolt from {{user}}'s attack strikes {{target:him/her/it}} firmly.",
          :effects=>%{
            :damage=>0.07,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-25,
                :duration=>6.0
              },
              %{
                :skill=>"block",
                :amount=>-25,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} winces in pain as a powerful bolt from your attack strikes him in the side.",
          :target_message=>"You wince in pain as a powerful bolt from {{user}}'s attack strikes him in the side.",
          :spectator_message=>"{{target}} winces in pain as a powerful bolt from {{user}}'s attack strikes him in the side.",
          :effects=>%{
            :damage=>0.15,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-25,
                :duration=>8.0
              },
              %{
                :skill=>"parry",
                :amount=>-25,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your electrical attack strikes {{target}} in {{target:his/her/its}} arm, rendering it useless.",
          :target_message=>"{{user}}'s electrical attack strikes you in your arm, rendering it useless.",
          :spectator_message=>"{{user}}'s electrical attack strikes {{target}} in {{target:his/her/its}} arm, rendering it useless.",
          :effects=>%{
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-30,
                :duration=>10.0
              }
            ],
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :damage=>0.05
          }
        },
        %{
          :user_message=>"All of {{target}}'s wind is knocked out as you collapse {{target:his/her/its}} lungs with a vicious electrical attack to the chest!",
          :target_message=>"All of your wind is knocked out as {{user}} collapses your lungs with a vicious electrical attack to the chest!",
          :spectator_message=>"All of {{target}}'s wind is knocked out as {{user}} collapses {{target:his/her/its}} lungs with a vicious electrical attack to the chest!",
          :effects=>%{
            :damage=>0.2,
            :stun=>3.0,
            :damage_over_time=>%{
              :damage=>0.04,
              :duration=>6.0
            }
          }
        },
        %{
          :user_message=>"{{target}}'s eyes go wide as lightning bolts from your attack dance about {{target:his/her/its}} body!!!",
          :target_message=>"Your eyes go wide as lightning bolts from {{user}}'s attack dance about your body!!!",
          :spectator_message=>"{{target}}'s eyes go wide as lightning bolts from {{user}}'s attack dance about {{target:his/her/its}} body!!!",
          :effects=>%{
            :damage=>0.25,
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-30,
                :duration=>12.0
              },
              %{
                :skill=>"dodge",
                :amount=>-40,
                :duration=>10.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.07,
              :duration=>3.0
            }
          }
        },
        %{
          :user_message=>"Your electrical attack strikes {{target}} firmly in the arm, charring it and rendering it useless.",
          :target_message=>"{{user}}'s electrical attack strikes you firmly in the arm, charring it and rendering it useless.",
          :spectator_message=>"{{user}}'s electrical attack strikes {{target}} firmly in the arm, charring it and rendering it useless.",
          :effects=>%{
            :damage=>0.2,
            :stun=>2.0
          }
        },
        %{
          :user_message=>"Several of {{target}}'s internal organs pop like ballons as a storm of lightning from your attack strikes {{target:him/her/it}} violently!  Following the conflagration, {{target}} falls to the ground, dead and smoking.",
          :target_message=>"Several of your internal organs pop like ballons as a storm of lightning from {{user}}'s attack strikes you violently!  Following the conflagration, you fall to the ground, dead and smoking.",
          :spectator_message=>"Several of {{target}}'s internal organs pop like ballons as a storm of lightning from {{user}}'s attack strikes {{target:him/her/it}} violently!  Following the conflagration, {{target}} falls to the ground, dead and smoking.",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"{{target}}'s lower body is set aflame as a lightning bolt from your attack strikes {{target:him/her/it}}.",
          :target_message=>"Your lower body is set aflame as a lightning bolt from {{user}}'s attack strikes you.",
          :spectator_message=>"{{target}}'s lower body is set aflame as a lightning bolt from {{user}}'s attack strikes {{target:him/her/it}}.",
          :effects=>%{
            :damage=>0.3,
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>6.0
            },
            :stun=>5.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-60,
                :duration=>12.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-25,
                :duration=>24.0
              }
            ]
          }
        },
        %{
          :user_message=>"You bring about violent contractions in {{target}}'s arms with your electrical attack, causing {{target}} to beat {{target}}self repeatedly!",
          :target_message=>"{{user}} brings about violent contractions in your arms with {{user:his/her/its}} electrical attack, causing you to beat youself repeatedly!",
          :spectator_message=>"{{user}} brings about violent contractions in {{target}}'s arms with {{user:his/her/its}} electrical attack, causing {{target}} to beat {{target}}self repeatedly!",
          :effects=>%{
            :damage=>0.25,
            :stun=>4.0,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>6.0
            },
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-80,
                :duration=>8.0
              },
              %{
                :skill=>"block",
                :amount=>-80,
                :duration=>8.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-45,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} flies through the air as your electrical attack strikes {{target:him/her/it}} in the chest, landing with a sickening SPLAT on the ground several feet away.",
          :target_message=>"You fly through the air as {{user}}'s electrical attack strikes you in the chest, landing with a sickening SPLAT on the ground several feet away.",
          :spectator_message=>"{{target}} flies through the air as {{user}}'s electrical attack strikes {{target:him/her/it}} in the chest, landing with a sickening SPLAT on the ground several feet away.",
          :effects=>%{
            :damage=>0.4,
            :stun=>7.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-80,
                :duration=>14.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}}'s arms fly violently out of their sockets as you strike {{target:him/her/it}} with a powerful electrical blast.",
          :target_message=>"Your arms fly violently out of their sockets as {{user}} strikes you with a powerful electrical blast.",
          :spectator_message=>"{{target}}'s arms fly violently out of their sockets as {{user}} strikes {{target:him/her/it}} with a powerful electrical blast.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"arm"
              },
              %{
                :kind=>"sever",
                :limb=>"arm"
              }
            ],
            :damage=>0.5,
            :stun=>5.0,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-40,
                :duration=>600.0
              }
            ],
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>1200.0
              }
            ]
          }
        },
        %{
          :user_message=>"Electrical force from your attack causes pressure to build up in {{target}}'s head. Suddenly, it explodes with great force, and {{target}}'s headless body falls limply to the ground.",
          :target_message=>"Electrical force from {{user}}'s attack causes pressure to build up in your head. Suddenly, it explodes with great force, and your headless body falls limply to the ground.",
          :spectator_message=>"Electrical force from {{user}}'s attack causes pressure to build up in {{target}}'s head. Suddenly, it explodes with great force, and {{target}}'s headless body falls limply to the ground.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"head"
              }
            ],
            :kill=>true
          }
        },
        %{
          :user_message=>"A storm of lightning from your attack strikes {{target}} with unearthly force. {{target:his/her/its}} eyes go wide, and then {{target:him/her/it}} suddenly explodes, scattering entrails about the area.",
          :target_message=>"A storm of lightning from {{user}}'s attack strikes you with unearthly force. Your eyes go wide, and then you suddenly explode, scattering entrails about the area.",
          :spectator_message=>"A storm of lightning from {{user}}'s attack strikes {{target}} with unearthly force. {{target:his/her/its}} eyes go wide, and then {{target:him/her/it}} suddenly explodes, scattering entrails about the area.",
          :effects=>%{
            :kill=>true
          }
        }
      ]
    }
  end

end
