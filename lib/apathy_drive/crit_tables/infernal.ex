defmodule ApathyDrive.CritTables.Infernal do

  def name do
    "infernal"
  end

  def damage_type do
    "magical"
  end

  def crits do
    %{
      "A"=>[
        %{
          :user_message=>"You glow mildly with infernal power.",
          :target_message=>"{{user}} glows mildly with infernal power.",
          :spectator_message=>"{{user}} glows mildly with infernal power.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"{{target}} swears that infernal chant can be heard as your attack hits {{target:him/her/it}}.",
          :target_message=>"You swear that infernal chant can be heard as {{user}}'s attack hits you.",
          :spectator_message=>"{{target}} swears that infernal chant can be heard as {{user}}'s attack hits {{target:him/her/it}}.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"An infernal mist seems to surround you as you attack {{target}} with great force.",
          :target_message=>"An infernal mist seems to surround {{user}} as {{user:he/she/it}} attacks you with great force.",
          :spectator_message=>"An infernal mist seems to surround {{user}} as {{user:he/she/it}} attacks {{target}} with great force.",
          :effects=>%{
            :damage=>0.1
          }
        },
        %{
          :user_message=>"{{target}} is tossed aside by the brutal power of your attack.",
          :target_message=>"You are tossed aside by the brutal power of {{user}}'s attack.",
          :spectator_message=>"{{target}} is tossed aside by the brutal power of {{user}}'s attack.",
          :effects=>%{
            :damage=>0.04
          }
        },
        %{
          :user_message=>"The infernal power of your attack causes you to be engulfed in a devilish haze.",
          :target_message=>"The infernal power of {{user}}'s attack causes {{user:him/her/it}} to be engulfed in a devilish haze.",
          :spectator_message=>"The infernal power of {{user}}'s attack causes {{user:him/her/it}} to be engulfed in a devilish haze.",
          :effects=>%{
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-50,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} is enchanted by your mezmerizing combat dance.",
          :target_message=>"You are enchanted by {{user}}'s mezmerizing combat dance.",
          :spectator_message=>"{{target}} is enchanted by {{user}}'s mezmerizing combat dance.",
          :effects=>%{
            :stun=>4.0
          }
        },
        %{
          :user_message=>"Your soul-less wail gates in an infernal imp to corrupt {{target}}, so that you may play more trecherously.",
          :target_message=>"{{user}}'s soul-less wail gates in an infernal imp to corrupt you, so that {{user}} may play more trecherously.",
          :spectator_message=>"{{user}}'s soul-less wail gates in an infernal imp to corrupt {{target}}, so that {{user}} may play more trecherously.",
          :effects=>%{
            :stun=>2.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-70,
                :duration=>8.0
              },
              %{
                :skill=>"parry",
                :amount=>-70,
                :duration=>20.0
              },
              %{
                :skill=>"dodge",
                :amount=>-70,
                :duration=>8.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-25,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your strike is directed by an infernal force, pounding {{target}} with exacting measure.",
          :target_message=>"{{user}}'s strike is directed by an infernal force, pounding you with exacting measure.",
          :spectator_message=>"{{user}}'s strike is directed by an infernal force, pounding {{target}} with exacting measure.",
          :effects=>%{
            :damage=>0.1,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"Infernal forces resulting from your attack envelop {{target}}, levitate {{target:him/her/it}} in the air, and drop {{target:him/her/it}}!",
          :target_message=>"Infernal forces resulting from {{user}}'s attack envelop you, levitate you in the air, and drop you!",
          :spectator_message=>"Infernal forces resulting from {{user}}'s attack envelop {{target}}, levitate {{target:him/her/it}} in the air, and drop {{target:him/her/it}}!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"Your fury channels infernal forces which intensify future attacks.",
          :target_message=>"{{user}}'s fury channels infernal forces which intensify future attacks.",
          :spectator_message=>"{{user}}'s fury channels infernal forces which intensify future attacks.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"{{target}} is struck violently by your devilish attack.",
          :target_message=>"You are struck violently by {{user}}'s devilish attack.",
          :spectator_message=>"{{target}} is struck violently by {{user}}'s devilish attack.",
          :effects=>%{
            :damage=>0.25,
            :stun=>2.0
          }
        },
        %{
          :user_message=>"Your utters an infernal curse causing {{target}} to quiver violently as all muscle control is momentarly lost.",
          :target_message=>"{{user}}'s utters an infernal curse causing you to quiver violently as all muscle control is momentarly lost.",
          :spectator_message=>"{{user}}'s utters an infernal curse causing {{target}} to quiver violently as all muscle control is momentarly lost.",
          :effects=>%{
            :stun=>2.0,
            :damage=>0.15,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-20,
                :duration=>16.0
              }
            ]
          }
        },
        %{
          :user_message=>"You are driven to attack {{target}} again by infernal fury.",
          :target_message=>"{{user}} ares driven to attack you again by infernal fury.",
          :spectator_message=>"{{user}} ares driven to attack {{target}} again by infernal fury.",
          :effects=>%{
            :damage=>0.2,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-40,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"Infernal forces commanded by your attack and blast {{target}} a great distance backward.",
          :target_message=>"Infernal forces commanded by {{user}}'s attack and blast you a great distance backward.",
          :spectator_message=>"Infernal forces commanded by {{user}}'s attack and blast {{target}} a great distance backward.",
          :effects=>%{
            :damage=>0.25,
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>10.0
              },
              %{
                :skill=>"dodge",
                :amount=>-50,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"A impish creature is gated by your command, and it proceeds to attack {{target}}!",
          :target_message=>"A impish creature is gated by {{user}}'s command, and it proceeds to attack you!",
          :spectator_message=>"A impish creature is gated by {{user}}'s command, and it proceeds to attack {{target}}!",
          :effects=>%{
            :damage=>0.1,
            :stun=>2.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-50,
                :duration=>10.0
              },
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>10.0
              }
            ]
          }
        }
      ],
      "B"=>[
        %{
          :user_message=>"You are infused with carnal power momentarily.",
          :target_message=>"{{user}} ares infused with carnal power momentarily.",
          :spectator_message=>"{{user}} ares infused with carnal power momentarily.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"Your rage, coupled with carnal power, unbalances {{target}}.",
          :target_message=>"{{user}}'s rage, coupled with carnal power, unbalances you.",
          :spectator_message=>"{{user}}'s rage, coupled with carnal power, unbalances {{target}}.",
          :effects=>%{
            :stun=>2.0
          }
        },
        %{
          :user_message=>"Your rage is fueled by carnal power, much to {{target}}'s dismay!",
          :target_message=>"{{user}}'s rage is fueled by carnal power, much to your dismay!",
          :spectator_message=>"{{user}}'s rage is fueled by carnal power, much to {{target}}'s dismay!",
          :effects=>%{
            :damage=>0.05,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-30,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"Tendrals of carnal power from your rage strike {{target}} with ugly efficency.",
          :target_message=>"Tendrals of carnal power from {{user}}'s rage strike you with ugly efficency.",
          :spectator_message=>"Tendrals of carnal power from {{user}}'s rage strike {{target}} with ugly efficency.",
          :effects=>%{
            :damage=>0.15,
            :stun=>2.0,
            :damage_over_time=>%{
              :damage=>0.04,
              :duration=>3.0
            },
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
          :user_message=>"You strike {{target}} with nasty power, as your rage is controled by infernal influence.",
          :target_message=>"{{user}} strikes you with nasty power, as {{user:his/her/its}} rage is controled by infernal influence.",
          :spectator_message=>"{{user}} strikes {{target}} with nasty power, as {{user:his/her/its}} rage is controled by infernal influence.",
          :effects=>%{
            :damage=>0.18,
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your rage is infused with carnal power, and it causes {{target}} to stagger in dismay.",
          :target_message=>"{{user}}'s rage is infused with carnal power, and it causes you to stagger in dismay.",
          :spectator_message=>"{{user}}'s rage is infused with carnal power, and it causes {{target}} to stagger in dismay.",
          :effects=>%{
            :damage=>0.2,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-80,
                :duration=>20.0
              },
              %{
                :skill=>"parry",
                :amount=>-80,
                :duration=>20.0
              }
            ],
            :stun=>3.0
          }
        },
        %{
          :user_message=>"Your infernal rage summons a small flaming bolt to strike {{target}}.",
          :target_message=>"{{user}}'s infernal rage summons a small flaming bolt to strike you.",
          :spectator_message=>"{{user}}'s infernal rage summons a small flaming bolt to strike {{target}}.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"Your attack is controlled by infernal power, and {{target}} is casually blasted to the ground, cracking a few ribs.",
          :target_message=>"{{user}}'s attack is controlled by infernal power, and you are casually blasted to the ground, cracking a few ribs.",
          :spectator_message=>"{{user}}'s attack is controlled by infernal power, and {{target}} is casually blasted to the ground, cracking a few ribs.",
          :effects=>%{
            :damage=>0.25,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>4.0
            },
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>16.0
              },
              %{
                :skill=>"parry",
                :amount=>-40,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your carnal rage gains the attention of the lower planes, and a duet of impish creatures join in the dance on your behalf!",
          :target_message=>"{{user}}'s carnal rage gains the attention of the lower planes, and a duet of impish creatures join in the dance on {{user}}'s behalf!",
          :spectator_message=>"{{user}}'s carnal rage gains the attention of the lower planes, and a duet of impish creatures join in the dance on {{user}}'s behalf!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"Your attack is enhanced with infernal strength, and you crush {{target}}!",
          :target_message=>"{{user}}'s attack is enhanced with infernal strength, and {{user:he/she/it}} crushes you!",
          :spectator_message=>"{{user}}'s attack is enhanced with infernal strength, and {{user:he/she/it}} crushes {{target}}!",
          :effects=>%{
            :damage=>0.5,
            :stun=>4.0,
            :damage_over_time=>%{
              :damage=>0.07,
              :duration=>5.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-90,
                :duration=>8.0
              },
              %{
                :skill=>"dodge",
                :amount=>-30,
                :duration=>20.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-20,
                :duration=>5.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your corrupting efforts seen in hell, you are channeled additional strength!",
          :target_message=>"{{user:his/her/its}} corrupting efforts seen in hell, {{user}} ares channeled additional strength!",
          :spectator_message=>"{{user:his/her/its}} corrupting efforts seen in hell, {{user}} ares channeled additional strength!",
          :effects=>%{
            :damage=>0.5
          }
        },
        %{
          :user_message=>"You channel unholy power to gate in an impish creature whilst pummeling on {{target}}.",
          :target_message=>"{{user}} channels unholy power to gate in an impish creature whilst pummeling on you.",
          :spectator_message=>"{{user}} channels unholy power to gate in an impish creature whilst pummeling on {{target}}.",
          :effects=>%{
            :damage=>0.3
          }
        },
        %{
          :user_message=>"You blast {{target}}'s arm the force of corruption, leaving it in a useless state!",
          :target_message=>"{{user}} blasts your arm the force of corruption, leaving it in a useless state!",
          :spectator_message=>"{{user}} blasts {{target}}'s arm the force of corruption, leaving it in a useless state!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :stun=>3.0
          }
        },
        %{
          :user_message=>"With determination that {{target}} thought impossible, you blast {{target:him/her/it}}, pounding {{target:him/her/it}} effortlessly into a quiver heap of flesh.",
          :target_message=>"With determination that you thought impossible, {{user}} blasts you, pounding you effortlessly into a quiver heap of flesh.",
          :spectator_message=>"With determination that {{target}} thought impossible, {{user}} blasts {{target:him/her/it}}, pounding {{target:him/her/it}} effortlessly into a quiver heap of flesh.",
          :effects=>%{
            :damage=>0.45,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>8.0
            },
            :stun=>5.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"To your suprise...a flaming gate opens and an Erines Devil appears to fight along side you in your combat!",
          :target_message=>"To {{user}}'s suprise...a flaming gate opens and an Erines Devil appears to fight along side {{user}} in {{user:his/her/its}} combat!",
          :spectator_message=>"To {{user}}'s suprise...a flaming gate opens and an Erines Devil appears to fight along side {{user}} in {{user:his/her/its}} combat!",
          :effects=>%{

          }
        }
      ],
      "C"=>[
        %{
          :user_message=>"You reek of corruption.",
          :target_message=>"{{user}} reeks of corruption.",
          :spectator_message=>"{{user}} reeks of corruption.",
          :effects=>%{
            :damage=>0.05
          }
        },
        %{
          :user_message=>"You channel power of the damned, allowing you to attack with insidious efficiency.",
          :target_message=>"{{user}} channels power of the damned, allowing {{user:him/her/it}} to attack with insidious efficiency.",
          :spectator_message=>"{{user}} channels power of the damned, allowing {{user:him/her/it}} to attack with insidious efficiency.",
          :effects=>%{
            :damage=>0.1,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-10,
                :duration=>10.0
              }
            ],
            :skill_mod=>[
              %{
                :skill=>"block",
                :amount=>-50,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"You seem to go into a frenzy momentarily, attacking repeatedly!",
          :target_message=>"{{user}} seems to go into a frenzy momentarily, attacking repeatedly!",
          :spectator_message=>"{{user}} seems to go into a frenzy momentarily, attacking repeatedly!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"Your fury channels the 'Pain of Damnation', which causes screams of agony to come from {{target}}!",
          :target_message=>"{{user}}'s fury channels the 'Pain of Damnation', which causes screams of agony to come from you!",
          :spectator_message=>"{{user}}'s fury channels the 'Pain of Damnation', which causes screams of agony to come from {{target}}!",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.07,
              :duration=>15.0
            },
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-50,
                :duration=>10.0
              },
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"An impish creature gates in to mosh with you in battle.",
          :target_message=>"An impish creature gates in to mosh with {{user}} in battle.",
          :spectator_message=>"An impish creature gates in to mosh with {{user}} in battle.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"Hellish corruption called forth by your bewilder and confuse {{target}}.",
          :target_message=>"Hellish corruption called forth by {{user}}'s bewilder and confuse you.",
          :spectator_message=>"Hellish corruption called forth by {{user}}'s bewilder and confuse {{target}}.",
          :effects=>%{
            :stun=>7.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-60,
                :duration=>14.0
              }
            ]
          }
        },
        %{
          :user_message=>"Infernal bias from your assault leaves {{target}} staggering!",
          :target_message=>"Infernal bias from {{user}}'s assault leaves you staggering!",
          :spectator_message=>"Infernal bias from {{user}}'s assault leaves {{target}} staggering!",
          :effects=>%{
            :damage=>0.35,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}}'s eyes roll back as your blow engulfs {{target:him/her/it}}, buffetting {{target:him/her/it}} wildly.",
          :target_message=>"Your eyes roll back as {{user}}'s blow engulfs you, buffetting you wildly.",
          :spectator_message=>"{{target}}'s eyes roll back as {{user}}'s blow engulfs {{target:him/her/it}}, buffetting {{target:him/her/it}} wildly.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.15,
              :duration=>3.0
            },
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-80,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"Two hellspawn...an Erines Devil, and an impish creature, gate in to destroy you!",
          :target_message=>"Two hellspawn...an Erines Devil, and an impish creature, gate in to destroy {{user}}!",
          :spectator_message=>"Two hellspawn...an Erines Devil, and an impish creature, gate in to destroy {{user}}!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"Your blow blasts {{target}} in the leg, with a force is so great that {{target:his/her/its}} leg is crippled!",
          :target_message=>"{{user}}'s blow blasts you in the leg, with a force is so great that your leg is crippled!",
          :spectator_message=>"{{user}}'s blow blasts {{target}} in the leg, with a force is so great that {{target:his/her/its}} leg is crippled!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"leg"
              }
            ],
            :stun=>2.0,
            :damage_over_time=>%{
              :damage=>0.15,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"{{target}} is blasted back by your raving assault.",
          :target_message=>"You are blasted back by {{user}}'s raving assault.",
          :spectator_message=>"{{target}} is blasted back by {{user}}'s raving assault.",
          :effects=>%{
            :damage=>0.5,
            :stun=>4.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-70,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} feels blood boil as flames caress {{target:him/her/it}} intestines from your 'Bowel Fire' curse!",
          :target_message=>"You feel blood boil as flames caress you intestines from {{user}}'s 'Bowel Fire' curse!",
          :spectator_message=>"{{target}} feels blood boil as flames caress {{target:him/her/it}} intestines from {{user}}'s 'Bowel Fire' curse!",
          :effects=>%{
            :damage=>0.2,
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>8.0
            },
            :stat_mod=>[
              %{
                :stat=>"health",
                :amount=>-10,
                :duration=>32.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} watches as the flesh and muscle of {{target:his/her/its}} arms quickly decays to dust and are rendered useless.",
          :target_message=>"You watch as the flesh and muscle of your arms quickly decays to dust and are rendered useless.",
          :spectator_message=>"{{target}} watches as the flesh and muscle of {{target:his/her/its}} arms quickly decays to dust and are rendered useless.",
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
            :stun=>5.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-80,
                :duration=>20.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.06,
              :duration=>8.0
            }
          }
        },
        %{
          :user_message=>"{{target}} is sickened painfully by a black mist from your attack, which causes {{target:him/her/it}} to retch violently.",
          :target_message=>"You are sickened painfully by a black mist from {{user}}'s attack, which causes you to retch violently.",
          :spectator_message=>"{{target}} is sickened painfully by a black mist from {{user}}'s attack, which causes {{target:him/her/it}} to retch violently.",
          :effects=>%{
            :damage=>0.4,
            :stun=>5.0,
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>6.0
            },
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"A regal Devilish Noble arises from a circle of blue flame and engages {{target}} in the 'Dance of Damnation'!!!",
          :target_message=>"A regal Devilish Noble arises from a circle of blue flame and engages you in the 'Dance of Damnation'!!!",
          :spectator_message=>"A regal Devilish Noble arises from a circle of blue flame and engages {{target}} in the 'Dance of Damnation'!!!",
          :effects=>%{

          }
        }
      ],
      "D"=>[
        %{
          :user_message=>"You pummel {{target}} with hellish enthusiasm.",
          :target_message=>"{{user}} pummels you with hellish enthusiasm.",
          :spectator_message=>"{{user}} pummels {{target}} with hellish enthusiasm.",
          :effects=>%{
            :damage=>0.08
          }
        },
        %{
          :user_message=>"Imbued the power of corruption, you blast {{target}} with extreme vengence.",
          :target_message=>"Imbued the power of corruption, {{user}} blasts you with extreme vengence.",
          :spectator_message=>"Imbued the power of corruption, {{user}} blasts {{target}} with extreme vengence.",
          :effects=>%{
            :damage=>0.15,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"Malignant energy from your foul chant causes spasms in {{target}}'s legs, and {{target:he/she/it}} collapses to the ground, momentarily helpless.",
          :target_message=>"Malignant energy from {{user}}'s foul chant causes spasms in your legs, and you collapse to the ground, momentarily helpless.",
          :spectator_message=>"Malignant energy from {{user}}'s foul chant causes spasms in {{target}}'s legs, and {{target:he/she/it}} collapses to the ground, momentarily helpless.",
          :effects=>%{
            :damage=>0.15,
            :damage_over_time=>%{
              :damage=>0.01,
              :duration=>15.0
            },
            :stun=>4.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-20,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} gets all of {{target:him/her/it}} wind knocked out upon viewing a 'Glyph of Suffocation' displayed by you.",
          :target_message=>"You get all of you wind knocked out upon viewing a 'Glyph of Suffocation' displayed by {{user}}.",
          :spectator_message=>"{{target}} gets all of {{target:him/her/it}} wind knocked out upon viewing a 'Glyph of Suffocation' displayed by {{user}}.",
          :effects=>%{
            :damage=>0.2,
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>15.0
            },
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-70,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"Two impish creatures coalesce from blood spilt by you. They immediately engage {{target}}!",
          :target_message=>"Two impish creatures coalesce from blood spilt by {{user}}. They immediately engage you!",
          :spectator_message=>"Two impish creatures coalesce from blood spilt by {{user}}. They immediately engage {{target}}!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"{{target}} is staggered and rendered unable to breathe by a 'Greater Glyph of Suffocation' channeled by you.",
          :target_message=>"You are staggered and rendered unable to breathe by a 'Greater Glyph of Suffocation' channeled by {{user}}.",
          :spectator_message=>"{{target}} is staggered and rendered unable to breathe by a 'Greater Glyph of Suffocation' channeled by {{user}}.",
          :effects=>%{
            :stun=>5.0,
            :damage_over_time=>%{
              :damage=>0.01,
              :duration=>21.0
            }
          }
        },
        %{
          :user_message=>"An Erines Devil appears in response to the fervor of you in combat.",
          :target_message=>"An Erines Devil appears in response to the fervor of {{user}} in combat.",
          :spectator_message=>"An Erines Devil appears in response to the fervor of {{user}} in combat.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You smash {{target}}'s legs with intense excitement, mangling them repulsively!",
          :target_message=>"{{user}} smashes your legs with intense excitement, mangling them repulsively!",
          :spectator_message=>"{{user}} smashes {{target}}'s legs with intense excitement, mangling them repulsively!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"leg"
              },
              %{
                :kind=>"cripple",
                :limb=>"leg"
              }
            ],
            :stun=>2.0
          }
        },
        %{
          :user_message=>"A regal Devilish Noble leaps from a blue conflagration and bellows to {{target}}, 'Your ASS is MINE!'",
          :target_message=>"A regal Devilish Noble leaps from a blue conflagration and bellows to you, 'Your ASS is MINE!'",
          :spectator_message=>"A regal Devilish Noble leaps from a blue conflagration and bellows to {{target}}, 'Your ASS is MINE!'",
          :effects=>%{

          }
        },
        %{
          :user_message=>"{{target}} takes a crushing blow from your well timed assault, shattering {{target:his/her/its}} hip.",
          :target_message=>"You take a crushing blow from {{user}}'s well timed assault, shattering your hip.",
          :spectator_message=>"{{target}} takes a crushing blow from {{user}}'s well timed assault, shattering {{target:his/her/its}} hip.",
          :effects=>%{
            :damage=>0.55,
            :damage_over_time=>%{
              :damage=>0.07,
              :duration=>4.0
            },
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-70,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} falls and slams {{target:his/her/its}} head, rendering {{target:him/her/it}} momentarily unconscious...{{target}} cackles devilishly.",
          :target_message=>"You falls and slams your head, rendering you momentarily unconscious...you cackles devilishly.",
          :spectator_message=>"{{target}} falls and slams {{target:his/her/its}} head, rendering {{target:him/her/it}} momentarily unconscious...{{target}} cackles devilishly.",
          :effects=>%{
            :damage=>0.2,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>18.0
            },
            :stun=>8.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-200,
                :duration=>16.0
              },
              %{
                :skill=>"block",
                :amount=>-200,
                :duration=>16.0
              }
            ]
          }
        },
        %{
          :user_message=>"You obliterate {{target}}'s arm with 'Hells Fury,' reducing it to a useless lump of mangled tissue!",
          :target_message=>"{{user}} obliterates your arm with 'Hells Fury,' reducing it to a useless lump of mangled tissue!",
          :spectator_message=>"{{user}} obliterates {{target}}'s arm with 'Hells Fury,' reducing it to a useless lump of mangled tissue!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :stun=>3.0,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"Your fury channels a 'Finger of Corruption,' and causes hundreds of puss oozing wounds in {{target}}'s back, and {{target:he/she/it}} bleeds profusely.",
          :target_message=>"{{user}}'s fury channels a 'Finger of Corruption,' and causes hundreds of puss oozing wounds in your back, and you bleed profusely.",
          :spectator_message=>"{{user}}'s fury channels a 'Finger of Corruption,' and causes hundreds of puss oozing wounds in {{target}}'s back, and {{target:he/she/it}} bleeds profusely.",
          :effects=>%{
            :damage=>0.05,
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>10.0
            },
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-60,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your assault pounds {{target}} nastily in the chest, shattering many ribs with a sickening sound.",
          :target_message=>"{{user}}'s assault pounds you nastily in the chest, shattering many ribs with a sickening sound.",
          :spectator_message=>"{{user}}'s assault pounds {{target}} nastily in the chest, shattering many ribs with a sickening sound.",
          :effects=>%{
            :damage=>0.7,
            :damage_over_time=>%{
              :damage=>0.07,
              :duration=>8.0
            },
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-70,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You utters an in an unearthly voice but only a few of the damnedable sounds are distinguishable, '...Gadz En Sole-Fellen Kom...'  Suddenly, a violet flaming portal opens beneath {{target}} and the flames erupt in an immolation storm...{{target:he/she/it}} is instantly reduced to ash!",
          :target_message=>"{{user}} utters an in an unearthly voice but only a few of the damnedable sounds are distinguishable, '...Gadz En Sole-Fellen Kom...'  Suddenly, a violet flaming portal opens beneath you and the flames erupt in an immolation storm...you are instantly reduced to ash!",
          :spectator_message=>"{{user}} utters an in an unearthly voice but only a few of the damnedable sounds are distinguishable, '...Gadz En Sole-Fellen Kom...'  Suddenly, a violet flaming portal opens beneath {{target}} and the flames erupt in an immolation storm...{{target:he/she/it}} is instantly reduced to ash!",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "E"=>[
        %{
          :user_message=>"You strike {{target}} with great force.",
          :target_message=>"{{user}} strikes you with great force.",
          :spectator_message=>"{{user}} strikes {{target}} with great force.",
          :effects=>%{
            :damage=>0.1
          }
        },
        %{
          :user_message=>"{{target}} is disoriented by the might of your attack.",
          :target_message=>"You are disoriented by the might of {{user}}'s attack.",
          :spectator_message=>"{{target}} is disoriented by the might of {{user}}'s attack.",
          :effects=>%{
            :damage=>0.08,
            :stun=>3.0
          }
        },
        %{
          :user_message=>"You gate in an Erines Devil to join you in the fun.",
          :target_message=>"{{user}} gates in an Erines Devil to join {{user:him/her/it}} in the fun.",
          :spectator_message=>"{{user}} gates in an Erines Devil to join {{user:him/her/it}} in the fun.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"A pillar of hellfire erupts from your hands transfixing {{target}} and corrupting {{target}}'s body grievously.",
          :target_message=>"A pillar of hellfire erupts from {{user}}'s hands transfixing you and corrupting your body grievously.",
          :spectator_message=>"A pillar of hellfire erupts from {{user}}'s hands transfixing {{target}} and corrupting {{target}}'s body grievously.",
          :effects=>%{
            :damage=>0.15,
            :stun=>1.0,
            :damage_over_time=>%{
              :damage=>0.01,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"Your flays major wounds on {{target}}'s body.",
          :target_message=>"{{user}}'s flays major wounds on your body.",
          :spectator_message=>"{{user}}'s flays major wounds on {{target}}'s body.",
          :effects=>%{
            :damage=>0.2,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>5.0
            },
            :stun=>2.0
          }
        },
        %{
          :user_message=>"{{target}} is blasted by your aggressive assault and end up riddled dozens of bleeding lascerations.",
          :target_message=>"You are blasted by {{user}}'s aggressive assault and end up riddled dozens of bleeding lascerations.",
          :spectator_message=>"{{target}} is blasted by {{user}}'s aggressive assault and end up riddled dozens of bleeding lascerations.",
          :effects=>%{
            :damage=>0.2,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>10.0
            },
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-70,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} $(VT) bathed in a wash of burning, sulphurous fluid that leaves {{target:him/her/it}} wracked in pain.",
          :target_message=>"You $(VT) bathed in a wash of burning, sulphurous fluid that leaves you wracked in pain.",
          :spectator_message=>"{{target}} $(VT) bathed in a wash of burning, sulphurous fluid that leaves {{target:him/her/it}} wracked in pain.",
          :effects=>%{
            :damage=>0.5,
            :stun=>5.0
          }
        },
        %{
          :user_message=>"Heeding the call of you, an infernal member of royalty arrives with an assistant.",
          :target_message=>"Heeding the call of {{user}}, an infernal member of royalty arrives with an assistant.",
          :spectator_message=>"Heeding the call of {{user}}, an infernal member of royalty arrives with an assistant.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You coughs up a massive eruption of fetid blood that sprays all over {{target}}, causing {{target:him/her/it}} to twist and convulse violently. When the agony subsides, all that remains of {{target}} is a large puddle of crimson jelly.",
          :target_message=>"{{user}} coughs up a massive eruption of fetid blood that sprays all over you, causing you to twist and convulse violently. When the agony subsides, all that remains of you is a large puddle of crimson jelly.",
          :spectator_message=>"{{user}} coughs up a massive eruption of fetid blood that sprays all over {{target}}, causing {{target:him/her/it}} to twist and convulse violently. When the agony subsides, all that remains of {{target}} is a large puddle of crimson jelly.",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"A Devilish Noble suprises {{target}} from behind and rips off one of {{target:his/her/its}} arms and proceeds to violently attack {{target}}...using the liberated appendage as it primary weapon.",
          :target_message=>"A Devilish Noble suprises you from behind and rips off one of your arms and proceeds to violently attack you...using the liberated appendage as it primary weapon.",
          :spectator_message=>"A Devilish Noble suprises {{target}} from behind and rips off one of {{target:his/her/its}} arms and proceeds to violently attack {{target}}...using the liberated appendage as it primary weapon.",
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
          :user_message=>"The force of your assault is so massive, that an unbelievable amount of blood, bile, and spinal fluid spews from {{target:his/her/its}} mouth and ears!",
          :target_message=>"The force of {{user}}'s assault is so massive, that an unbelievable amount of blood, bile, and spinal fluid spews from your mouth and ears!",
          :spectator_message=>"The force of {{user}}'s assault is so massive, that an unbelievable amount of blood, bile, and spinal fluid spews from {{target:his/her/its}} mouth and ears!",
          :effects=>%{
            :damage=>0.6,
            :damage_over_time=>%{
              :damage=>0.07,
              :duration=>20.0
            }
          }
        },
        %{
          :user_message=>"Your hellish attacks mangle {{target}}'s arms, mincing the muscles effortlessly and quite thoroughly!",
          :target_message=>"{{user}}'s hellish attacks mangle your arms, mincing the muscles effortlessly and quite thoroughly!",
          :spectator_message=>"{{user}}'s hellish attacks mangle {{target}}'s arms, mincing the muscles effortlessly and quite thoroughly!",
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
            :damage=>0.4,
            :stun=>4.0
          }
        },
        %{
          :user_message=>"The furious savagery of your hatred causes {{target}} to spontaneously combust, blowing ALL FOUR of {{target:his/her/its}} limb off!",
          :target_message=>"The furious savagery of {{user}}'s hatred causes you to spontaneously combust, blowing ALL FOUR of your limb off!",
          :spectator_message=>"The furious savagery of {{user}}'s hatred causes {{target}} to spontaneously combust, blowing ALL FOUR of {{target:his/her/its}} limb off!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              },
              %{
                :kind=>"cripple",
                :limb=>"arm"
              },
              %{
                :kind=>"cripple",
                :limb=>"leg"
              },
              %{
                :kind=>"cripple",
                :limb=>"leg"
              }
            ],
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>8.0
              },
              %{
                :skill=>"dodge",
                :amount=>-100,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"Not one, but TWO Devilish Nobles appear from a flaming blue pit that opens in front of {{target}}...and accept {{target}} as a 'willing' sacrifice!",
          :target_message=>"Not one, but TWO Devilish Nobles appear from a flaming blue pit that opens in front of you...and accept you as a 'willing' sacrifice!",
          :spectator_message=>"Not one, but TWO Devilish Nobles appear from a flaming blue pit that opens in front of {{target}}...and accept {{target}} as a 'willing' sacrifice!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"As a reward for your loyalty...A Hellish Prince arrives with a myriad of hellspawn to support you in your efforts to spread corruption and defile all that is Holy...They then proceed to kick {{target:his/her/its}} ass.",
          :target_message=>"As a reward for {{user}}'s loyalty...A Hellish Prince arrives with a myriad of hellspawn to support {{user}} in {{user:his/her/its}} efforts to spread corruption and defile all that is Holy...They then proceed to kick your ass.",
          :spectator_message=>"As a reward for {{user}}'s loyalty...A Hellish Prince arrives with a myriad of hellspawn to support {{user}} in {{user:his/her/its}} efforts to spread corruption and defile all that is Holy...They then proceed to kick {{target:his/her/its}} ass.",
          :effects=>%{

          }
        }
      ]
    }
  end

end
