defmodule ApathyDrive.CritTables.Cold do

  def name do
    "cold"
  end

  def damage_type do
    "magical"
  end

  def crits do
    %{
      "A"=>[
        %{
          :user_message=>"You create a cold draft around {{target}}.",
          :target_message=>"{{user}} creates a cold draft around you.",
          :spectator_message=>"{{user}} creates a cold draft around {{target}}.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"{{target}} shivers slightly from your attack.",
          :target_message=>"You shiver slightly from {{user}}'s attack.",
          :spectator_message=>"{{target}} shivers slightly from {{user}}'s attack.",
          :effects=>%{
            :damage=>0.01
          }
        },
        %{
          :user_message=>"You summon a chilling mist around {{target}}'s feet.",
          :target_message=>"{{user}} summons a chilling mist around your feet.",
          :spectator_message=>"{{user}} summons a chilling mist around {{target}}'s feet.",
          :effects=>%{
            :damage=>0.02
          }
        },
        %{
          :user_message=>"{{target}} develops mild frostbite from your attack, causing discomfort.",
          :target_message=>"You develop mild frostbite from {{user}}'s attack, causing discomfort.",
          :spectator_message=>"{{target}} develops mild frostbite from {{user}}'s attack, causing discomfort.",
          :effects=>%{
            :damage=>0.02,
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
          :user_message=>"Your attack yield small patches of ice on {{target}}'s arms.",
          :target_message=>"{{user}}'s attack yields small patches of ice on your arms.",
          :spectator_message=>"{{user}}'s attack yields small patches of ice on {{target}}'s arms.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.01,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"You cause ice to cover the ground under {{target}} freezing {{target:his/her/its}} feet.",
          :target_message=>"{{user}} causes ice to cover the ground under you freezing your feet.",
          :spectator_message=>"{{user}} causes ice to cover the ground under {{target}} freezing {{target:his/her/its}} feet.",
          :effects=>%{
            :damage=>0.05,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-10,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You attack freezing {{target}} slowing {{target:his/her/its}} blood flow.",
          :target_message=>"{{user}} attacks freezing you slowing your blood flow.",
          :spectator_message=>"{{user}} attacks freezing {{target}} slowing {{target:his/her/its}} blood flow.",
          :effects=>%{
            :damage=>0.05,
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>2.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-15,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your attack freezes {{target}}'s ears!",
          :target_message=>"{{user}}'s attack freezes your ears!",
          :spectator_message=>"{{user}}'s attack freezes {{target}}'s ears!",
          :effects=>%{
            :damage=>0.04,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"Your attack turn {{target}}'s slightly blue causing {{target:his/her/its}} teeth to chatter violently.",
          :target_message=>"{{user}}'s attack turn your slightly blue causing your teeth to chatter violently.",
          :spectator_message=>"{{user}}'s attack turn {{target}}'s slightly blue causing {{target:his/her/its}} teeth to chatter violently.",
          :effects=>%{
            :damage=>0.04,
            :stat_mod=>[
              %{
                :stat=>nil,
                :amount=>-10,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You laugh as {{target}} stares blankly at {{target:his/her/its}} shattering hand. Cold.",
          :target_message=>"{{user}} laughs as you stares blankly at your shattering hand. Cold.",
          :spectator_message=>"{{user}} laughs as {{target}} stares blankly at {{target:his/her/its}} shattering hand. Cold.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"hand"
              }
            ],
            :damage=>0.05,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-30,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} grasps {{target:his/her/its}} throat as ice from your attack strangles {{target:his/her/its}}.",
          :target_message=>"You grasp your throat as ice from {{user}}'s attack strangles your.",
          :spectator_message=>"{{target}} grasps {{target:his/her/its}} throat as ice from {{user}}'s attack strangles {{target:his/her/its}}.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>3.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-20,
                :duration=>5.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} disappears under a hail of wind and snow at your will.",
          :target_message=>"You disappear under a hail of wind and snow at {{user}}'s will.",
          :spectator_message=>"{{target}} disappears under a hail of wind and snow at {{user}}'s will.",
          :effects=>%{
            :damage=>0.1,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-10,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} falls over smashing {{target:his/her/its}} head being left numb from your attack.",
          :target_message=>"You fall over smashing your head being left numb from {{user}}'s attack.",
          :spectator_message=>"{{target}} falls over smashing {{target:his/her/its}} head being left numb from {{user}}'s attack.",
          :effects=>%{
            :damage=>0.06,
            :stun=>3.0,
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
          :user_message=>"You blast {{target}} backwards off {{target:his/her/its}} feet with a barge of ice.",
          :target_message=>"{{user}} blasts you backwards off your feet with a barge of ice.",
          :spectator_message=>"{{user}} blasts {{target}} backwards off {{target:his/her/its}} feet with a barge of ice.",
          :effects=>%{
            :damage=>0.1,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-20,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your attack fills {{target}}'s mouth and nose leaving {{target:his/her/its}} coughing and struggling to breathe.",
          :target_message=>"{{user}}'s attack fills your mouth and nose leaving your coughing and struggling to breathe.",
          :spectator_message=>"{{user}}'s attack fills {{target}}'s mouth and nose leaving {{target:his/her/its}} coughing and struggling to breathe.",
          :effects=>%{
            :damage=>0.05,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>5.0
            },
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-70,
                :duration=>5.0
              }
            ]
          }
        },
        %{
          :user_message=>"You watch {{target}} cough up blood and frozen lung matter. Your attack has frozen {{target}}'s chest cavity.",
          :target_message=>"{{user}} watches you cough up blood and frozen lung matter. {{user}}'s attack has frozen your chest cavity.",
          :spectator_message=>"{{user}} watches {{target}} cough up blood and frozen lung matter. {{user}}'s attack has frozen {{target}}'s chest cavity.",
          :effects=>%{
            :damage=>0.15,
            :damage_over_time=>%{
              :damage=>0.07,
              :duration=>5.0
            },
            :stun=>5.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>8.0
              }
            ]
          }
        }
      ],
      "B"=>[
        %{
          :user_message=>"You summon a cold breeze to blow by {{target}}'s head",
          :target_message=>"{{user}} summons a cold breeze to blow by your head",
          :spectator_message=>"{{user}} summons a cold breeze to blow by {{target}}'s head",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You manage to create a small flurry of snow to fall on {{target}}.",
          :target_message=>"{{user}} manages to create a small flurry of snow to fall on you.",
          :spectator_message=>"{{user}} manages to create a small flurry of snow to fall on {{target}}.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.01,
              :duration=>3.0
            }
          }
        },
        %{
          :user_message=>"Your attack causes shards of ice to cleave through {{target}}'s nose.",
          :target_message=>"{{user}}'s attack causes shards of ice to cleave through your nose.",
          :spectator_message=>"{{user}}'s attack causes shards of ice to cleave through {{target}}'s nose.",
          :effects=>%{
            :damage=>0.02,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-5,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your attack leaves heavy ice on {{target}}'s shoulders unbalancing {{target:him/her/it}}.",
          :target_message=>"{{user}}'s attack leaves heavy ice on your shoulders unbalancing you.",
          :spectator_message=>"{{user}}'s attack leaves heavy ice on {{target}}'s shoulders unbalancing {{target:him/her/it}}.",
          :effects=>%{
            :damage=>0.01,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-10,
                :duration=>6.0
              },
              %{
                :skill=>"parry",
                :amount=>-10,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} screams as bitter winds tear at {{target:him/her/it}} from your attack, leaving {{target:him/her/it}} frostbitten and cold.",
          :target_message=>"You scream as bitter winds tear at you from {{user}}'s attack, leaving you frostbitten and cold.",
          :spectator_message=>"{{target}} screams as bitter winds tear at {{target:him/her/it}} from {{user}}'s attack, leaving {{target:him/her/it}} frostbitten and cold.",
          :effects=>%{
            :damage=>0.05,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>4.0
            }
          }
        },
        %{
          :user_message=>"{{target}} moves visibly slower as his blood and joints suffer from your brutal cold.",
          :target_message=>"You move visibly slower as his blood and joints suffer from {{user}}'s brutal cold.",
          :spectator_message=>"{{target}} moves visibly slower as his blood and joints suffer from {{user}}'s brutal cold.",
          :effects=>%{
            :damage=>0.04,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-10,
                :duration=>6.0
              },
              %{
                :skill=>"parry",
                :amount=>-10,
                :duration=>4.0
              },
              %{
                :skill=>"dodge",
                :amount=>-10,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You hail stones of ice to beat {{target}} about the head and neck.",
          :target_message=>"{{user}} hails stones of ice to beat you about the head and neck.",
          :spectator_message=>"{{user}} hails stones of ice to beat {{target}} about the head and neck.",
          :effects=>%{
            :damage=>0.07,
            :stun=>2.0
          }
        },
        %{
          :user_message=>"You focus his efforts, freezing {{target}}'s arms in a thin layer of ice.",
          :target_message=>"{{user}} focus his efforts, freezing your arms in a thin layer of ice.",
          :spectator_message=>"{{user}} focus his efforts, freezing {{target}}'s arms in a thin layer of ice.",
          :effects=>%{
            :damage=>0.02,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>5.0
            },
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-25,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You fire burst of hail stones breaking {{target:his/her/its}} ribs.",
          :target_message=>"{{user}} fires burst of hail stones breaking your ribs.",
          :spectator_message=>"{{user}} fires burst of hail stones breaking {{target:his/her/its}} ribs.",
          :effects=>%{
            :damage=>0.1,
            :stun=>2.0,
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
          :user_message=>"Much of {{target:his/her/its}} skin cracks and falls away frozen from your attack.",
          :target_message=>"Much of your skin cracks and falls away frozen from {{user}}'s attack.",
          :spectator_message=>"Much of {{target:his/her/its}} skin cracks and falls away frozen from {{user}}'s attack.",
          :effects=>%{
            :damage=>0.02,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>4.0
            },
            :stun=>3.0,
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
          :user_message=>"You create an icy blast, catching {{target}}'s side cracking ribs and spinning {{target:him/her/it}} around.",
          :target_message=>"{{user}} creates an icy blast, catching your side cracking ribs and spinning you around.",
          :spectator_message=>"{{user}} creates an icy blast, catching {{target}}'s side cracking ribs and spinning {{target:him/her/it}} around.",
          :effects=>%{
            :damage=>0.1,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>4.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>3.0
            }
          }
        },
        %{
          :user_message=>"You attack knocking {{target}} back a few feet trailing ice and frozen air.",
          :target_message=>"{{user}} attacks knocking you back a few feet trailing ice and frozen air.",
          :spectator_message=>"{{user}} attacks knocking {{target}} back a few feet trailing ice and frozen air.",
          :effects=>%{
            :damage=>0.1,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-10,
                :duration=>6.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-20,
                :duration=>8.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>6.0
            }
          }
        },
        %{
          :user_message=>"{{target}} slows down growing tired under the heavy weighted ice created by you.",
          :target_message=>"You slow down growing tired under the heavy weighted ice created by {{user}}.",
          :spectator_message=>"{{target}} slows down growing tired under the heavy weighted ice created by {{user}}.",
          :effects=>%{
            :damage=>0.05,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-15,
                :duration=>5.0
              },
              %{
                :stat=>"agility",
                :amount=>-15,
                :duration=>4.0
              }
            ],
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-30,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You laugh as the ice claims {{target}}'s, leg turning it deep blue.",
          :target_message=>"{{user}} laughs as the ice claims your, leg turning it deep blue.",
          :spectator_message=>"{{user}} laughs as the ice claims {{target}}'s, leg turning it deep blue.",
          :effects=>%{
            :damage=>0.05,
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>7.0
            },
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"leg"
              }
            ]
          }
        },
        %{
          :user_message=>"Your bitter cold drives {{target}} to {{target:his/her/its}} knees, wimpering.",
          :target_message=>"{{user}}'s bitter cold drives you to your knees, wimpering.",
          :spectator_message=>"{{user}}'s bitter cold drives {{target}} to {{target:his/her/its}} knees, wimpering.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.01,
              :duration=>9.0
            },
            :stun=>1.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-40,
                :duration=>4.0
              },
              %{
                :skill=>"parry",
                :amount=>-10,
                :duration=>10.0
              }
            ],
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
          :user_message=>"You watch as {{target}}'s eyes shatter like glass, leaving behind strangely-colored ice shards.",
          :target_message=>"{{user}} watches as your eyes shatter like glass, leaving behind strangely-colored ice shards.",
          :spectator_message=>"{{user}} watches as {{target}}'s eyes shatter like glass, leaving behind strangely-colored ice shards.",
          :effects=>%{
            :damage=>0.2,
            :damage_over_time=>%{
              :damage=>0.08,
              :duration=>2.0
            },
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-5,
                :duration=>20.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-50,
                :duration=>8.0
              },
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>8.0
              }
            ],
            :stun=>15.0
          }
        },
        %{
          :user_message=>"{{target}} lurches forward, leaving {{target:his/her/its}} legs behind, not realizing that you had frozen them solid.",
          :target_message=>"You lurch forward, leaving your legs behind, not realizing that {{user}} had frozen them solid.",
          :spectator_message=>"{{target}} lurches forward, leaving {{target:his/her/its}} legs behind, not realizing that {{user}} had frozen them solid.",
          :effects=>%{
            :damage=>0.5,
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>2.0
            },
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-15,
                :duration=>10.0
              }
            ],
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"leg"
              },
              %{
                :kind=>"sever",
                :limb=>"leg"
              }
            ],
            :stun=>10.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>10.0
              },
              %{
                :skill=>"attack",
                :amount=>-100,
                :duration=>20.0
              }
            ]
          }
        }
      ],
      "C"=>[
        %{
          :user_message=>"{{target}} receives a mild chill from your attack.",
          :target_message=>"You receive a mild chill from {{user}}'s attack.",
          :spectator_message=>"{{target}} receives a mild chill from {{user}}'s attack.",
          :effects=>%{
            :damage=>0.04
          }
        },
        %{
          :user_message=>"You surprise {{target}} with a frosty blast!",
          :target_message=>"{{user}} surprises you with a frosty blast!",
          :spectator_message=>"{{user}} surprises {{target}} with a frosty blast!",
          :effects=>%{
            :damage=>0.08,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-20,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your attack causes {{target}} to shiver violently.",
          :target_message=>"{{user}}'s attack causes you to shiver violently.",
          :spectator_message=>"{{user}}'s attack causes {{target}} to shiver violently.",
          :effects=>%{
            :damage=>0.05,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>3.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-40,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"A blast of freezing wind from you knocks {{target}} several feet backward",
          :target_message=>"A blast of freezing wind from {{user}} knocks you several feet backward",
          :spectator_message=>"A blast of freezing wind from {{user}} knocks {{target}} several feet backward",
          :effects=>%{
            :damage=>0.15,
            :stun=>2.0
          }
        },
        %{
          :user_message=>"Your freezing attack causes {{target}}'s skin to freeze.",
          :target_message=>"{{user}}'s freezing attack causes your skin to freeze.",
          :spectator_message=>"{{user}}'s freezing attack causes {{target}}'s skin to freeze.",
          :effects=>%{
            :damage=>0.1,
            :damage_over_time=>%{
              :damage=>0.04,
              :duration=>4.0
            },
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>8.0
              }
            ],
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
          :user_message=>"A sickening crackle is heard as your attack shatters {{target}}'s teeth!",
          :target_message=>"A sickening crackle is heard as {{user}}'s attack shatters your teeth!",
          :spectator_message=>"A sickening crackle is heard as {{user}}'s attack shatters {{target}}'s teeth!",
          :effects=>%{
            :damage=>0.2,
            :stun=>3.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-25,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"Blood leaks from {{target}}'s nose and eyes as your freezing attack bursts blood vessels {{target:his/her/its}} face!",
          :target_message=>"Blood leaks from your nose and eyes as {{user}}'s freezing attack bursts blood vessels your face!",
          :spectator_message=>"Blood leaks from {{target}}'s nose and eyes as {{user}}'s freezing attack bursts blood vessels {{target:his/her/its}} face!",
          :effects=>%{
            :damage=>0.15,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>4.0
            },
            :stun=>1.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-30,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"Shards of jagged ice lacerate {{target}} as you summon a freezing attack.",
          :target_message=>"Shards of jagged ice lacerate you as {{user}} summons a freezing attack.",
          :spectator_message=>"Shards of jagged ice lacerate {{target}} as {{user}} summons a freezing attack.",
          :effects=>%{
            :damage=>0.25,
            :damage_over_time=>%{
              :damage=>0.01,
              :duration=>12.0
            }
          }
        },
        %{
          :user_message=>"{{target}}'s arm is completely frozen by your attack. As {{target:he/she/it}} attacks, {{target:his/her/its}} arm shatters violently!",
          :target_message=>"Your arm is completely frozen by {{user:his/her/its}} attack. As you attack, your arm shatters violently!",
          :spectator_message=>"{{target}}'s arm is completely frozen by {{user:his/her/its}} attack. As {{target:he/she/it}} attacks, {{target:his/her/its}} arm shatters violently!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"arm"
              }
            ],
            :stun=>2.0,
            :damage_over_time=>%{
              :damage=>0.04,
              :duration=>4.0
            }
          }
        },
        %{
          :user_message=>"You cast a freezing blast which crystallizes the muscles in {{target}}'s leg.",
          :target_message=>"{{user}} casts a freezing blast which crystallizes the muscles in your leg.",
          :spectator_message=>"{{user}} casts a freezing blast which crystallizes the muscles in {{target}}'s leg.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"leg"
              }
            ],
            :stun=>1.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-80,
                :duration=>4.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-25,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} is rooted to the spot by the cold blast of your attack, and {{target:he/she/it}} struggles to free {{target:him/her/it}}self.",
          :target_message=>"You is rooted to the spot by the cold blast of {{user}}'s attack, and you struggle to free youself.",
          :spectator_message=>"{{target}} is rooted to the spot by the cold blast of {{user}}'s attack, and {{target:he/she/it}} struggles to free {{target:him/her/it}}self.",
          :effects=>%{
            :stun=>5.0,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-100,
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
          :user_message=>"You freeze {{target}}'s fingers together with your attack, and further actions by {{target}} cause rending and cracking of flesh!",
          :target_message=>"{{user}} freezes your fingers together with {{user:his/her/its}} attack, and further actions by you cause rending and cracking of flesh!",
          :spectator_message=>"{{user}} freezes {{target}}'s fingers together with {{user:his/her/its}} attack, and further actions by {{target}} cause rending and cracking of flesh!",
          :effects=>%{
            :damage=>0.1,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>8.0
            },
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-35,
                :duration=>16.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}}'s arms and torso are frozen by your attack. Further movement causes {{target:his/her/its}} flesh to crack and bones to snap.",
          :target_message=>"Your arms and torso are frozen by {{user}}'s attack. Further movement causes your flesh to crack and bones to snap.",
          :spectator_message=>"{{target}}'s arms and torso are frozen by {{user}}'s attack. Further movement causes {{target:his/her/its}} flesh to crack and bones to snap.",
          :effects=>%{
            :damage=>0.3,
            :damage_over_time=>%{
              :damage=>0.08,
              :duration=>5.0
            },
            :stun=>2.0,
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
                :limb=>"arm"
              },
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} lungs are frozen solid as {{target:he/she/it}} inhales the freezing air from your attack!",
          :target_message=>"You lungs are frozen solid as you inhale the freezing air from {{user}}'s attack!",
          :spectator_message=>"{{target}} lungs are frozen solid as {{target:he/she/it}} inhales the freezing air from {{user}}'s attack!",
          :effects=>%{
            :damage=>0.3,
            :damage_over_time=>%{
              :damage=>0.04,
              :duration=>20.0
            },
            :stun=>2.0,
            :stat_mod=>[
              %{
                :stat=>"health",
                :amount=>-30,
                :duration=>40.0
              }
            ]
          }
        },
        %{
          :user_message=>"You project a focused cold blast straight into {{target}}'s vitals, exploding {{target:his/her/its}} heart like a water balloon!",
          :target_message=>"{{user}} projects a focused cold blast straight into your vitals, exploding your heart like a water balloon!",
          :spectator_message=>"{{user}} projects a focused cold blast straight into {{target}}'s vitals, exploding {{target:his/her/its}} heart like a water balloon!",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "D"=>[
        %{
          :user_message=>"You mildly cool {{target}} off with a cold wind.",
          :target_message=>"{{user}} mildly cools you off with a cold wind.",
          :spectator_message=>"{{user}} mildly cools {{target}} off with a cold wind.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You chill {{target}} with your cold blast.",
          :target_message=>"{{user}} chills you with {{user:his/her/its}} cold blast.",
          :spectator_message=>"{{user}} chills {{target}} with {{user:his/her/its}} cold blast.",
          :effects=>%{
            :damage=>0.07
          }
        },
        %{
          :user_message=>"{{target}}'s forearm is chilled by your attack.",
          :target_message=>"Your forearm is chilled by {{user}}'s attack.",
          :spectator_message=>"{{target}}'s forearm is chilled by {{user}}'s attack.",
          :effects=>%{
            :damage=>0.2
          }
        },
        %{
          :user_message=>"You cause icicles to form about {{target}}'s face, freezing the moisture on {{target}}'s eyeballs!",
          :target_message=>"{{user}} causes icicles to form about your face, freezing the moisture on your eyeballs!",
          :spectator_message=>"{{user}} causes icicles to form about {{target}}'s face, freezing the moisture on {{target}}'s eyeballs!",
          :effects=>%{
            :damage=>0.15,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>3.0
            },
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-20,
                :duration=>12.0
              }
            ]
          }
        },
        %{
          :user_message=>"You freeze {{target}}'s hand solid!",
          :target_message=>"{{user}} freezes your hand solid!",
          :spectator_message=>"{{user}} freezes {{target}}'s hand solid!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"hand"
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} are surprised by the extreme cold of your attack, as {{target:his/her/its}} upper body is surrounded by a freezing wind.",
          :target_message=>"You ares surprised by the extreme cold of {{user}}'s attack, as your upper body is surrounded by a freezing wind.",
          :spectator_message=>"{{target}} ares surprised by the extreme cold of {{user}}'s attack, as {{target:his/her/its}} upper body is surrounded by a freezing wind.",
          :effects=>%{
            :damage=>0.4,
            :damage_over_time=>%{
              :damage=>0.04,
              :duration=>5.0
            },
            :stun=>1.0
          }
        },
        %{
          :user_message=>"{{target}}'s legs are rooted to the ground momentarily by your freezing attack",
          :target_message=>"Your legs are rooted to the ground momentarily by {{user}}'s freezing attack",
          :spectator_message=>"{{target}}'s legs are rooted to the ground momentarily by {{user}}'s freezing attack",
          :effects=>%{
            :stun=>4.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>8.0
              },
              %{
                :skill=>"dodge",
                :amount=>-50,
                :duration=>12.0
              }
            ],
            :damage=>0.1
          }
        },
        %{
          :user_message=>"The muscles in {{target}}'s shoulder are crystallized by your attack!",
          :target_message=>"The muscles in your shoulder are crystallized by {{user}}'s attack!",
          :spectator_message=>"The muscles in {{target}}'s shoulder are crystallized by {{user}}'s attack!",
          :effects=>%{
            :damage=>0.2,
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
          :user_message=>"Your sub-freezing attack fills {{target}}'s lungs with air, instantly shattering them. {{target}} falls to the ground, lifeless, and bits of shattered lung issue forth from {{target:his/her/its}} mouth.",
          :target_message=>"{{user}}'s sub-freezing attack fills your lungs with air, instantly shattering them. You fall to the ground, lifeless, and bits of shattered lung issue forth from your mouth.",
          :spectator_message=>"{{user}}'s sub-freezing attack fills {{target}}'s lungs with air, instantly shattering them. {{Target}} falls to the ground, lifeless, and bits of shattered lung issue forth from {{target:his/her/its}} mouth.",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"{{target}} watches in horror as {{target:his/her/its}} arm is completely shattered by your attack!",
          :target_message=>"You watch in horror as your arm is completely shattered by {{user}}'s attack!",
          :spectator_message=>"{{target}} watches in horror as {{target:his/her/its}} arm is completely shattered by {{user}}'s attack!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"arm"
              }
            ],
            :stun=>1.0,
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
          :user_message=>"{{target}} is unbalanced by a freezing strike from your attack.",
          :target_message=>"You are unbalanced by a freezing strike from {{user}}'s attack.",
          :spectator_message=>"{{target}} is unbalanced by a freezing strike from {{user}}'s attack.",
          :effects=>%{
            :damage=>0.3,
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
              :duration=>10.0
            }
          }
        },
        %{
          :user_message=>"You completely freeze the skin on {{target}}'s torso, and further movement on {{target:his/her/its}} part results in the cracking of skin, which falls off, exposing the muscles underneath!",
          :target_message=>"{{user}} completely freezes the skin on your torso, and further movement on your part results in the cracking of skin, which falls off, exposing the muscles underneath!",
          :spectator_message=>"{{user}} completely freezes the skin on {{target}}'s torso, and further movement on {{target:his/her/its}} part results in the cracking of skin, which falls off, exposing the muscles underneath!",
          :effects=>%{
            :damage=>0.2,
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>5.0
            },
            :stun=>2.0,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-35,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your freezing attack causes {{target}} to fall backward. The impact shatters a variety of important internal tissues!",
          :target_message=>"{{user}}'s freezing attack causes you to fall backward. The impact shatters a variety of important internal tissues!",
          :spectator_message=>"{{user}}'s freezing attack causes {{target}} to fall backward. The impact shatters a variety of important internal tissues!",
          :effects=>%{
            :damage=>0.4,
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>8.0
            },
            :stun=>5.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>20.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-50,
                :duration=>20.0
              },
              %{
                :stat=>"intellect",
                :amount=>-50,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your freezing attack halts {{target}} in {{target:his/her/its}} tracks. You blow lightly on {{target}}'s head, which crumbles like a fine powder!",
          :target_message=>"{{user}}'s freezing attack halts you in your tracks. {{user}} blows lightly on your head, which crumbles like a fine powder!",
          :spectator_message=>"{{user}}'s freezing attack halts {{target}} in {{target:his/her/its}} tracks. {{user}} blows lightly on {{target}}'s head, which crumbles like a fine powder!",
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
          :user_message=>"{{target}} is frozen solid by your attack!  you watch gleefully as {{target}}'s limbs fall off in succession.",
          :target_message=>"You is frozen solid by {{user}}'s attack!  {{user}} watches gleefully as your limbs fall off in succession.",
          :spectator_message=>"{{target}} is frozen solid by {{user}}'s attack!  {{user}} watches gleefully as {{target}}'s limbs fall off in succession.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"random"
              },
              %{
                :kind=>"sever",
                :limb=>"random"
              },
              %{
                :kind=>"sever",
                :limb=>"random"
              },
              %{
                :kind=>"sever",
                :limb=>"random"
              },
              %{
                :kind=>"sever",
                :limb=>"random"
              }
            ],
            :kill=>true
          }
        }
      ],
      "E"=>[
        %{
          :user_message=>"You cackle as {{target}}....wait, that attack did absolutely squat!",
          :target_message=>"{{user}} cackles as you....wait, that attack did absolutely squat!",
          :spectator_message=>"{{user}} cackles as {{target}}....wait, that attack did absolutely squat!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You cause some ice crystals to form about {{target}}'s legs",
          :target_message=>"{{user}} causes some ice crystals to form about your legs",
          :spectator_message=>"{{user}} causes some ice crystals to form about {{target}}'s legs",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>5.0
            },
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-10,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}}'s side is frozen by {{target}}'s attack!",
          :target_message=>"Your side is frozen by your attack!",
          :spectator_message=>"{{target}}'s side is frozen by {{target}}'s attack!",
          :effects=>%{
            :damage=>0.2,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"The freezing wind from your attack is so cold that ice crystals form, lacerating {{target}}'s face and neck!",
          :target_message=>"The freezing wind from {{user}}'s attack is so cold that ice crystals form, lacerating your face and neck!",
          :spectator_message=>"The freezing wind from {{user}}'s attack is so cold that ice crystals form, lacerating {{target}}'s face and neck!",
          :effects=>%{
            :damage=>0.1,
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>3.0
            },
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-50,
                :duration=>6.0
              },
              %{
                :skill=>"parry",
                :amount=>-70,
                :duration=>6.0
              },
              %{
                :skill=>"attack",
                :amount=>-50,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You hit {{target}} right in the mouth with his attack, freezing {{target:his/her/its}} teeth and forcing them to crack. Blood oozes from {{target}}'s mouth!",
          :target_message=>"{{user}} hits you right in the mouth with his attack, freezing your teeth and forcing them to crack. Blood oozes from your mouth!",
          :spectator_message=>"{{user}} hits {{target}} right in the mouth with his attack, freezing {{target:his/her/its}} teeth and forcing them to crack. Blood oozes from {{target}}'s mouth!",
          :effects=>%{
            :damage=>0.15,
            :stun=>2.0,
            :damage_over_time=>%{
              :damage=>0.08,
              :duration=>5.0
            },
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-15,
                :duration=>10.0
              }
            ],
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>10.0
              },
              %{
                :skill=>"attack",
                :amount=>-100,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"You freeze {{target}}'s arm to {{target:his/her/its}} side with a powerful blast!  Further movement of the arm on {{target}}'s part causes ripping and cracking of frozen skin.",
          :target_message=>"{{user}} freezes your arm to your side with a powerful blast!  Further movement of the arm on your part causes ripping and cracking of frozen skin.",
          :spectator_message=>"{{user}} freezes {{target}}'s arm to {{target:his/her/its}} side with a powerful blast!  Further movement of the arm on {{target}}'s part causes ripping and cracking of frozen skin.",
          :effects=>%{
            :damage=>0.05,
            :damage_over_time=>%{
              :damage=>0.07,
              :duration=>8.0
            },
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-60,
                :duration=>16.0
              },
              %{
                :skill=>"attack",
                :amount=>-60,
                :duration=>16.0
              },
              %{
                :skill=>"block",
                :amount=>-40,
                :duration=>16.0
              }
            ]
          }
        },
        %{
          :user_message=>"It feels as if knives pierce into {{target}}'s lungs as {{target:he/she/it}} inhales the freezing air from your attack!",
          :target_message=>"It feels as if knives pierce into your lungs as you inhale the freezing air from {{user}}'s attack!",
          :spectator_message=>"It feels as if knives pierce into {{target}}'s lungs as {{target:he/she/it}} inhales the freezing air from {{user}}'s attack!",
          :effects=>%{
            :damage=>0.45,
            :stun=>3.0
          }
        },
        %{
          :user_message=>"An entire side of {{target}}'s face is stripped away as you release a blast of freezing air at {{target:he/she/it}}!",
          :target_message=>"An entire side of your face is stripped away as {{user}} releases a blast of freezing air at you!",
          :spectator_message=>"An entire side of {{target}}'s face is stripped away as {{user}} releases a blast of freezing air at {{target:he/she/it}}!",
          :effects=>%{
            :damage=>0.6,
            :stun=>4.0
          }
        },
        %{
          :user_message=>"{{target}} seems to pause momentarily, before falling backward, {{target:his/her/its}} torso shattering like a glass figurine, all thanks to you.",
          :target_message=>"You seem to pause momentarily, before falling backward, your torso shattering like a glass figurine, all thanks to {{user}}.",
          :spectator_message=>"{{target}} seems to pause momentarily, before falling backward, {{target:his/her/its}} torso shattering like a glass figurine, all thanks to {{user}}.",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"As {{target}} attacks, {{target:his/her/its}} arm cracks off at the shoulder and flies over your head!  It had been frozen solid by you!",
          :target_message=>"As you attack, your arm cracks off at the shoulder and flies over {{user}}'s head!  It had been frozen solid by {{user}}!",
          :spectator_message=>"As {{target}} attacks, {{target:his/her/its}} arm cracks off at the shoulder and flies over {{user}}'s head!  It had been frozen solid by {{user}}!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"arm"
              }
            ],
            :stun=>3.0
          }
        },
        %{
          :user_message=>"The tissues of {{target}}'s breathing passages are completely frozen by your attack. Blood issues forth from {{target}}'s mouth!",
          :target_message=>"The tissues of your breathing passages are completely frozen by {{user}}'s attack. Blood issues forth from your mouth!",
          :spectator_message=>"The tissues of {{target}}'s breathing passages are completely frozen by {{user}}'s attack. Blood issues forth from {{target}}'s mouth!",
          :effects=>%{
            :damage=>0.2,
            :damage_over_time=>%{
              :damage=>0.15,
              :duration=>7.0
            },
            :stun=>2.0
          }
        },
        %{
          :user_message=>"An entire side of {{target}}'s body is frozen solid by your attack!",
          :target_message=>"An entire side of your body is frozen solid by {{user}}'s attack!",
          :spectator_message=>"An entire side of {{target}}'s body is frozen solid by {{user}}'s attack!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"right arm"
              },
              %{
                :kind=>"cripple",
                :limb=>"right leg"
              }
            ],
            :damage=>0.7,
            :stun=>2.0
          }
        },
        %{
          :user_message=>"You cause {{target}}'s blood to congeal into a substance with the consistency of Jell-O.",
          :target_message=>"{{user}} causes your blood to congeal into a substance with the consistency of Jell-O.",
          :spectator_message=>"{{user}} causes {{target}}'s blood to congeal into a substance with the consistency of Jell-O.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.15,
              :duration=>9.0
            },
            :stun=>2.0
          }
        },
        %{
          :user_message=>"You encase {{target}} completely in ice with your attack. {{target}} will never return to the realm of the living. Oh well.",
          :target_message=>"{{user}} encases you completely in ice with {{user:his/her/its}} attack. You will never return to the realm of the living. Oh well.",
          :spectator_message=>"{{user}} encases {{target}} completely in ice with {{user:his/her/its}} attack. {{target}} will never return to the realm of the living. Oh well.",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"{{target}} spectacularly shatters into a thousand pieces as your attack hits {{target:him/her/it}} square in the chest!",
          :target_message=>"You spectacularly shatter into a thousand pieces as {{user}}'s attack hits you square in the chest!",
          :spectator_message=>"{{target}} spectacularly shatters into a thousand pieces as {{user}}'s attack hits {{target:him/her/it}} square in the chest!",
          :effects=>%{
            :kill=>true
          }
        }
      ]
    }
  end

end
