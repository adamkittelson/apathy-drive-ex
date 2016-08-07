defmodule ApathyDrive.CritTables.Impaling do

  def name do
    "impaling"
  end

  def damage_type do
    "physical"
  end

  def crits do
    %{
      "A"=>[
        %{
          :user_message=>"You poke at {{target}}.",
          :target_message=>"{{user}} pokes at you.",
          :spectator_message=>"{{user}} pokes at {{target}}.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You stab {{target}} a little.",
          :target_message=>"{{user}} stabs you a little.",
          :spectator_message=>"{{user}} stabs {{target}} a little.",
          :effects=>%{
            :damage=>0.02
          }
        },
        %{
          :user_message=>"You poke {{target}} in the face.",
          :target_message=>"{{user}} pokes you in the face.",
          :spectator_message=>"{{user}} pokes {{target}} in the face.",
          :effects=>%{
            :damage=>0.02,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"You jab {{target}} a few times.",
          :target_message=>"{{user}} jabs you a few times.",
          :spectator_message=>"{{user}} jabs {{target}} a few times.",
          :effects=>%{
            :damage=>0.04,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-15,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"You stab {{target}}, putting {{target:him/her/it}} on the defensive.",
          :target_message=>"{{user}} stabs you, putting you on the defensive.",
          :spectator_message=>"{{user}} stabs {{target}}, putting {{target:him/her/it}} on the defensive.",
          :effects=>%{
            :damage=>0.05,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-10,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"You stick {{target}}'s thigh, which was left carelessly unguarded.",
          :target_message=>"{{user}} sticks your thigh, which was left carelessly unguarded.",
          :spectator_message=>"{{user}} sticks {{target}}'s thigh, which was left carelessly unguarded.",
          :effects=>%{
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-5,
                :duration=>4.0
              }
            ],
            :damage=>0.03
          }
        },
        %{
          :user_message=>"You jab {{target}} in the hip. Ow!",
          :target_message=>"{{user}} jabs you in the hip. Ow!",
          :spectator_message=>"{{user}} jabs {{target}} in the hip. Ow!",
          :effects=>%{
            :damage=>0.06,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-10,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"You give {{target}} a stinging scratch on the face.",
          :target_message=>"{{user}} gives you a stinging scratch on the face.",
          :spectator_message=>"{{user}} gives {{target}} a stinging scratch on the face.",
          :effects=>%{
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-10,
                :duration=>12.0
              },
              %{
                :skill=>"parry",
                :amount=>-10,
                :duration=>18.0
              }
            ],
            :damage=>0.04
          }
        },
        %{
          :user_message=>"You bite into {{target}}'s skin.",
          :target_message=>"{{user}} bites into your skin.",
          :spectator_message=>"{{user}} bites into {{target}}'s skin.",
          :effects=>%{
            :damage=>0.07
          }
        },
        %{
          :user_message=>"You stab {{target}}'s abdomen...and it hurts.",
          :target_message=>"{{user}} stabs your abdomen...and it hurts.",
          :spectator_message=>"{{user}} stabs {{target}}'s abdomen...and it hurts.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>4.0
            },
            :stun=>1.0
          }
        },
        %{
          :user_message=>"You stab {{target}} in the shoulder.",
          :target_message=>"{{user}} stabs you in the shoulder.",
          :spectator_message=>"{{user}} stabs {{target}} in the shoulder.",
          :effects=>%{
            :damage=>0.06,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-10,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"You easily pierce {{target}}'s side.",
          :target_message=>"{{user}} easily pierces your side.",
          :spectator_message=>"{{user}} easily pierces {{target}}'s side.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"You poke a bloody hole in {{target}}.",
          :target_message=>"{{user}} pokes a bloody hole in you.",
          :spectator_message=>"{{user}} pokes a bloody hole in {{target}}.",
          :effects=>%{
            :damage=>0.07,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"You stab deeply into {{target}}'s shoulder, rendering {{target:his/her/its}} arm useless!",
          :target_message=>"{{user}} stabs deeply into your shoulder, rendering your arm useless!",
          :spectator_message=>"{{user}} stabs deeply into {{target}}'s shoulder, rendering {{target:his/her/its}} arm useless!",
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
          :user_message=>"You drive your point between {{target}}'s throat and collar. OUCH!",
          :target_message=>"{{user}} drives {{user:his/her/its}} point between your throat and collar. OUCH!",
          :spectator_message=>"{{user}} drives {{user:his/her/its}} point between {{target}}'s throat and collar. OUCH!",
          :effects=>%{
            :damage=>0.1,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-75,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"You thrust into {{target}}'s knee, tearing vital tendons!",
          :target_message=>"{{user}} thrusts into your knee, tearing vital tendons!",
          :spectator_message=>"{{user}} thrusts into {{target}}'s knee, tearing vital tendons!",
          :effects=>%{
            :damage=>0.06,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-10,
                :duration=>4.0
              },
              %{
                :skill=>"attack",
                :amount=>-20,
                :duration=>8.0
              },
              %{
                :skill=>"dodge",
                :amount=>-30,
                :duration=>12.0
              }
            ]
          }
        },
        %{
          :user_message=>"You puncture {{target}}'s upper arm, leaving it numb and soaked with blood.",
          :target_message=>"{{user}} punctures your upper arm, leaving it numb and soaked with blood.",
          :spectator_message=>"{{user}} punctures {{target}}'s upper arm, leaving it numb and soaked with blood.",
          :effects=>%{
            :damage=>0.05,
            :damage_over_time=>%{
              :damage=>0.01,
              :duration=>10.0
            },
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-25,
                :duration=>10.0
              },
              %{
                :skill=>"attack",
                :amount=>-10,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"You bury your lethal point between {{target}}'s ribs!",
          :target_message=>"{{user}} buries {{user:his/her/its}} lethal point between your ribs!",
          :spectator_message=>"{{user}} buries {{user:his/her/its}} lethal point between {{target}}'s ribs!",
          :effects=>%{
            :stun=>5.0,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>8.0
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
          :user_message=>"You surprise {{target}} with a frightening thrust into the chest!",
          :target_message=>"{{user}} surprises you with a frightening thrust into the chest!",
          :spectator_message=>"{{user}} surprises {{target}} with a frightening thrust into the chest!",
          :effects=>%{
            :damage=>0.15,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-200,
                :duration=>4.0
              },
              %{
                :skill=>"attack",
                :amount=>-20,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"You puncture a nasty hole in {{target}}'s gut, spilling much blood.",
          :target_message=>"{{user}} punctures a nasty hole in your gut, spilling much blood.",
          :spectator_message=>"{{user}} punctures a nasty hole in {{target}}'s gut, spilling much blood.",
          :effects=>%{
            :damage=>0.05,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"block",
                :amount=>-30,
                :duration=>10.0
              },
              %{
                :skill=>"parry",
                :amount=>-30,
                :duration=>10.0
              },
              %{
                :skill=>"attack",
                :amount=>-30,
                :duration=>10.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"You strike {{target}} in the face, leaving an unforgettable scar!",
          :target_message=>"{{user}} strikes you in the face, leaving an unforgettable scar!",
          :spectator_message=>"{{user}} strikes {{target}} in the face, leaving an unforgettable scar!",
          :effects=>%{
            :stun=>3.0,
            :damage=>0.08,
            :damage_over_time=>%{
              :damage=>0.01,
              :duration=>10.0
            },
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-10,
                :duration=>20.0
              },
              %{
                :skill=>"dodge",
                :amount=>-10,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"You thrust into {{target}}'s neck, through the jugular which bleeds profusely.",
          :target_message=>"{{user}} thrusts into your neck, through the jugular which bleeds profusely.",
          :spectator_message=>"{{user}} thrusts into {{target}}'s neck, through the jugular which bleeds profusely.",
          :effects=>%{
            :damage=>0.15,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-20,
                :duration=>10.0
              }
            ]
          }
        }
      ],
      "B"=>[
        %{
          :user_message=>"You deliver a glancing blow to {{target}}'s chest.",
          :target_message=>"{{user}} delivers a glancing blow to your chest.",
          :spectator_message=>"{{user}} delivers a glancing blow to {{target}}'s chest.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You pierce {{target}}'s skin.",
          :target_message=>"{{user}} pierces your skin.",
          :spectator_message=>"{{user}} pierces {{target}}'s skin.",
          :effects=>%{
            :damage=>0.03
          }
        },
        %{
          :user_message=>"You jab {{target}} in the side.",
          :target_message=>"{{user}} jabs you in the side.",
          :spectator_message=>"{{user}} jabs {{target}} in the side.",
          :effects=>%{
            :damage=>0.06,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-15,
                :duration=>1.0
              }
            ]
          }
        },
        %{
          :user_message=>"You stab {{target}} in the stomach.",
          :target_message=>"{{user}} stabs you in the stomach.",
          :spectator_message=>"{{user}} stabs {{target}} in the stomach.",
          :effects=>%{
            :damage=>0.04,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-10,
                :duration=>2.0
              }
            ],
            :stun=>1.0
          }
        },
        %{
          :user_message=>"You put a good thrust straight into {{target}}.",
          :target_message=>"{{user}} puts a good thrust straight into you.",
          :spectator_message=>"{{user}} puts a good thrust straight into {{target}}.",
          :effects=>%{
            :damage=>0.04,
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
          :user_message=>"You wound {{target}}'s leg.",
          :target_message=>"{{user}} wounds your leg.",
          :spectator_message=>"{{user}} wounds {{target}}'s leg.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>4.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-15,
                :duration=>8.0
              },
              %{
                :skill=>"dodge",
                :amount=>-15,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"You bite into {{target}}'s ribs.",
          :target_message=>"{{user}} bites into your ribs.",
          :spectator_message=>"{{user}} bites into {{target}}'s ribs.",
          :effects=>%{
            :damage=>0.07,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"You wound {{target}} in the chest.",
          :target_message=>"{{user}} wounds you in the chest.",
          :spectator_message=>"{{user}} wounds {{target}} in the chest.",
          :effects=>%{
            :damage=>0.03,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>2.0
            },
            :stun=>2.0
          }
        },
        %{
          :user_message=>"You pierce into {{target}}'s lower torso.",
          :target_message=>"{{user}} pierces into your lower torso.",
          :spectator_message=>"{{user}} pierces into {{target}}'s lower torso.",
          :effects=>%{
            :damage=>0.11,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-200,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"You jab your attack into {{target}}'s forearm.",
          :target_message=>"{{user}} jabs {{user:his/her/its}} attack into your forearm.",
          :spectator_message=>"{{user}} jabs {{user:his/her/its}} attack into {{target}}'s forearm.",
          :effects=>%{
            :damage=>0.07,
            :stun=>1.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-10,
                :duration=>8.0
              },
              %{
                :skill=>"parry",
                :amount=>-10,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"You strike {{target}}'s elbow, splitting bone and numbing {{target:his/her/its}} arm!",
          :target_message=>"{{user}} strikes your elbow, splitting bone and numbing your arm!",
          :spectator_message=>"{{user}} strikes {{target}}'s elbow, splitting bone and numbing {{target:his/her/its}} arm!",
          :effects=>%{
            :damage=>0.09,
            :stun=>4.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-200,
                :duration=>4.0
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
          :user_message=>"You pierce {{target}} in the throat!",
          :target_message=>"{{user}} pierces you in the throat!",
          :spectator_message=>"{{user}} pierces {{target}} in the throat!",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.06,
              :duration=>3.0
            },
            :stun=>3.0,
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
          :user_message=>"You puncture painfully through muscles in {{target}}'s calf.",
          :target_message=>"{{user}} punctures painfully through muscles in your calf.",
          :spectator_message=>"{{user}} punctures painfully through muscles in {{target}}'s calf.",
          :effects=>%{
            :damage=>0.05,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-30,
                :duration=>4.0
              },
              %{
                :skill=>"dodge",
                :amount=>-40,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"You pierce {{target}}'s arm deeply. Rather nasty.",
          :target_message=>"{{user}} pierces your arm deeply. Rather nasty.",
          :spectator_message=>"{{user}} pierces {{target}}'s arm deeply. Rather nasty.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>3.0
            },
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-30,
                :duration=>6.0
              },
              %{
                :skill=>"block",
                :amount=>-50,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"You thrust up into {{target}}'s vitals!!!",
          :target_message=>"{{user}} thrusts up into your vitals!!!",
          :spectator_message=>"{{user}} thrusts up into {{target}}'s vitals!!!",
          :effects=>%{
            :damage=>0.12,
            :damage_over_time=>%{
              :damage=>0.06,
              :duration=>3.0
            },
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>6.0
              },
              %{
                :skill=>"attack",
                :amount=>-25,
                :duration=>18.0
              },
              %{
                :skill=>"dodge",
                :amount=>-15,
                :duration=>12.0
              }
            ]
          }
        },
        %{
          :user_message=>"You fracture {{target}}'s skull with a powerful biting thrust!",
          :target_message=>"{{user}} fractures your skull with a powerful biting thrust!",
          :spectator_message=>"{{user}} fractures {{target}}'s skull with a powerful biting thrust!",
          :effects=>%{
            :stun=>5.0,
            :damage=>0.18,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>10.0
              },
              %{
                :skill=>"block",
                :amount=>-15,
                :duration=>20.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.07,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"You strike through {{target}}'s hip, leaving a nasty wound.",
          :target_message=>"{{user}} strikes through your hip, leaving a nasty wound.",
          :spectator_message=>"{{user}} strikes through {{target}}'s hip, leaving a nasty wound.",
          :effects=>%{
            :stun=>1.0,
            :damage=>0.15,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-25,
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
          :user_message=>"You strike through {{target}}'s neck, puncturing {{target:his/her/its}} most vital arteries!",
          :target_message=>"{{user}} strikes through your neck, puncturing your most vital arteries!",
          :spectator_message=>"{{user}} strikes through {{target}}'s neck, puncturing {{target:his/her/its}} most vital arteries!",
          :effects=>%{
            :stun=>4.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-20,
                :duration=>18.0
              }
            ],
            :damage=>0.25,
            :damage_over_time=>%{
              :damage=>0.09,
              :duration=>9.0
            }
          }
        },
        %{
          :user_message=>"You pierce {{target}} through the eye and into the brain. NICE MOVE!",
          :target_message=>"{{user}} pierces you through the eye and into the brain. NICE MOVE!",
          :spectator_message=>"{{user}} pierces {{target}} through the eye and into the brain. NICE MOVE!",
          :effects=>%{
            :stun=>8.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-40,
                :duration=>20.0
              }
            ],
            :damage=>0.4,
            :damage_over_time=>%{
              :damage=>0.08,
              :duration=>4.0
            }
          }
        }
      ],
      "C"=>[
        %{
          :user_message=>"You jab {{target}} nastily",
          :target_message=>"{{user}} jabs you nastily",
          :spectator_message=>"{{user}} jabs {{target}} nastily",
          :effects=>%{
            :damage=>0.02
          }
        },
        %{
          :user_message=>"You wound {{target}} with a stab.",
          :target_message=>"{{user}} wounds you with a stab.",
          :spectator_message=>"{{user}} wounds {{target}} with a stab.",
          :effects=>%{
            :damage=>0.05
          }
        },
        %{
          :user_message=>"You deliver a stinging thrust into {{target}}'s side.",
          :target_message=>"{{user}} delivers a stinging thrust into your side.",
          :spectator_message=>"{{user}} delivers a stinging thrust into {{target}}'s side.",
          :effects=>%{
            :damage=>0.09,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"You leap forward to stick {{target}} in the gut.",
          :target_message=>"{{user}} leaps forward to stick you in the gut.",
          :spectator_message=>"{{user}} leaps forward to stick {{target}} in the gut.",
          :effects=>%{
            :damage=>0.06,
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
          :user_message=>"You tear into {{target}}, ripping flesh from {{target}}'s bones.",
          :target_message=>"{{user}} tears into you, ripping flesh from your bones.",
          :spectator_message=>"{{user}} tears into {{target}}, ripping flesh from {{target}}'s bones.",
          :effects=>%{
            :damage=>0.08,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"You stab {{target}} wickedly, leaving a bloody wound above {{target:his/her/its}} groin.",
          :target_message=>"{{user}} stabs you wickedly, leaving a bloody wound above your groin.",
          :spectator_message=>"{{user}} stabs {{target}} wickedly, leaving a bloody wound above {{target:his/her/its}} groin.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>4.0
            },
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-15,
                :duration=>8.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-5,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"You injure {{target}} with a surprise thrust to the lower torso.",
          :target_message=>"{{user}} injures you with a surprise thrust to the lower torso.",
          :spectator_message=>"{{user}} injures {{target}} with a surprise thrust to the lower torso.",
          :effects=>%{
            :damage=>0.09,
            :stun=>2.0
          }
        },
        %{
          :user_message=>"You stab {{target}} just below the chest, drawing much blood.",
          :target_message=>"{{user}} stabs you just below the chest, drawing much blood.",
          :spectator_message=>"{{user}} stabs {{target}} just below the chest, drawing much blood.",
          :effects=>%{
            :damage=>0.13,
            :stun=>2.0
          }
        },
        %{
          :user_message=>"You pierce {{target}}'s upper leg most painfully!",
          :target_message=>"{{user}} pierces your upper leg most painfully!",
          :spectator_message=>"{{user}} pierces {{target}}'s upper leg most painfully!",
          :effects=>%{
            :damage=>0.05,
            :damage_over_time=>%{
              :damage=>0.04,
              :duration=>2.0
            },
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-15,
                :duration=>16.0
              }
            ]
          }
        },
        %{
          :user_message=>"You stab, puncturing deep into {{target}}'s wrist.",
          :target_message=>"{{user}} stabs, puncturing deep into your wrist.",
          :spectator_message=>"{{user}} stabs, puncturing deep into {{target}}'s wrist.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>3.0
            },
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-10,
                :duration=>12.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your strike shatter {{target}}'s knee, toppling {{target:him/her/it}} to the ground amidst cries of agony.",
          :target_message=>"{{user}}'s strike shatters your knee, toppling you to the ground amidst cries of agony.",
          :spectator_message=>"{{user}}'s strike shatters {{target}}'s knee, toppling {{target:him/her/it}} to the ground amidst cries of agony.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"leg"
              }
            ],
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-90,
                :duration=>6.0
              },
              %{
                :skill=>"attack",
                :amount=>-90,
                :duration=>12.0
              }
            ]
          }
        },
        %{
          :user_message=>"You drill {{target}} hard under the jaw, and {{target:he/she/it}} backs away clutching {{target:his/her/its}} bloody neck in horror.",
          :target_message=>"{{user}} drills you hard under the jaw, and you backs away clutching your bloody neck in horror.",
          :spectator_message=>"{{user}} drills {{target}} hard under the jaw, and {{target:he/she/it}} backs away clutching {{target:his/her/its}} bloody neck in horror.",
          :effects=>%{
            :stun=>4.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-200,
                :duration=>4.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>4.0
            }
          }
        },
        %{
          :user_message=>"You stab {{target}} squarely in the abdomen, and {{target:he/she/it}} doubles over in stifled pain.",
          :target_message=>"{{user}} stabs you squarely in the abdomen, and you doubles over in stifled pain.",
          :spectator_message=>"{{user}} stabs {{target}} squarely in the abdomen, and {{target:he/she/it}} doubles over in stifled pain.",
          :effects=>%{
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>4.0
              },
              %{
                :skill=>"attack",
                :amount=>-50,
                :duration=>10.0
              }
            ],
            :damage=>0.1
          }
        },
        %{
          :user_message=>"You thrust your attack through {{target}}'s upper arm and {{target:he/she/it}} falls back, moaning and bleeding badly.",
          :target_message=>"{{user}} thrusts {{user:his/her/its}} attack through your upper arm and you falls back, moaning and bleeding badly.",
          :spectator_message=>"{{user}} thrusts {{user:his/her/its}} attack through {{target}}'s upper arm and {{target:he/she/it}} falls back, moaning and bleeding badly.",
          :effects=>%{
            :stun=>6.0,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>6.0
            },
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-25,
                :duration=>20.0
              },
              %{
                :skill=>"block",
                :amount=>-25,
                :duration=>30.0
              }
            ]
          }
        },
        %{
          :user_message=>"You drive a fearful wound into {{target}}'s side!",
          :target_message=>"{{user}} drives a fearful wound into your side!",
          :spectator_message=>"{{user}} drives a fearful wound into {{target}}'s side!",
          :effects=>%{
            :damage=>0.09,
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-25,
                :duration=>10.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"Your nasty thrust to {{target}}'s face knock {{target:him/her/it}} down, wiping blood from {{target:his/her/its}} eyes.",
          :target_message=>"{{user}}'s nasty thrust to your face knocks you down, wiping blood from your eyes.",
          :spectator_message=>"{{user}}'s nasty thrust to {{target}}'s face knocks {{target:him/her/it}} down, wiping blood from {{target:his/her/its}} eyes.",
          :effects=>%{
            :damage=>0.08,
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
          :user_message=>"You thrust your attack straight through {{target}}'s chest!!",
          :target_message=>"{{user}} thrusts {{user:his/her/its}} attack straight through your chest!!",
          :spectator_message=>"{{user}} thrusts {{user:his/her/its}} attack straight through {{target}}'s chest!!",
          :effects=>%{
            :damage=>0.22,
            :stun=>9.0,
            :damage_over_time=>%{
              :damage=>0.2,
              :duration=>9.0
            }
          }
        },
        %{
          :user_message=>"You pierce adeptly right through {{target}}'s neck and spinal column with a frightening CRACK!! Last sound {{target}} will ever hear....",
          :target_message=>"{{user}} pierces adeptly right through your neck and spinal column with a frightening CRACK!! Last sound you will ever hear....",
          :spectator_message=>"{{user}} pierces adeptly right through {{target}}'s neck and spinal column with a frightening CRACK!! Last sound {{target}} will ever hear....",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"You thrust into {{target}}'s vitals, then with a violent twist of your attack, {{target}}'s entrails and arteries become a homogenous soup.",
          :target_message=>"{{user}} thrusts into your vitals, then with a violent twist of {{user:his/her/its}} attack, your entrails and arteries become a homogenous soup.",
          :spectator_message=>"{{user}} thrusts into {{target}}'s vitals, then with a violent twist of {{user:his/her/its}} attack, {{target}}'s entrails and arteries become a homogenous soup.",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "D"=>[
        %{
          :user_message=>"You stab {{target}} quite nastily",
          :target_message=>"{{user}} stabs you quite nastily",
          :spectator_message=>"{{user}} stabs {{target}} quite nastily",
          :effects=>%{
            :damage=>0.04
          }
        },
        %{
          :user_message=>"You stab {{target}} quite visciously.",
          :target_message=>"{{user}} stabs you quite visciously.",
          :spectator_message=>"{{user}} stabs {{target}} quite visciously.",
          :effects=>%{
            :damage=>0.09
          }
        },
        %{
          :user_message=>"You stab {{target}} most painfully.",
          :target_message=>"{{user}} stabs you most painfully.",
          :spectator_message=>"{{user}} stabs {{target}} most painfully.",
          :effects=>%{
            :damage=>0.07,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"You wound {{target}} with a solid thrust to {{target:his/her/its}} side.",
          :target_message=>"{{user}} wounds you with a solid thrust to your side.",
          :spectator_message=>"{{user}} wounds {{target}} with a solid thrust to {{target:his/her/its}} side.",
          :effects=>%{
            :damage=>0.1,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-15,
                :duration=>12.0
              }
            ]
          }
        },
        %{
          :user_message=>"You strike into {{target}}'s ribs!",
          :target_message=>"{{user}} strikes into your ribs!",
          :spectator_message=>"{{user}} strikes into {{target}}'s ribs!",
          :effects=>%{
            :damage=>0.08,
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
          :user_message=>"You puncture a major vein in {{target}}'s leg.",
          :target_message=>"{{user}} punctures a major vein in your leg.",
          :spectator_message=>"{{user}} punctures a major vein in {{target}}'s leg.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>10.0
            },
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-15,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"You stab into {{target}}'s vitals, spilling {{target:his/her/its}} blood.",
          :target_message=>"{{user}} stabs into your vitals, spilling your blood.",
          :spectator_message=>"{{user}} stabs into {{target}}'s vitals, spilling {{target:his/her/its}} blood.",
          :effects=>%{
            :damage=>0.06,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-25,
                :duration=>8.0
              },
              %{
                :skill=>"block",
                :amount=>-25,
                :duration=>8.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.04,
              :duration=>4.0
            }
          }
        },
        %{
          :user_message=>"You wound {{target}} in the chest with a forceful thrust!",
          :target_message=>"{{user}} wounds you in the chest with a forceful thrust!",
          :spectator_message=>"{{user}} wounds {{target}} in the chest with a forceful thrust!",
          :effects=>%{
            :damage=>0.1,
            :stun=>1.0,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>7.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-15,
                :duration=>14.0
              },
              %{
                :skill=>"block",
                :amount=>-15,
                :duration=>14.0
              }
            ]
          }
        },
        %{
          :user_message=>"You puncture {{target}} nastily in the thigh, making {{target:his/her/its}} knees weak.",
          :target_message=>"{{user}} punctures you nastily in the thigh, making your knees weak.",
          :spectator_message=>"{{user}} punctures {{target}} nastily in the thigh, making {{target:his/her/its}} knees weak.",
          :effects=>%{
            :damage=>0.12,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-75,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"You pierce {{target}}'s forearm, biting bone and leaving grave wounds!",
          :target_message=>"{{user}} pierces your forearm, biting bone and leaving grave wounds!",
          :spectator_message=>"{{user}} pierces {{target}}'s forearm, biting bone and leaving grave wounds!",
          :effects=>%{
            :damage=>0.06,
            :damage_over_time=>%{
              :damage=>0.04,
              :duration=>2.0
            },
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-10,
                :duration=>4.0
              },
              %{
                :skill=>"defense",
                :amount=>-50,
                :duration=>4.0
              },
              %{
                :skill=>"attack",
                :amount=>-50,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"You shaft {{target}} in the head!!! {{target:his/her/its}} skull nearly splits in half!",
          :target_message=>"{{user}} shafts you in the head!!! your skull nearly splits in half!",
          :spectator_message=>"{{user}} shafts {{target}} in the head!!! {{target:his/her/its}} skull nearly splits in half!",
          :effects=>%{
            :damage=>0.36,
            :stun=>9.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>18.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-35,
                :duration=>18.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.21,
              :duration=>9.0
            }
          }
        },
        %{
          :user_message=>"You strike hard into {{target}}'s shoulder, spinning {{target:his/her/its}} around dizzily.",
          :target_message=>"{{user}} strikes hard into your shoulder, spinning your around dizzily.",
          :spectator_message=>"{{user}} strikes hard into {{target}}'s shoulder, spinning {{target:his/her/its}} around dizzily.",
          :effects=>%{
            :damage=>0.07,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-20,
                :duration=>14.0
              },
              %{
                :skill=>"attack",
                :amount=>-20,
                :duration=>14.0
              }
            ]
          }
        },
        %{
          :user_message=>"You strike {{target}} wickedly in the lower leg, spilling blood and broken cartilage on the ground.",
          :target_message=>"{{user}} strikes you wickedly in the lower leg, spilling blood and broken cartilage on the ground.",
          :spectator_message=>"{{user}} strikes {{target}} wickedly in the lower leg, spilling blood and broken cartilage on the ground.",
          :effects=>%{
            :damage=>0.12,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>4.0
              },
              %{
                :skill=>"dodge",
                :amount=>-50,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"You pierce powerfully through {{target}}'s shoulder, driving {{target:him/her/it}} to step back in pain, gripping the bleeding, lifeless member.",
          :target_message=>"{{user}} pierces powerfully through your shoulder, driving you to step back in pain, gripping the bleeding, lifeless member.",
          :spectator_message=>"{{user}} pierces powerfully through {{target}}'s shoulder, driving {{target:him/her/it}} to step back in pain, gripping the bleeding, lifeless member.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :stun=>6.0,
            :damage_over_time=>%{
              :damage=>0.06,
              :duration=>6.0
            },
            :damage=>0.03
          }
        },
        %{
          :user_message=>"You thrust your attack frighteningly deep into {{target}}'s gut, who is bleeding badly and is in shock with terror!",
          :target_message=>"{{user}} thrusts {{user:his/her/its}} attack frighteningly deep into your gut, who is bleeding badly and is in shock with terror!",
          :spectator_message=>"{{user}} thrusts {{user:his/her/its}} attack frighteningly deep into {{target}}'s gut, who is bleeding badly and is in shock with terror!",
          :effects=>%{
            :damage=>0.2,
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>6.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.12,
              :duration=>3.0
            }
          }
        },
        %{
          :user_message=>"You stab through {{target}}'s vitals, puncturing {{target:his/her/its}} kidney and leaving {{target:him/her/it}} on the ground, horribly wounded.",
          :target_message=>"{{user}} stabs through your vitals, puncturing your kidney and leaving you on the ground, horribly wounded.",
          :spectator_message=>"{{user}} stabs through {{target}}'s vitals, puncturing {{target:his/her/its}} kidney and leaving {{target:him/her/it}} on the ground, horribly wounded.",
          :effects=>%{
            :stun=>9.0,
            :damage=>0.23,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-20,
                :duration=>40.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.11,
              :duration=>66.0
            }
          }
        },
        %{
          :user_message=>"You grab {{target}} by the arm before pulling {{target:him/her/it}} roughly onto your blade, ruthlessly impaling {{target:him/her/it}}!",
          :target_message=>"{{user}} grabs you by the arm before pulling you roughly onto {{user:his/her/its}} blade, ruthlessly impaling you!",
          :spectator_message=>"{{user}} grabs {{target}} by the arm before pulling {{target:him/her/it}} roughly onto {{user:his/her/its}} blade, ruthlessly impaling {{target:him/her/it}}!",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"You drive your attack with amazing force directly through {{target}}'s heart, spilling blood everywhere. {{target}}'s corpse quivers on the ground.",
          :target_message=>"{{user}} drives {{user:his/her/its}} attack with amazing force directly through your heart, spilling blood everywhere. Your corpse quivers on the ground.",
          :spectator_message=>"{{user}} drives {{user:his/her/its}} attack with amazing force directly through {{target}}'s heart, spilling blood everywhere. {{target}}'s corpse quivers on the ground.",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"You pierce {{target}} up through the tender neck and into {{target:his/her/its}} brain!! Disgusting.",
          :target_message=>"{{user}} pierces you up through the tender neck and into your brain!! Disgusting.",
          :spectator_message=>"{{user}} pierces {{target}} up through the tender neck and into {{target:his/her/its}} brain!! Disgusting.",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "E"=>[
        %{
          :user_message=>"You stab {{target}} rather wickedly.",
          :target_message=>"{{user}} stabs you rather wickedly.",
          :spectator_message=>"{{user}} stabs {{target}} rather wickedly.",
          :effects=>%{
            :damage=>0.08,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-20,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"You nastily stab {{target}} in the chest!",
          :target_message=>"{{user}} nastily stabs you in the chest!",
          :spectator_message=>"{{user}} nastily stabs {{target}} in the chest!",
          :effects=>%{
            :damage=>0.14,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"You painfully pierce into {{target}}'s gut!",
          :target_message=>"{{user}} painfully pierces into your gut!",
          :spectator_message=>"{{user}} painfully pierces into {{target}}'s gut!",
          :effects=>%{
            :damage=>0.11,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-80,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"You thrust your attack deep into {{target}}'s hip!",
          :target_message=>"{{user}} thrusts {{user:his/her/its}} attack deep into your hip!",
          :spectator_message=>"{{user}} thrusts {{user:his/her/its}} attack deep into {{target}}'s hip!",
          :effects=>%{
            :damage=>0.13,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-30,
                :duration=>10.0
              },
              %{
                :skill=>"attack",
                :amount=>-15,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You thrust your attack forcefully between {{target}}'s ribs, piercing {{target:his/her/its}} organs!",
          :target_message=>"{{user}} thrusts {{user:his/her/its}} attack forcefully between your ribs, piercing your organs!",
          :spectator_message=>"{{user}} thrusts {{user:his/her/its}} attack forcefully between {{target}}'s ribs, piercing {{target:his/her/its}} organs!",
          :effects=>%{
            :damage=>0.21,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-25,
                :duration=>8.0
              },
              %{
                :skill=>"block",
                :amount=>-25,
                :duration=>8.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>6.0
            }
          }
        },
        %{
          :user_message=>"You drill {{target}} square in the belly, opening a terrible wound that pours blood and entrails!!",
          :target_message=>"{{user}} drills you square in the belly, opening a terrible wound that pours blood and entrails!!",
          :spectator_message=>"{{user}} drills {{target}} square in the belly, opening a terrible wound that pours blood and entrails!!",
          :effects=>%{
            :stun=>3.0,
            :damage=>0.08,
            :damage_over_time=>%{
              :damage=>0.08,
              :duration=>5.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-10,
                :duration=>14.0
              }
            ]
          }
        },
        %{
          :user_message=>"You pierce into {{target}}'s arm, puncturing arteries and splitting bones! The arm hangs limp at the side of the terrified {{target}}!",
          :target_message=>"{{user}} pierces into your arm, puncturing arteries and splitting bones! The arm hangs limp at the side of the terrified you!",
          :spectator_message=>"{{user}} pierces into {{target}}'s arm, puncturing arteries and splitting bones! The arm hangs limp at the side of the terrified {{target}}!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>11.0
            },
            :stun=>4.0
          }
        },
        %{
          :user_message=>"You stab ruthlessly straight through {{target}}'s leg, sending {{target:him/her/it}} to the ground, curling in flowing blood!",
          :target_message=>"{{user}} stabs ruthlessly straight through your leg, sending you to the ground, curling in flowing blood!",
          :spectator_message=>"{{user}} stabs ruthlessly straight through {{target}}'s leg, sending {{target:him/her/it}} to the ground, curling in flowing blood!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"leg"
              }
            ],
            :damage_over_time=>%{
              :damage=>0.06,
              :duration=>7.0
            },
            :stun=>6.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>12.0
              }
            ]
          }
        },
        %{
          :user_message=>"You dive upon {{target}}, driving your attack brutally through {{target}}'s vital organs!!",
          :target_message=>"{{user}} dives upon you, driving {{user:his/her/its}} attack brutally through your vital organs!!",
          :spectator_message=>"{{user}} dives upon {{target}}, driving {{user:his/her/its}} attack brutally through {{target}}'s vital organs!!",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"You pierce right through {{target}}'s arm, which is then ripped off mercilessly with a twist of your attack!!",
          :target_message=>"{{user}} pierces right through your arm, which is then ripped off mercilessly with a twist of {{user}}'s attack!!",
          :spectator_message=>"{{user}} pierces right through {{target}}'s arm, which is then ripped off mercilessly with a twist of {{user}}'s attack!!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"arm"
              }
            ],
            :damage=>0.3,
            :stun=>8.0,
            :damage_over_time=>%{
              :damage=>0.08,
              :duration=>8.0
            }
          }
        },
        %{
          :user_message=>"You put a quick and powerful thrust into {{target}}'s neck!",
          :target_message=>"{{user}} puts a quick and powerful thrust into your neck!",
          :spectator_message=>"{{user}} puts a quick and powerful thrust into {{target}}'s neck!",
          :effects=>%{
            :damage=>0.19,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-30,
                :duration=>4.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>9.0
            }
          }
        },
        %{
          :user_message=>"You lunge forward, thrusting your attack relentlessly through {{target}}'s lower body!!",
          :target_message=>"{{user}} lunges forward, thrusting {{user:his/her/its}} attack relentlessly through your lower body!!",
          :spectator_message=>"{{user}} lunges forward, thrusting {{user:his/her/its}} attack relentlessly through {{target}}'s lower body!!",
          :effects=>%{
            :damage=>0.5,
            :stun=>10.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-40,
                :duration=>20.0
              },
              %{
                :skill=>"block",
                :amount=>-20,
                :duration=>20.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.07,
              :duration=>10.0
            }
          }
        },
        %{
          :user_message=>"You puncture through {{target}}'s gut, disrupting {{target:his/her/its}} spine on the other side...CRACK!",
          :target_message=>"{{user}} punctures through your gut, disrupting your spine on the other side...CRACK!",
          :spectator_message=>"{{user}} punctures through {{target}}'s gut, disrupting {{target:his/her/its}} spine on the other side...CRACK!",
          :effects=>%{
            :damage=>1.25,
            :stun=>8.0,
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              },
              %{
                :kind=>"cripple",
                :limb=>"leg"
              }
            ]
          }
        },
        %{
          :user_message=>"You stab hard through {{target}}'s arm and then through the side of {{target:his/her/its}} neck. VERY NASTY!!!!",
          :target_message=>"{{user}} stabs hard through your arm and then through the side of your neck. VERY NASTY!!!!",
          :spectator_message=>"{{user}} stabs hard through {{target}}'s arm and then through the side of {{target:his/her/its}} neck. VERY NASTY!!!!",
          :effects=>%{
            :damage=>0.85,
            :stun=>5.0,
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :damage_over_time=>%{
              :damage=>0.45,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"You ruthlessly impale {{target}} through {{target:his/her/its}} most vital organs. Blood spills from {{target}}'s mouth and runs down to {{target:his/her/its}} twitching fingers, then drips to the ground.",
          :target_message=>"{{user}} ruthlessly impales you through your most vital organs. Blood spills from your mouth and runs down to your twitching fingers, then drips to the ground.",
          :spectator_message=>"{{user}} ruthlessly impales {{target}} through {{target:his/her/its}} most vital organs. Blood spills from {{target}}'s mouth and runs down to {{target:his/her/its}} twitching fingers, then drips to the ground.",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"You thrust powerfully down upon {{target}}, fully impaling {{target:him/her/it}}, and spearing {{target:him/her/it}} to the ground below. Not a pretty sight.",
          :target_message=>"{{user}} thrusts powerfully down upon you, fully impaling you, and spearing you to the ground below. Not a pretty sight.",
          :spectator_message=>"{{user}} thrusts powerfully down upon {{target}}, fully impaling {{target:him/her/it}}, and spearing {{target:him/her/it}} to the ground below. Not a pretty sight.",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"You, after arresting {{target}} by {{target:his/her/its}} neck, drive your attack carefully right through {{target}}'s heart, not letting go until {{target:he/she/it}} expires.",
          :target_message=>"{{user}}, after arresting you by your neck, drives {{user:his/her/its}} attack carefully right through your heart, not letting go until you expires.",
          :spectator_message=>"{{user}}, after arresting {{target}} by {{target:his/her/its}} neck, drives {{user:his/her/its}} attack carefully right through {{target}}'s heart, not letting go until {{target:he/she/it}} expires.",
          :effects=>%{
            :kill=>true
          }
        }
      ]
    }
  end

end
