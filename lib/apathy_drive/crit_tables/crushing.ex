defmodule ApathyDrive.CritTables.Crushing do

  def name do
    "crushing"
  end

  def damage_type do
    "physical"
  end

  def crits do
    %{
      "A"=>[
        %{
          :user_message=>"Your blow was too weak to really harm {{target}}.",
          :target_message=>"{{user}}'s blow was too weak to really harm you.",
          :spectator_message=>"{{user}}'s blow was too weak to really harm {{target}}.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You whack {{target}}.",
          :target_message=>"{{user}} whacks you.",
          :spectator_message=>"{{user}} whacks {{target}}.",
          :effects=>%{
            :damage=>0.01
          }
        },
        %{
          :user_message=>"You deliver a glancing blow to {{target}}'s head.",
          :target_message=>"{{user}} delivers a glancing blow to your head.",
          :spectator_message=>"{{user}} delivers a glancing blow to {{target}}'s head.",
          :effects=>%{
            :damage=>0.03,
            :skill_mod=>[
              %{
                :skill=>"attack",
                :amount=>-40,
                :duration=>2.0
              },
              %{
                :skill=>"dodge",
                :amount=>-15,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"You surprise {{target}} with a solid hit.",
          :target_message=>"{{user}} surprises you with a solid hit.",
          :spectator_message=>"{{user}} surprises {{target}} with a solid hit.",
          :effects=>%{
            :damage=>0.02,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"You smack {{target}} hard.",
          :target_message=>"{{user}} smacks you hard.",
          :spectator_message=>"{{user}} smacks {{target}} hard.",
          :effects=>%{
            :damage=>0.03,
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
          :user_message=>"You pound {{target}} good.",
          :target_message=>"{{user}} pounds you good.",
          :spectator_message=>"{{user}} pounds {{target}} good.",
          :effects=>%{
            :damage=>0.06,
            :skill_mod=>[
              %{
                :skill=>"attack",
                :amount=>-10,
                :duration=>2.0
              },
              %{
                :skill=>"parry",
                :amount=>-10,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"You hurt {{target}} with a hard but clumsy strike.",
          :target_message=>"{{user}} hurts you with a hard but clumsy strike.",
          :spectator_message=>"{{user}} hurts {{target}} with a hard but clumsy strike.",
          :effects=>%{
            :damage=>0.04,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-25,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"You bruise {{target}} with a blow to the ribs.",
          :target_message=>"{{user}} bruises you with a blow to the ribs.",
          :spectator_message=>"{{user}} bruises {{target}} with a blow to the ribs.",
          :effects=>%{
            :damage=>0.05,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-25,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"You strike {{target}} hard in the thigh.",
          :target_message=>"{{user}} strikes you hard in the thigh.",
          :spectator_message=>"{{user}} strikes {{target}} hard in the thigh.",
          :effects=>%{
            :damage=>0.06,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-20,
                :duration=>2.0
              },
              %{
                :skill=>"dodge",
                :amount=>-15,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You painfully whack {{target}} in the forearm.",
          :target_message=>"{{user}} painfully whacks you in the forearm.",
          :spectator_message=>"{{user}} painfully whacks {{target}} in the forearm.",
          :effects=>%{
            :damage=>0.05,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"attack",
                :amount=>-20,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You completely shatter {{target}}'s shoulder with a crushing blow!",
          :target_message=>"{{user}} completely shatters your shoulder with a crushing blow!",
          :spectator_message=>"{{user}} completely shatters {{target}}'s shoulder with a crushing blow!",
          :effects=>%{
            :damage=>0.08,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-110,
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
          :user_message=>"You deliver a solid downward blow to {{target}}'s chest that sends {{target:him/her/it}} reeling.",
          :target_message=>"{{user}} delivers a solid downward blow to your chest that sends you reeling.",
          :spectator_message=>"{{user}} delivers a solid downward blow to {{target}}'s chest that sends {{target:him/her/it}} reeling.",
          :effects=>%{
            :damage=>0.08,
            :stun=>3.0,
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
          :user_message=>"You swing at {{target}}'s leg, connecting with a PAINFUL blow.",
          :target_message=>"{{user}} swings at your leg, connecting with a PAINFUL blow.",
          :spectator_message=>"{{user}} swings at {{target}}'s leg, connecting with a PAINFUL blow.",
          :effects=>%{
            :damage=>0.05,
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>2.0
              },
              %{
                :skill=>"attack",
                :amount=>-20,
                :duration=>12.0
              },
              %{
                :skill=>"dodge",
                :amount=>-20,
                :duration=>18.0
              }
            ]
          }
        },
        %{
          :user_message=>"You knock {{target}} around with a blow to {{target:his/her/its}} upper arm.",
          :target_message=>"{{user}} knocks you around with a blow to your upper arm.",
          :spectator_message=>"{{user}} knocks {{target}} around with a blow to {{target:his/her/its}} upper arm.",
          :effects=>%{
            :damage=>0.05,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-25,
                :duration=>10.0
              },
              %{
                :skill=>"attack",
                :amount=>-25,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"You swing a strong blow into {{target}}'s side, making {{target:him/her/it}} cringe!",
          :target_message=>"{{user}} swings a strong blow into your side, making you cringe!",
          :spectator_message=>"{{user}} swings a strong blow into {{target}}'s side, making {{target:him/her/it}} cringe!",
          :effects=>%{
            :damage=>0.1,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-40,
                :duration=>4.0
              },
              %{
                :skill=>"attack",
                :amount=>-20,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"You strike a frightening blow to {{target}}'s bones!",
          :target_message=>"{{user}} strikes a frightening blow to your bones!",
          :spectator_message=>"{{user}} strikes a frightening blow to {{target}}'s bones!",
          :effects=>%{
            :damage=>0.12,
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-25,
                :duration=>12.0
              },
              %{
                :skill=>"block",
                :amount=>-25,
                :duration=>12.0
              },
              %{
                :skill=>"attack",
                :amount=>-25,
                :duration=>12.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your powerful strike to {{target}}'s face breaks {{target:his/her/its}} nose!",
          :target_message=>"{{user}}'s powerful strike to your face breaks your nose!",
          :spectator_message=>"{{user}}'s powerful strike to {{target}}'s face breaks {{target:his/her/its}} nose!",
          :effects=>%{
            :damage=>0.15,
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-30,
                :duration=>20.0
              },
              %{
                :skill=>"attack",
                :amount=>-30,
                :duration=>20.0
              },
              %{
                :skill=>"dodge",
                :amount=>-30,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"You forcefully strike {{target}} in the head, knocking {{target:him/her/it}} to the ground!",
          :target_message=>"{{user}} forcefully strikes you in the head, knocking you to the ground!",
          :spectator_message=>"{{user}} forcefully strikes {{target}} in the head, knocking {{target:him/her/it}} to the ground!",
          :effects=>%{
            :damage=>0.2,
            :stun=>6.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-15,
                :duration=>12.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>6.0
            }
          }
        },
        %{
          :user_message=>"Your crushing blow to {{target}}'s jaw sends splinters of {{target:his/her/its}} skull flying!!",
          :target_message=>"{{user}}'s crushing blow to your jaw sends splinters of your skull flying!!",
          :spectator_message=>"{{user}}'s crushing blow to {{target}}'s jaw sends splinters of {{target:his/her/its}} skull flying!!",
          :effects=>%{
            :damage=>0.5,
            :stun=>10.0,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>10.0
            }
          }
        }
      ],
      "B"=>[
        %{
          :user_message=>"You connect a clumsy blow to {{target}}.",
          :target_message=>"{{user}} connects a clumsy blow to you.",
          :spectator_message=>"{{user}} connects a clumsy blow to {{target}}.",
          :effects=>%{
            :damage=>0.01
          }
        },
        %{
          :user_message=>"You whack {{target}} good.",
          :target_message=>"{{user}} whacks you good.",
          :spectator_message=>"{{user}} whacks {{target}} good.",
          :effects=>%{
            :damage=>0.02
          }
        },
        %{
          :user_message=>"You sharply smack {{target}}.",
          :target_message=>"{{user}} sharply smacks you.",
          :spectator_message=>"{{user}} sharply smacks {{target}}.",
          :effects=>%{
            :damage=>0.03,
            :skill_mod=>[
              %{
                :skill=>"attack",
                :amount=>-15,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"You strike {{target}} painfully in the ribs.",
          :target_message=>"{{user}} strikes you painfully in the ribs.",
          :spectator_message=>"{{user}} strikes {{target}} painfully in the ribs.",
          :effects=>%{
            :damage=>0.04,
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
          :user_message=>"You strike {{target}} hard, knocking {{target:him/her/it}} off balance.",
          :target_message=>"{{user}} strikes you hard, knocking you off balance.",
          :spectator_message=>"{{user}} strikes {{target}} hard, knocking {{target:him/her/it}} off balance.",
          :effects=>%{
            :damage=>0.05,
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
          :user_message=>"You smack {{target}} HARD in the lower torso.",
          :target_message=>"{{user}} smacks you HARD in the lower torso.",
          :spectator_message=>"{{user}} smacks {{target}} HARD in the lower torso.",
          :effects=>%{
            :damage=>0.06,
            :skill_mod=>[
              %{
                :skill=>"attack",
                :amount=>-20,
                :duration=>4.0
              },
              %{
                :skill=>"dodge",
                :amount=>-20,
                :duration=>4.0
              },
              %{
                :skill=>"block",
                :amount=>-20,
                :duration=>4.0
              },
              %{
                :skill=>"parry",
                :amount=>-20,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"You deliver a strong blow along {{target}}'s back.",
          :target_message=>"{{user}} delivers a strong blow along your back.",
          :spectator_message=>"{{user}} delivers a strong blow along {{target}}'s back.",
          :effects=>%{
            :damage=>0.06,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-25,
                :duration=>2.0
              }
            ],
            :stun=>1.0
          }
        },
        %{
          :user_message=>"You strike the frightened {{target}} hard in {{target:his/her/its}} chest.",
          :target_message=>"{{user}} strikes the frightened you hard in your chest.",
          :spectator_message=>"{{user}} strikes the frightened {{target}} hard in {{target:his/her/its}} chest.",
          :effects=>%{
            :damage=>0.06,
            :stun=>2.0
          }
        },
        %{
          :user_message=>"You bruise {{target}}'s hip with a nasty hit.",
          :target_message=>"{{user}} bruises your hip with a nasty hit.",
          :spectator_message=>"{{user}} bruises {{target}}'s hip with a nasty hit.",
          :effects=>%{
            :damage=>0.06,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"attack",
                :amount=>-5,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"You swing down forcefully, hitting {{target}}'s forearm and upper body.",
          :target_message=>"{{user}} swings down forcefully, hitting your forearm and upper body.",
          :spectator_message=>"{{user}} swings down forcefully, hitting {{target}}'s forearm and upper body.",
          :effects=>%{
            :damage=>0.09,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"attack",
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
          :user_message=>"Your wicked swing shatter {{target}}'s elbow!!",
          :target_message=>"{{user}}'s wicked swing shatters your elbow!!",
          :spectator_message=>"{{user}}'s wicked swing shatters {{target}}'s elbow!!",
          :effects=>%{
            :damage=>0.06,
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
                :amount=>-60,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"You pound down on {{target}}'s shoulder, shocking {{target:him/her/it}} with agony!",
          :target_message=>"{{user}} pounds down on your shoulder, shocking you with agony!",
          :spectator_message=>"{{user}} pounds down on {{target}}'s shoulder, shocking {{target:him/her/it}} with agony!",
          :effects=>%{
            :damage=>0.1,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-75,
                :duration=>4.0
              },
              %{
                :skill=>"attack",
                :amount=>-10,
                :duration=>16.0
              }
            ]
          }
        },
        %{
          :user_message=>"You shake {{target}} up with a very strong blow to {{target:his/her/its}} leg!",
          :target_message=>"{{user}} shakes you up with a very strong blow to your leg!",
          :spectator_message=>"{{user}} shakes {{target}} up with a very strong blow to {{target:his/her/its}} leg!",
          :effects=>%{
            :damage=>0.1,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-60,
                :duration=>2.0
              },
              %{
                :skill=>"attack",
                :amount=>-35,
                :duration=>8.0
              },
              %{
                :skill=>"dodge",
                :amount=>-35,
                :duration=>16.0
              },
              %{
                :skill=>"block",
                :amount=>-35,
                :duration=>12.0
              }
            ]
          }
        },
        %{
          :user_message=>"You crush {{target}}'s wrist into splinters with a furious swing!",
          :target_message=>"{{user}} crushes your wrist into splinters with a furious swing!",
          :spectator_message=>"{{user}} crushes {{target}}'s wrist into splinters with a furious swing!",
          :effects=>%{
            :damage=>0.06,
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"hand"
              }
            ],
            :stun=>1.0
          }
        },
        %{
          :user_message=>"You smash into {{target}}'s ribs with a staggering blow!",
          :target_message=>"{{user}} smashes into your ribs with a staggering blow!",
          :spectator_message=>"{{user}} smashes into {{target}}'s ribs with a staggering blow!",
          :effects=>%{
            :damage=>0.12,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-200,
                :duration=>4.0
              },
              %{
                :skill=>"attack",
                :amount=>-35,
                :duration=>14.0
              },
              %{
                :skill=>"block",
                :amount=>-35,
                :duration=>14.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your shocking blow knocks {{target}} down, smashing nerves and tendons!",
          :target_message=>"{{user}}'s shocking blow knocks you down, smashing nerves and tendons!",
          :spectator_message=>"{{user}}'s shocking blow knocks {{target}} down, smashing nerves and tendons!",
          :effects=>%{
            :damage=>0.16,
            :stun=>4.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-30,
                :duration=>24.0
              },
              %{
                :skill=>"attack",
                :amount=>-30,
                :duration=>24.0
              },
              %{
                :skill=>"dodge",
                :amount=>-30,
                :duration=>16.0
              }
            ]
          }
        },
        %{
          :user_message=>"You whack {{target}} wickedly on the head, spraying blood and tiny skull splinters!",
          :target_message=>"{{user}} whacks you wickedly on the head, spraying blood and tiny skull splinters!",
          :spectator_message=>"{{user}} whacks {{target}} wickedly on the head, spraying blood and tiny skull splinters!",
          :effects=>%{
            :damage=>0.2,
            :stun=>12.0,
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>12.0
            }
          }
        },
        %{
          :user_message=>"You leap to crush {{target}}'s ribs nastily, blasting {{target:him/her/it}} to the ground!!",
          :target_message=>"{{user}} leaps to crush your ribs nastily, blasting you to the ground!!",
          :spectator_message=>"{{user}} leaps to crush {{target}}'s ribs nastily, blasting {{target:him/her/it}} to the ground!!",
          :effects=>%{
            :damage=>0.6,
            :stun=>8.0,
            :damage_over_time=>%{
              :damage=>0.07,
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
          :user_message=>"You strike {{target}} adeptly in the back of the waist, paralyzing {{target:him/her/it}} from the shoulders down....",
          :target_message=>"{{user}} strikes you adeptly in the back of the waist, paralyzing you from the shoulders down....",
          :spectator_message=>"{{user}} strikes {{target}} adeptly in the back of the waist, paralyzing {{target:him/her/it}} from the shoulders down....",
          :effects=>%{
            :damage=>0.25,
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
        }
      ],
      "C"=>[
        %{
          :user_message=>"You whap {{target}}.",
          :target_message=>"{{user}} whaps you.",
          :spectator_message=>"{{user}} whaps {{target}}.",
          :effects=>%{
            :damage=>0.01
          }
        },
        %{
          :user_message=>"You nastily smack {{target}}.",
          :target_message=>"{{user}} nastily smacks you.",
          :spectator_message=>"{{user}} nastily smacks {{target}}.",
          :effects=>%{
            :damage=>0.03
          }
        },
        %{
          :user_message=>"You strike {{target}} very hard in {{target:his/her/its}} side.",
          :target_message=>"{{user}} strikes you very hard in your side.",
          :spectator_message=>"{{user}} strikes {{target}} very hard in {{target:his/her/its}} side.",
          :effects=>%{
            :damage=>0.07,
            :skill_mod=>[
              %{
                :skill=>"attack",
                :amount=>-10,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"You pound {{target}} in the side.",
          :target_message=>"{{user}} pounds you in the side.",
          :spectator_message=>"{{user}} pounds {{target}} in the side.",
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
          :user_message=>"You crack {{target}}'s rib with a strong blow to {{target:his/her/its}} side.",
          :target_message=>"{{user}} cracks your rib with a strong blow to your side.",
          :spectator_message=>"{{user}} cracks {{target}}'s rib with a strong blow to {{target:his/her/its}} side.",
          :effects=>%{
            :damage=>0.05,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"You wound {{target}}'s hip with a strong strike.",
          :target_message=>"{{user}} wounds your hip with a strong strike.",
          :spectator_message=>"{{user}} wounds {{target}}'s hip with a strong strike.",
          :effects=>%{
            :damage=>0.09,
            :skill_mod=>[
              %{
                :skill=>"attack",
                :amount=>-25,
                :duration=>4.0
              },
              %{
                :skill=>"parry",
                :amount=>-25,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"You strike a nasty blow to {{target}}'s lower back, stunning him.",
          :target_message=>"{{user}} strikes a nasty blow to your lower back, stunning him.",
          :spectator_message=>"{{user}} strikes a nasty blow to {{target}}'s lower back, stunning him.",
          :effects=>%{
            :damage=>0.05,
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
              },
              %{
                :skill=>"attack",
                :amount=>-10,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"You pound {{target}} very hard in the chest, knocking {{target:him/her/it}} backward.",
          :target_message=>"{{user}} pounds you very hard in the chest, knocking you backward.",
          :spectator_message=>"{{user}} pounds {{target}} very hard in the chest, knocking {{target:him/her/it}} backward.",
          :effects=>%{
            :damage=>0.08,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"attack",
                :amount=>-10,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"You powerfully hammer {{target}} in the abdomen.",
          :target_message=>"{{user}} powerfully hammers you in the abdomen.",
          :spectator_message=>"{{user}} powerfully hammers {{target}} in the abdomen.",
          :effects=>%{
            :damage=>0.06,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-5,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You sharply strike {{target}}'s forearm, making {{target:him/her/it}} wince in pain.",
          :target_message=>"{{user}} sharply strikes your forearm, making you wince in pain.",
          :spectator_message=>"{{user}} sharply strikes {{target}}'s forearm, making {{target:him/her/it}} wince in pain.",
          :effects=>%{
            :damage=>0.05,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"Your wicked blow shatters {{target}}'s knee into tiny fragments!!!",
          :target_message=>"{{user}}'s wicked blow shatters your knee into tiny fragments!!!",
          :spectator_message=>"{{user}}'s wicked blow shatters {{target}}'s knee into tiny fragments!!!",
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
                :amount=>-75,
                :duration=>16.0
              },
              %{
                :skill=>"dodge",
                :amount=>-75,
                :duration=>16.0
              },
              %{
                :skill=>"attack",
                :amount=>-75,
                :duration=>16.0
              }
            ]
          }
        },
        %{
          :user_message=>"You deliver a staggering blow to {{target}}!",
          :target_message=>"{{user}} delivers a staggering blow to you!",
          :spectator_message=>"{{user}} delivers a staggering blow to {{target}}!",
          :effects=>%{
            :damage=>0.1,
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>4.0
              },
              %{
                :skill=>"attack",
                :amount=>-10,
                :duration=>12.0
              }
            ]
          }
        },
        %{
          :user_message=>"You painfully pound {{target}}'s knee, bringing {{target:him/her/it}} to the floor!",
          :target_message=>"{{user}} painfully pounds your knee, bringing you to the floor!",
          :spectator_message=>"{{user}} painfully pounds {{target}}'s knee, bringing {{target:him/her/it}} to the floor!",
          :effects=>%{
            :damage=>0.09,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-40,
                :duration=>8.0
              },
              %{
                :skill=>"dodge",
                :amount=>-40,
                :duration=>12.0
              }
            ]
          }
        },
        %{
          :user_message=>"You badly bruise {{target}}'s arm with a forceful strike.",
          :target_message=>"{{user}} badly bruises your arm with a forceful strike.",
          :spectator_message=>"{{user}} badly bruises {{target}}'s arm with a forceful strike.",
          :effects=>%{
            :damage=>0.09,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
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
          :user_message=>"You strike {{target}} extremely hard in the side, breaking a few ribs!",
          :target_message=>"{{user}} strikes you extremely hard in the side, breaking a few ribs!",
          :spectator_message=>"{{user}} strikes {{target}} extremely hard in the side, breaking a few ribs!",
          :effects=>%{
            :damage=>0.13,
            :stun=>3.0,
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
          :user_message=>"Your blow to {{target}}'s back causes a few fractures and knocks {{target}} down flat!",
          :target_message=>"{{user}}'s blow to your back causes a few fractures and knocks you down flat!",
          :spectator_message=>"{{user}}'s blow to {{target}}'s back causes a few fractures and knocks {{target}} down flat!",
          :effects=>%{
            :damage=>0.2,
            :stun=>6.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>12.0
              },
              %{
                :skill=>"attack",
                :amount=>-50,
                :duration=>18.0
              },
              %{
                :skill=>"dodge",
                :amount=>-50,
                :duration=>24.0
              },
              %{
                :skill=>"block",
                :amount=>-50,
                :duration=>18.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your amazing blow breaks {{target}}'s pelvis, giving {{target:him/her/it}} horrible compound fractures through {{target:his/her/its}} arteries!!",
          :target_message=>"{{user}}'s amazing blow breaks your pelvis, giving you horrible compound fractures through your arteries!!",
          :spectator_message=>"{{user}}'s amazing blow breaks {{target}}'s pelvis, giving {{target:him/her/it}} horrible compound fractures through {{target:his/her/its}} arteries!!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"leg"
              }
            ],
            :damage_over_time=>%{
              :damage=>0.09,
              :duration=>12.0
            },
            :stun=>12.0
          }
        },
        %{
          :user_message=>"You smash {{target}}'s forehead, dislocating the front plate of {{target:his/her/its}} skull!!",
          :target_message=>"{{user}} smashes your forehead, dislocating the front plate of your skull!!",
          :spectator_message=>"{{user}} smashes {{target}}'s forehead, dislocating the front plate of {{target:his/her/its}} skull!!",
          :effects=>%{
            :damage=>0.4,
            :stun=>24.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>48.0
              },
              %{
                :skill=>"block",
                :amount=>-50,
                :duration=>48.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>24.0
            }
          }
        },
        %{
          :user_message=>"You deliver a CRUSHING blow to {{target}}'s abdomen that ruptures a variety of important organs! {{target}} spews blood before dying.",
          :target_message=>"{{user}} delivers a CRUSHING blow to your abdomen that ruptures a variety of important organs! you spews blood before dying.",
          :spectator_message=>"{{user}} delivers a CRUSHING blow to {{target}}'s abdomen that ruptures a variety of important organs! {{target}} spews blood before dying.",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "D"=>[
        %{
          :user_message=>"You strike {{target}} hard.",
          :target_message=>"{{user}} strikes you hard.",
          :spectator_message=>"{{user}} strikes {{target}} hard.",
          :effects=>%{
            :damage=>0.02
          }
        },
        %{
          :user_message=>"You wickedly smack {{target}}.",
          :target_message=>"{{user}} wickedly smacks you.",
          :spectator_message=>"{{user}} wickedly smacks {{target}}.",
          :effects=>%{
            :damage=>0.04
          }
        },
        %{
          :user_message=>"{{target}} is hurt by your strong, fast blow.",
          :target_message=>"You are hurt by {{user}}'s strong, fast blow.",
          :spectator_message=>"{{target}} is hurt by {{user}}'s strong, fast blow.",
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
          :user_message=>"Your nasty blow causes minor fractures of {{target}}'s ribs.",
          :target_message=>"{{user}}'s nasty blow causes minor fractures of your ribs.",
          :spectator_message=>"{{user}}'s nasty blow causes minor fractures of {{target}}'s ribs.",
          :effects=>%{
            :damage=>0.05,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-5,
                :duration=>6.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-10,
                :duration=>20.0
              },
              %{
                :stat=>"agility",
                :amount=>-10,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"You strike a powerful blow to {{target}}'s side. OUCH!",
          :target_message=>"{{user}} strikes a powerful blow to your side. OUCH!",
          :spectator_message=>"{{user}} strikes a powerful blow to {{target}}'s side. OUCH!",
          :effects=>%{
            :damage=>0.04,
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
          :user_message=>"You ruthlessly smack {{target}}'s lower body!",
          :target_message=>"{{user}} ruthlessly smacks your lower body!",
          :spectator_message=>"{{user}} ruthlessly smacks {{target}}'s lower body!",
          :effects=>%{
            :damage=>0.1,
            :skill_mod=>[
              %{
                :skill=>"attack",
                :amount=>-30,
                :duration=>8.0
              },
              %{
                :skill=>"parry",
                :amount=>-10,
                :duration=>8.0
              },
              %{
                :skill=>"block",
                :amount=>-10,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"You strike {{target}} with a crushing blow to {{target:his/her/its}} back!",
          :target_message=>"{{user}} strikes you with a crushing blow to your back!",
          :spectator_message=>"{{user}} strikes {{target}} with a crushing blow to {{target:his/her/its}} back!",
          :effects=>%{
            :damage=>0.1,
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
          :user_message=>"You break a couple of {{target}}'s ribs with a huge blow to the chest!",
          :target_message=>"{{user}} breaks a couple of your ribs with a huge blow to the chest!",
          :spectator_message=>"{{user}} breaks a couple of {{target}}'s ribs with a huge blow to the chest!",
          :effects=>%{
            :damage=>0.1,
            :skill_mod=>[
              %{
                :skill=>"attack",
                :amount=>-15,
                :duration=>12.0
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
          :user_message=>"You whack {{target}} very hard in the thigh.",
          :target_message=>"{{user}} whacks you very hard in the thigh.",
          :spectator_message=>"{{user}} whacks {{target}} very hard in the thigh.",
          :effects=>%{
            :damage=>0.06,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-10,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"You knock {{target}} backward with a MASSIVE blow to {{target:his/her/its}} arm!",
          :target_message=>"{{user}} knocks you backward with a MASSIVE blow to your arm!",
          :spectator_message=>"{{user}} knocks {{target}} backward with a MASSIVE blow to {{target:his/her/its}} arm!",
          :effects=>%{
            :damage=>0.1,
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
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"You ruthlessly pound {{target}} in the head, spinning {{target:him/her/it}} across the ground with a badly fractured skull!!",
          :target_message=>"{{user}} ruthlessly pounds you in the head, spinning you across the ground with a badly fractured skull!!",
          :spectator_message=>"{{user}} ruthlessly pounds {{target}} in the head, spinning {{target:him/her/it}} across the ground with a badly fractured skull!!",
          :effects=>%{
            :damage=>0.4,
            :stun=>10.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-40,
                :duration=>20.0
              },
              %{
                :skill=>"block",
                :amount=>-40,
                :duration=>20.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>10.0
            }
          }
        },
        %{
          :user_message=>"You strike {{target}}'s shoulder area, dislocating and fracturing bones there.",
          :target_message=>"{{user}} strikes your shoulder area, dislocating and fracturing bones there.",
          :spectator_message=>"{{user}} strikes {{target}}'s shoulder area, dislocating and fracturing bones there.",
          :effects=>%{
            :damage=>0.07,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-20,
                :duration=>10.0
              },
              %{
                :skill=>"attack",
                :amount=>-30,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"You smash {{target}}'s leg, breaking a bone and shattering cartilage!",
          :target_message=>"{{user}} smashes your leg, breaking a bone and shattering cartilage!",
          :spectator_message=>"{{user}} smashes {{target}}'s leg, breaking a bone and shattering cartilage!",
          :effects=>%{
            :damage=>0.12,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>12.0
              },
              %{
                :skill=>"attack",
                :amount=>-50,
                :duration=>12.0
              },
              %{
                :skill=>"dodge",
                :amount=>-50,
                :duration=>12.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your crushing blow breaks {{target}}'s arm and smashes vital tendons!",
          :target_message=>"{{user}}'s crushing blow breaks your arm and smashes vital tendons!",
          :spectator_message=>"{{user}}'s crushing blow breaks {{target}}'s arm and smashes vital tendons!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
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
          :user_message=>"You smash {{target}} extremely hard in the side, sending {{target:him/her/it}} sprawling!",
          :target_message=>"{{user}} smashes you extremely hard in the side, sending you sprawling!",
          :spectator_message=>"{{user}} smashes {{target}} extremely hard in the side, sending {{target:him/her/it}} sprawling!",
          :effects=>%{
            :damage=>0.15,
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-10,
                :duration=>6.0
              },
              %{
                :skill=>"block",
                :amount=>-10,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You strike {{target}}'s neck area with awesome force, cracking {{target:his/her/its}} spine!!",
          :target_message=>"{{user}} strikes your neck area with awesome force, cracking your spine!!",
          :spectator_message=>"{{user}} strikes {{target}}'s neck area with awesome force, cracking {{target:his/her/its}} spine!!",
          :effects=>%{
            :damage=>0.25,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>4.0
              },
              %{
                :skill=>"block",
                :amount=>-200,
                :duration=>4.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.25,
              :duration=>2.0
            }
          }
        },
        %{
          :user_message=>"You strike a blow that crushes {{target}}'s arm, sending splintered bones through veins and arteries! MOST horrible!",
          :target_message=>"{{user}} strikes a blow that crushes your arm, sending splintered bones through veins and arteries! MOST horrible!",
          :spectator_message=>"{{user}} strikes a blow that crushes {{target}}'s arm, sending splintered bones through veins and arteries! MOST horrible!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :stun=>9.0,
            :skill_mod=>[
              %{
                :skill=>"block",
                :amount=>-100,
                :duration=>18.0
              },
              %{
                :skill=>"parry",
                :amount=>-20,
                :duration=>18.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>9.0
            }
          }
        },
        %{
          :user_message=>"Your amazing blow CRUSHES {{target}}'s chest cavity, knocking {{target:him/her/it}} limp to the ground!!!",
          :target_message=>"{{user}}'s amazing blow CRUSHES your chest cavity, knocking you limp to the ground!!!",
          :spectator_message=>"{{user}}'s amazing blow CRUSHES {{target}}'s chest cavity, knocking {{target:him/her/it}} limp to the ground!!!",
          :effects=>%{
            :damage=>0.8,
            :stun=>3.0,
            :damage_over_time=>%{
              :damage=>0.3,
              :duration=>3.0
            },
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>6.0
              },
              %{
                :skill=>"block",
                :amount=>-50,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You blast {{target}}'s rib cage into {{target:his/her/its}} own heart!! Quite messy!",
          :target_message=>"{{user}} blasts your rib cage into your own heart!! Quite messy!",
          :spectator_message=>"{{user}} blasts {{target}}'s rib cage into {{target:his/her/its}} own heart!! Quite messy!",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "E"=>[
        %{
          :user_message=>"You nastily smack {{target}}.",
          :target_message=>"{{user}} nastily smacks you.",
          :spectator_message=>"{{user}} nastily smacks {{target}}.",
          :effects=>%{
            :damage=>0.03
          }
        },
        %{
          :user_message=>"You strike a very powerful but clumsy blow to {{target}}.",
          :target_message=>"{{user}} strikes a very powerful but clumsy blow to you.",
          :spectator_message=>"{{user}} strikes a very powerful but clumsy blow to {{target}}.",
          :effects=>%{
            :damage=>0.06,
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
          :user_message=>"You pound {{target}} good and hard, shaking {{target:him/her/it}} up.",
          :target_message=>"{{user}} pounds you good and hard, shaking you up.",
          :spectator_message=>"{{user}} pounds {{target}} good and hard, shaking {{target:him/her/it}} up.",
          :effects=>%{
            :damage=>0.06,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-5,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"You deliver a terribly strong blow to {{target}}, taking him by surprise!",
          :target_message=>"{{user}} delivers a terribly strong blow to you, taking him by surprise!",
          :spectator_message=>"{{user}} delivers a terribly strong blow to {{target}}, taking him by surprise!",
          :effects=>%{
            :damage=>0.1,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"You crack some of {{target}}'s ribs!",
          :target_message=>"{{user}} cracks some of your ribs!",
          :spectator_message=>"{{user}} cracks some of {{target}}'s ribs!",
          :effects=>%{
            :damage=>0.1,
            :skill_mod=>[
              %{
                :skill=>"attack",
                :amount=>-10,
                :duration=>12.0
              },
              %{
                :skill=>"parry",
                :amount=>-10,
                :duration=>12.0
              },
              %{
                :skill=>"block",
                :amount=>-10,
                :duration=>12.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your forceful blow to {{target}}'s upper leg causes a nasty fracture!",
          :target_message=>"{{user}}'s forceful blow to your upper leg causes a nasty fracture!",
          :spectator_message=>"{{user}}'s forceful blow to {{target}}'s upper leg causes a nasty fracture!",
          :effects=>%{
            :damage=>0.12,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-30,
                :duration=>12.0
              },
              %{
                :skill=>"attack",
                :amount=>-30,
                :duration=>12.0
              }
            ],
            :stun=>1.0
          }
        },
        %{
          :user_message=>"You slam {{target}} VERY hard in the lower back, knocking {{target:him/her/it}} to {{target:his/her/its}} knees!",
          :target_message=>"{{user}} slams you VERY hard in the lower back, knocking you to your knees!",
          :spectator_message=>"{{user}} slams {{target}} VERY hard in the lower back, knocking {{target:him/her/it}} to {{target:his/her/its}} knees!",
          :effects=>%{
            :damage=>0.15,
            :stun=>2.0,
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
          :user_message=>"You strike a very strong blow to {{target}}'s chest that knocks the wind and some blood out of {{target:him/her/it}}!",
          :target_message=>"{{user}} strikes a very strong blow to your chest that knocks the wind and some blood out of you!",
          :spectator_message=>"{{user}} strikes a very strong blow to {{target}}'s chest that knocks the wind and some blood out of {{target:him/her/it}}!",
          :effects=>%{
            :damage=>0.16,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-10,
                :duration=>8.0
              },
              %{
                :skill=>"attack",
                :amount=>-20,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"You smack {{target}} really hard in {{target:his/her/its}} leg, making {{target:him/her/it}} hold {{target:his/her/its}} knee in pure agony!",
          :target_message=>"{{user}} smacks you really hard in your leg, making you hold your knee in pure agony!",
          :spectator_message=>"{{user}} smacks {{target}} really hard in {{target:his/her/its}} leg, making {{target:him/her/it}} hold {{target:his/her/its}} knee in pure agony!",
          :effects=>%{
            :damage=>0.1,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-10,
                :duration=>14.0
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
          :user_message=>"You strike a sharp, strong blow to {{target}}'s forearm that makes a crunching noise!",
          :target_message=>"{{user}} strikes a sharp, strong blow to your forearm that makes a crunching noise!",
          :spectator_message=>"{{user}} strikes a sharp, strong blow to {{target}}'s forearm that makes a crunching noise!",
          :effects=>%{
            :damage=>0.1,
            :stun=>1.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-15,
                :duration=>16.0
              },
              %{
                :skill=>"attack",
                :amount=>-15,
                :duration=>16.0
              }
            ]
          }
        },
        %{
          :user_message=>"You hammer {{target}} so hard in the back that {{target:his/her/its}} spine is split between two middle vertebrae! {{target}}'s contorted corpse falls in shock to the ground!",
          :target_message=>"{{user}} hammers you so hard in the back that your spine is split between two middle vertebrae! your contorted corpse falls in shock to the ground!",
          :spectator_message=>"{{user}} hammers {{target}} so hard in the back that {{target:his/her/its}} spine is split between two middle vertebrae! {{target}}'s contorted corpse falls in shock to the ground!",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"You shatter {{target}}'s shoulder, crushing bones and muscles like peanut shells!",
          :target_message=>"{{user}} shatters your shoulder, crushing bones and muscles like peanut shells!",
          :spectator_message=>"{{user}} shatters {{target}}'s shoulder, crushing bones and muscles like peanut shells!",
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
          :user_message=>"You break {{target}}'s hip with a strong blow that slams {{target:him/her/it}} to the floor!",
          :target_message=>"{{user}} breaks your hip with a strong blow that slams you to the floor!",
          :spectator_message=>"{{user}} breaks {{target}}'s hip with a strong blow that slams {{target:him/her/it}} to the floor!",
          :effects=>%{
            :damage=>0.15,
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-75,
                :duration=>8.0
              },
              %{
                :skill=>"attack",
                :amount=>-75,
                :duration=>16.0
              },
              %{
                :skill=>"dodge",
                :amount=>-75,
                :duration=>24.0
              }
            ]
          }
        },
        %{
          :user_message=>"You crush {{target}}'s elbow, sending bone fragments flying, blood pouring, and {{target}} reeling!!",
          :target_message=>"{{user}} crushes your elbow, sending bone fragments flying, blood pouring, and you reeling!!",
          :spectator_message=>"{{user}} crushes {{target}}'s elbow, sending bone fragments flying, blood pouring, and {{target}} reeling!!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-200,
                :duration=>4.0
              },
              %{
                :skill=>"block",
                :amount=>-200,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your staggering blow to {{target}}'s side compresses {{target:his/her/its}} lungs and knocks him down!!",
          :target_message=>"{{user}}'s staggering blow to your side compresses your lungs and knocks him down!!",
          :spectator_message=>"{{user}}'s staggering blow to {{target}}'s side compresses {{target:his/her/its}} lungs and knocks him down!!",
          :effects=>%{
            :damage=>0.3,
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"attack",
                :amount=>-50,
                :duration=>18.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.09,
              :duration=>3.0
            }
          }
        },
        %{
          :user_message=>"You devastate {{target}} in the neck, crushing {{target:his/her/its}} windpipe and breaking veins open!!!",
          :target_message=>"{{user}} devastates you in the neck, crushing your windpipe and breaking veins open!!!",
          :spectator_message=>"{{user}} devastates {{target}} in the neck, crushing {{target:his/her/its}} windpipe and breaking veins open!!!",
          :effects=>%{
            :damage=>0.25,
            :stun=>12.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-25,
                :duration=>24.0
              },
              %{
                :skill=>"block",
                :amount=>-25,
                :duration=>24.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.11,
              :duration=>12.0
            }
          }
        },
        %{
          :user_message=>"Your devastating blow breaks {{target}}'s ribs and drives lethal bones into {{target:his/her/its}} vitals!!!",
          :target_message=>"{{user}}'s devastating blow breaks your ribs and drives lethal bones into your vitals!!!",
          :spectator_message=>"{{user}}'s devastating blow breaks {{target}}'s ribs and drives lethal bones into {{target:his/her/its}} vitals!!!",
          :effects=>%{
            :damage=>0.75,
            :stun=>6.0,
            :damage_over_time=>%{
              :damage=>0.25,
              :duration=>6.0
            }
          }
        },
        %{
          :user_message=>"You destroy {{target}}'s heart and lungs with a crushing blow to the side of the chest!!! {{target}}'s body is flung to the side by the force, with blood pouring from {{target:his/her/its}} mouth!",
          :target_message=>"{{user}} destroys your heart and lungs with a crushing blow to the side of the chest!!! your body is flung to the side by the force, with blood pouring from your mouth!",
          :spectator_message=>"{{user}} destroys {{target}}'s heart and lungs with a crushing blow to the side of the chest!!! {{target}}'s body is flung to the side by the force, with blood pouring from {{target:his/her/its}} mouth!",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"You knock {{target}}'s head clean off {{target:his/her/its}} shoulders, sending it flying and rolling away from the bloody mess!!!!",
          :target_message=>"{{user}} knocks your head clean off your shoulders, sending it flying and rolling away from the bloody mess!!!!",
          :spectator_message=>"{{user}} knocks {{target}}'s head clean off {{target:his/her/its}} shoulders, sending it flying and rolling away from the bloody mess!!!!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"head"
              }
            ],
            :kill=>true
          }
        }
      ]
    }
  end

end
