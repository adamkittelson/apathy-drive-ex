defmodule ApathyDrive.CritTables.Strike do

  def name do
    "strike"
  end

  def damage_type do
    "physical"
  end

  def crits do
    %{
      "A"=>[
        %{
          :user_message=>"You fall on your face while trying to attack.",
          :target_message=>"{{user}} falls on {{user:his/her/its}} face while trying to attack.",
          :spectator_message=>"{{user}} falls on {{user:his/her/its}} face while trying to attack.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You back flip, landing a foot on {{target}}'s jaw.",
          :target_message=>"{{user}} back flips, landing a foot on your jaw.",
          :spectator_message=>"{{user}} back flips, landing a foot on {{target}}'s jaw.",
          :effects=>%{
            :damage=>0.07,
            :stun=>2.0
          }
        },
        %{
          :user_message=>"You get in a quick jab to {{target}}'s side.",
          :target_message=>"{{user}} gets in a quick jab to your side.",
          :spectator_message=>"{{user}} gets in a quick jab to {{target}}'s side.",
          :effects=>%{
            :damage=>0.03,
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
          :user_message=>"You spin {{target}} around and around and around by {{target:his/her/its}} feet.",
          :target_message=>"{{user}} spins you around and around and around by your feet.",
          :spectator_message=>"{{user}} spins {{target}} around and around and around by {{target:his/her/its}} feet.",
          :effects=>%{
            :stun=>3.0,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-10,
                :duration=>4.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-15,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"You spin {{target}} around and around and around by {{target:his/her/its}} feet, before suddenly releasing {{target:him/her/it}}!",
          :target_message=>"{{user}} spins you around and around and around by your feet, before suddenly releasing you!",
          :spectator_message=>"{{user}} spins {{target}} around and around and around by {{target:his/her/its}} feet, before suddenly releasing {{target:him/her/it}}!",
          :effects=>%{
            :damage=>0.09,
            :stun=>3.0,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-20,
                :duration=>8.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-15,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"You leap over {{target}}, stomping on {{target:his/her/its}} head.",
          :target_message=>"{{user}} leaps over you, stomping on your head.",
          :spectator_message=>"{{user}} leaps over {{target}}, stomping on {{target:his/her/its}} head.",
          :effects=>%{
            :damage=>0.04,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-5,
                :duration=>15.0
              },
              %{
                :stat=>"willpower",
                :amount=>-5,
                :duration=>15.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} is stupified by your show of martial arts skill.",
          :target_message=>"You are stupified by {{user}}'s show of martial arts skill.",
          :spectator_message=>"{{target}} is stupified by {{user}}'s show of martial arts skill.",
          :effects=>%{
            :stun=>4.0,
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
          :user_message=>"{{target}} winces in pain from your attack to {{target:his/her/its}} chest!",
          :target_message=>"You wince in pain from {{user}}'s attack to your chest!",
          :spectator_message=>"{{target}} winces in pain from {{user}}'s attack to {{target:his/her/its}} chest!",
          :effects=>%{
            :damage=>0.15
          }
        },
        %{
          :user_message=>"You tear open a hole in {{target}}'s leg with your finger!",
          :target_message=>"{{user}} tears open a hole in your leg with {{user:his/her/its}} finger!",
          :spectator_message=>"{{user}} tears open a hole in {{target}}'s leg with {{user:his/her/its}} finger!",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.06,
              :duration=>6.0
            },
            :damage=>0.08
          }
        },
        %{
          :user_message=>"Using an ancient martial arts skill,  you pretend your hands are scissors.  These scissors lop off one of {{target}}'s limbs!",
          :target_message=>"Using an ancient martial arts skill,  {{user}} pretends {{user:his/her/its}} hands are scissors.  These scissors lop off one of your limbs!",
          :spectator_message=>"Using an ancient martial arts skill,  {{user}} pretends {{user:his/her/its}} hands are scissors.  These scissors lop off one of {{target}}'s limbs!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-5,
                :duration=>20.0
              }
            ],
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-16,
                :duration=>18.0
              }
            ]
          }
        },
        %{
          :user_message=>"You punches {{target}}'s stomach, forcing {{target:his/her/its}} stomach out of {{target:his/her/its}} mouth!",
          :target_message=>"{{user}} punches your stomach, forcing your stomach out of your mouth!",
          :spectator_message=>"{{user}} punches {{target}}'s stomach, forcing {{target:his/her/its}} stomach out of {{target:his/her/its}} mouth!",
          :effects=>%{
            :damage=>1.0
          }
        }
      ],
      "B"=>[
        %{
          :user_message=>"Trying to do a cool move, you accidently whack youself in the head.",
          :target_message=>"Trying to do a cool move, {{user}} accidently whacks {{user}}self in the head.",
          :spectator_message=>"Trying to do a cool move, {{user}} accidently whacks {{user}}self in the head.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You poke {{target}} in the ribes!",
          :target_message=>"{{user}} pokes you in the ribes!",
          :spectator_message=>"{{user}} pokes {{target}} in the ribes!",
          :effects=>%{
            :damage=>0.01
          }
        },
        %{
          :user_message=>"You poke {{target}} in the ribs with a bone crushing sound!",
          :target_message=>"{{user}} pokes you in the ribs with a bone crushing sound!",
          :spectator_message=>"{{user}} pokes {{target}} in the ribs with a bone crushing sound!",
          :effects=>%{
            :damage=>0.21,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-5,
                :duration=>16.0
              },
              %{
                :stat=>"agility",
                :amount=>-5,
                :duration=>16.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} gets lots of dust kicked up in {{target:his/her/its}} face by you.",
          :target_message=>"You get lots of dust kicked up in your face by {{user}}.",
          :spectator_message=>"{{target}} gets lots of dust kicked up in {{target:his/her/its}} face by {{user}}.",
          :effects=>%{
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-26,
                :duration=>8.0
              },
              %{
                :skill=>"block",
                :amount=>-26,
                :duration=>8.0
              },
              %{
                :skill=>"parry",
                :amount=>-26,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"You get in a good punch on {{target}}'s cheek.",
          :target_message=>"{{user}} gets in a good punch on your cheek.",
          :spectator_message=>"{{user}} gets in a good punch on {{target}}'s cheek.",
          :effects=>%{
            :damage=>0.05,
            :stun=>1.0,
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-6,
                :duration=>40.0
              }
            ]
          }
        },
        %{
          :user_message=>"Using both fists, you get a good one on BOTH of {{target}}'s cheeks!",
          :target_message=>"Using both fists, {{user}} gets a good one on BOTH of your cheeks!",
          :spectator_message=>"Using both fists, {{user}} gets a good one on BOTH of {{target}}'s cheeks!",
          :effects=>%{
            :damage=>0.1,
            :stun=>2.0,
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-5,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"You bear hug {{target}}.",
          :target_message=>"{{user}} bear hugs you.",
          :spectator_message=>"{{user}} bear hugs {{target}}.",
          :effects=>%{
            :stun=>5.0,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"You clap, and {{target}}'s head gets caught in the onslaught!",
          :target_message=>"{{user}} claps, and your head gets caught in the onslaught!",
          :spectator_message=>"{{user}} claps, and {{target}}'s head gets caught in the onslaught!",
          :effects=>%{
            :damage=>0.11,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-6,
                :duration=>8.0
              },
              %{
                :skill=>"dodge",
                :amount=>-9,
                :duration=>10.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-15,
                :duration=>22.0
              }
            ]
          }
        },
        %{
          :user_message=>"You will {{target}} into punching your rock hard chest, which breaks {{target}}'s hand!",
          :target_message=>"{{user}} wills you into punching {{user:his/her/its}} rock hard chest, which breaks your hand!",
          :spectator_message=>"{{user}} wills {{target}} into punching {{user:his/her/its}} rock hard chest, which breaks {{target}}'s hand!",
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
          :user_message=>"You do a high kick to {{target}}'s throat.  Ouch!",
          :target_message=>"{{user}} dos a high kick to your throat.  Ouch!",
          :spectator_message=>"{{user}} dos a high kick to {{target}}'s throat.  Ouch!",
          :effects=>%{
            :damage=>0.15,
            :damage_over_time=>%{
              :damage=>0.06,
              :duration=>5.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-16,
                :duration=>10.0
              },
              %{
                :skill=>"dodge",
                :amount=>-32,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You knee {{target}} hard in the groin.",
          :target_message=>"{{user}} knees you hard in the groin.",
          :spectator_message=>"{{user}} knees {{target}} hard in the groin.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.01,
              :duration=>5.0
            },
            :damage=>0.16,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-10,
                :duration=>8.0
              },
              %{
                :stat=>"agility",
                :amount=>-10,
                :duration=>8.0
              }
            ],
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-45,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"You grab {{target}} by the arm, tossing {{target:him/her/it}} over your shoulder.  After closer examination, it is revealed that you threw {{target}}, but did not let go of the arm!",
          :target_message=>"{{user}} grabs you by the arm, tossing you over {{user:his/her/its}} shoulder.  After closer examination, it is revealed that {{user}} threw you, but did not let go of the arm!",
          :spectator_message=>"{{user}} grabs {{target}} by the arm, tossing {{target:him/her/it}} over {{user:his/her/its}} shoulder.  After closer examination, it is revealed that {{user}} threw {{target}}, but did not let go of the arm!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"arm"
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-23,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"You poke {{target}} in the eyes.",
          :target_message=>"{{user}} pokes you in the eyes.",
          :spectator_message=>"{{user}} pokes {{target}} in the eyes.",
          :effects=>%{
            :stun=>4.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-50,
                :duration=>10.0
              },
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>12.0
              },
              %{
                :skill=>"block",
                :amount=>-50,
                :duration=>12.0
              },
              %{
                :skill=>"dodge",
                :amount=>-50,
                :duration=>12.0
              },
              %{
                :skill=>"melee",
                :amount=>-50,
                :duration=>12.0
              }
            ]
          }
        },
        %{
          :user_message=>"A voice booms \"FINISH HIM!\" as you rip off {{target}}'s head,  spinal cord still attached.",
          :target_message=>"A voice booms \"FINISH HIM!\" as {{user}} rips off your head,  spinal cord still attached.",
          :spectator_message=>"A voice booms \"FINISH HIM!\" as {{user}} rips off {{target}}'s head,  spinal cord still attached.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"head"
              }
            ]
          }
        }
      ],
      "C"=>[
        %{
          :user_message=>"You think about an old Japenese movie you saw once.",
          :target_message=>"{{user}} thinks about an old Japenese movie {{user:he/she/it}} saw once.",
          :spectator_message=>"{{user}} thinks about an old Japenese movie {{user:he/she/it}} saw once.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"{{target}} takes one wiff of your breath,  and the damage is done.",
          :target_message=>"You take one wiff of {{user}}'s breath,  and the damage is done.",
          :spectator_message=>"{{target}} takes one wiff of {{user}}'s breath,  and the damage is done.",
          :effects=>%{
            :damage=>0.13,
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
          :user_message=>"You kick {{target}} with a lot of skill.",
          :target_message=>"{{user}} kicks you with a lot of skill.",
          :spectator_message=>"{{user}} kicks {{target}} with a lot of skill.",
          :effects=>%{
            :damage=>0.15,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"You bring both of his arms down onto {{target}}'s shoulders, giving him {{target:him/her/it}} permanent spinal damage.",
          :target_message=>"{{user}} brings both of his arms down onto your shoulders, giving him you permanent spinal damage.",
          :spectator_message=>"{{user}} brings both of his arms down onto {{target}}'s shoulders, giving him {{target:him/her/it}} permanent spinal damage.",
          :effects=>%{
            :damage=>0.16,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-10,
                :duration=>10.0
              },
              %{
                :stat=>"agility",
                :amount=>-10,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"You give {{target}} whiplash with your attack.  Think of the chiropractic bills!  Ack.",
          :target_message=>"{{user}} gives you whiplash with {{user:his/her/its}} attack.  Think of the chiropractic bills!  Ack.",
          :spectator_message=>"{{user}} gives {{target}} whiplash with {{user:his/her/its}} attack.  Think of the chiropractic bills!  Ack.",
          :effects=>%{
            :damage=>0.17,
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-10,
                :duration=>12.0
              }
            ],
            :stun=>4.0
          }
        },
        %{
          :user_message=>"You knock {{target}}'s mind onto other subjects besides fighting.",
          :target_message=>"{{user}} knocks your mind onto other subjects besides fighting.",
          :spectator_message=>"{{user}} knocks {{target}}'s mind onto other subjects besides fighting.",
          :effects=>%{
            :damage=>0.01,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-69,
                :duration=>8.0
              },
              %{
                :skill=>"dodge",
                :amount=>-79,
                :duration=>8.0
              },
              %{
                :skill=>"parry",
                :amount=>-58,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} gets a foot in {{target:his/her/its}} mouth thanks to you.",
          :target_message=>"You get a foot in your mouth thanks to {{user}}.",
          :spectator_message=>"{{target}} gets a foot in {{target:his/her/its}} mouth thanks to {{user}}.",
          :effects=>%{
            :damage=>0.03,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>10.0
            }
          }
        },
        %{
          :user_message=>"With fire in your eyes, you cripple one of {{target}}'s arms!",
          :target_message=>"With fire in {{user:his/her/its}} eyes, {{user}} cripples one of your arms!",
          :spectator_message=>"With fire in {{user:his/her/its}} eyes, {{user}} cripples one of {{target}}'s arms!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :damage=>0.04,
            :damage_over_time=>%{
              :damage=>0.01,
              :duration=>6.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-34,
                :duration=>14.0
              }
            ]
          }
        },
        %{
          :user_message=>"OooooWAAA!  you rip off TWO of {{target}}'s limbs!",
          :target_message=>"OooooWAAA!  {{user}} rips off TWO of your limbs!",
          :spectator_message=>"OooooWAAA!  {{user}} rips off TWO of {{target}}'s limbs!",
          :effects=>%{
            :limb_loss=>[
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
          :user_message=>"With just one hand you open a deadly wound on {{target}}'s stomach.",
          :target_message=>"With just one hand {{user}} opens a deadly wound on your stomach.",
          :spectator_message=>"With just one hand {{user}} opens a deadly wound on {{target}}'s stomach.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.04,
              :duration=>18.0
            }
          }
        },
        %{
          :user_message=>"You smack {{target}} EXTREMELY hard, sending {{target}} to the ground with a loud thud.",
          :target_message=>"{{user}} smacks you EXTREMELY hard, sending you to the ground with a loud thud.",
          :spectator_message=>"{{user}} smacks {{target}} EXTREMELY hard, sending {{target}} to the ground with a loud thud.",
          :effects=>%{
            :damage=>0.41,
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-25,
                :duration=>14.0
              }
            ]
          }
        },
        %{
          :user_message=>"You find one of the ancient secret and fatal pressure points on {{target}}'s neck.  'nuf said.",
          :target_message=>"{{user}} finds one of the ancient secret and fatal pressure points on your neck.  'nuf said.",
          :spectator_message=>"{{user}} finds one of the ancient secret and fatal pressure points on {{target}}'s neck.  'nuf said.",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "D"=>[
        %{
          :user_message=>"You make a mean face at {{target}}.",
          :target_message=>"{{user}} makes a mean face at you.",
          :spectator_message=>"{{user}} makes a mean face at {{target}}.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You show {{target}} that magic sucks with your fancy show of martial arts skill.",
          :target_message=>"{{user}} shows you that magic sucks with {{user:his/her/its}} fancy show of martial arts skill.",
          :spectator_message=>"{{user}} shows {{target}} that magic sucks with {{user:his/her/its}} fancy show of martial arts skill.",
          :effects=>%{
            :stat_mod=>[
              %{
                :stat=>"willpower",
                :amount=>-40,
                :duration=>20.0
              },
              %{
                :stat=>"intellect",
                :amount=>-40,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"Get",
          :target_message=>"Get",
          :spectator_message=>"Get",
          :effects=>%{
            :stun=>3.0
          }
        },
        %{
          :user_message=>"{{target}} is totally stunned by your head butt.",
          :target_message=>"You are totally stunned by {{user}}'s head butt.",
          :spectator_message=>"{{target}} is totally stunned by {{user}}'s head butt.",
          :effects=>%{
            :stun=>7.0
          }
        },
        %{
          :user_message=>"{{target}} is SLAMMED in the noggin by you-  that could hurt for a while.",
          :target_message=>"You are SLAMMED in the noggin by {{user}}-  that could hurt for a while.",
          :spectator_message=>"{{target}} is SLAMMED in the noggin by {{user}}-  that could hurt for a while.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.04,
              :duration=>15.0
            }
          }
        },
        %{
          :user_message=>"You break {{target}}'s arm as quick as a whistle.",
          :target_message=>"{{user}} breaks your arm as quick as a whistle.",
          :spectator_message=>"{{user}} breaks {{target}}'s arm as quick as a whistle.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :damage=>0.03,
            :damage_over_time=>%{
              :damage=>0.01,
              :duration=>13.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-14,
                :duration=>10.0
              },
              %{
                :skill=>"parry",
                :amount=>-17,
                :duration=>16.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your relentless assault completely exhausts {{target}}.",
          :target_message=>"{{user}}'s relentless assault completely exhausts you.",
          :spectator_message=>"{{user}}'s relentless assault completely exhausts {{target}}.",
          :effects=>%{
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-45,
                :duration=>16.0
              }
            ],
            :damage=>0.03
          }
        },
        %{
          :user_message=>"Your kick penetrates deep into {{target}}'s insides.",
          :target_message=>"{{user}}'s kick penetrates deep into your insides.",
          :spectator_message=>"{{user}}'s kick penetrates deep into {{target}}'s insides.",
          :effects=>%{
            :damage=>0.53,
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>10.0
            },
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-34,
                :duration=>10.0
              }
            ],
            :stun=>1.0
          }
        },
        %{
          :user_message=>"With his bare hands you relieve {{target}} of three of his prized limbs.",
          :target_message=>"With his bare hands {{user}} relieves you of three of his prized limbs.",
          :spectator_message=>"With his bare hands {{user}} relieves {{target}} of three of his prized limbs.",
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
          :user_message=>"You slam down {{target}}'s back hard on his knee,  breaking {{target:his/her/its}} spine.",
          :target_message=>"{{user}} slams down your back hard on his knee,  breaking your spine.",
          :spectator_message=>"{{user}} slams down {{target}}'s back hard on his knee,  breaking {{target:his/her/its}} spine.",
          :effects=>%{
            :damage=>0.78,
            :stun=>5.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-15,
                :duration=>30.0
              },
              %{
                :stat=>"agility",
                :amount=>-25,
                :duration=>30.0
              }
            ]
          }
        },
        %{
          :user_message=>"Before {{target:he/she/it}} can react, you neatly alphabetize {{target}}'s vital organs on the ground.",
          :target_message=>"Before you can react, {{user}} neatly alphabetizes your vital organs on the ground.",
          :spectator_message=>"Before {{target:he/she/it}} can react, {{user}} neatly alphabetizes {{target}}'s vital organs on the ground.",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "E"=>[
        %{
          :user_message=>"You make an utter fool out of youself.  Just look at the fool!  Ha ha!",
          :target_message=>"{{user}} makes an utter fool out of {{user}}self.  Just look at the fool!  Ha ha!",
          :spectator_message=>"{{user}} makes an utter fool out of {{user}}self.  Just look at the fool!  Ha ha!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You girly punch {{target}} in the shoulder",
          :target_message=>"{{user}} girly punches you in the shoulder",
          :spectator_message=>"{{user}} girly punches {{target}} in the shoulder",
          :effects=>%{
            :damage=>0.07
          }
        },
        %{
          :user_message=>"Assuming a yoga position, you release {{target}}'s skills into the air!",
          :target_message=>"Assuming a yoga position, {{user}} releases your skills into the air!",
          :spectator_message=>"Assuming a yoga position, {{user}} releases {{target}}'s skills into the air!",
          :effects=>%{
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-80,
                :duration=>12.0
              },
              %{
                :skill=>"dodge",
                :amount=>-80,
                :duration=>12.0
              },
              %{
                :skill=>"parry",
                :amount=>-80,
                :duration=>12.0
              },
              %{
                :skill=>"block",
                :amount=>-80,
                :duration=>12.0
              },
              %{
                :skill=>"melee",
                :amount=>-80,
                :duration=>12.0
              }
            ]
          }
        },
        %{
          :user_message=>"You make a very good battering ram, as is seen with this attack!",
          :target_message=>"{{user}} makes a very good battering ram, as is seen with this attack!",
          :spectator_message=>"{{user}} makes a very good battering ram, as is seen with this attack!",
          :effects=>%{
            :damage=>0.32,
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>15.0
            }
          }
        },
        %{
          :user_message=>"With a swift sweep kick, you break {{target}}'s legs!",
          :target_message=>"With a swift sweep kick, {{user}} breaks your legs!",
          :spectator_message=>"With a swift sweep kick, {{user}} breaks {{target}}'s legs!",
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
            ]
          }
        },
        %{
          :user_message=>"Burning with rage, you totally annihilate {{target}} with a barrage of devastating punches and kicks.",
          :target_message=>"Burning with rage, {{user}} totally annihilates you with a barrage of devastating punches and kicks.",
          :spectator_message=>"Burning with rage, {{user}} totally annihilates {{target}} with a barrage of devastating punches and kicks.",
          :effects=>%{
            :damage=>0.57,
            :damage_over_time=>%{
              :damage=>0.01,
              :duration=>20.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-23,
                :duration=>10.0
              }
            ],
            :stun=>3.0
          }
        },
        %{
          :user_message=>"You cup your hands together, using them to bash {{target}} out of comition.  Knock out!",
          :target_message=>"{{user}} cups {{user:his/her/its}} hands together, using them to bash you out of comition.  Knock out!",
          :spectator_message=>"{{user}} cups {{user:his/her/its}} hands together, using them to bash {{target}} out of comition.  Knock out!",
          :effects=>%{
            :damage=>0.17,
            :stun=>10.0
          }
        },
        %{
          :user_message=>"Yelling all the way, you rip FOUR of {{target}}'s useful limbs off, proceeding to use them as confetti.",
          :target_message=>"Yelling all the way, {{user}} rips FOUR of your useful limbs off, proceeding to use them as confetti.",
          :spectator_message=>"Yelling all the way, {{user}} rips FOUR of {{target}}'s useful limbs off, proceeding to use them as confetti.",
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
            ]
          }
        },
        %{
          :user_message=>"Falling deep into meditation, you call upon a red dragon to take care of {{target}}.",
          :target_message=>"Falling deep into meditation, {{user}} calls upon a red dragon to take care of you.",
          :spectator_message=>"Falling deep into meditation, {{user}} calls upon a red dragon to take care of {{target}}.",
          :effects=>%{
            :damage=>1.25
          }
        },
        %{
          :user_message=>"Falling deep into meditation, you cause {{target}} to simply melt.",
          :target_message=>"Falling deep into meditation, {{user}} causes you to simply melt.",
          :spectator_message=>"Falling deep into meditation, {{user}} causes {{target}} to simply melt.",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"You jump right through {{target}}.  That could never heal!",
          :target_message=>"{{user}} jumps right through you.  That could never heal!",
          :spectator_message=>"{{user}} jumps right through {{target}}.  That could never heal!",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.2,
              :duration=>50.0
            }
          }
        },
        %{
          :user_message=>"With a \"HI-YA!\" and a \"UUMM-BA!\" you violently dismember {{target}} with his bare hands.  Who do you think is going to clean up this mess?",
          :target_message=>"With a \"HI-YA!\" and a \"UUMM-BA!\" {{user}} violently dismembers you with his bare hands.  Who do you think is going to clean up this mess?",
          :spectator_message=>"With a \"HI-YA!\" and a \"UUMM-BA!\" {{user}} violently dismembers {{target}} with his bare hands.  Who do you think is going to clean up this mess?",
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
            :stun=>30.0
          }
        }
      ]
    }  end

end
