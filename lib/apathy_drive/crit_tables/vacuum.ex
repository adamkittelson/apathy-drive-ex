defmodule ApathyDrive.CritTables.Vacuum do

  def name do
    "vacuum"
  end

  def damage_type do
    "magical"
  end

  def crits do
    %{
      "A"=>[
        %{
          :user_message=>"You clean up {{target}}'s feet with a gust of air.  Big deal.",
          :target_message=>"{{user}} cleans up your feet with a gust of air.  Big deal.",
          :spectator_message=>"{{user}} cleans up {{target}}'s feet with a gust of air.  Big deal.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"A sudden gust brings {{target}} slightly towards you. {{target}} is a little stunned",
          :target_message=>"A sudden gust brings you slightly towards {{user}}. You is a little stunned",
          :spectator_message=>"A sudden gust brings {{target}} slightly towards {{user}}. {{target}} is a little stunned",
          :effects=>%{
            :stun=>1.0
          }
        },
        %{
          :user_message=>"You blind {{target}} by sucking {{target}}'s focus out of alignment.",
          :target_message=>"{{user}} blinds you by sucking your focus out of alignment.",
          :spectator_message=>"{{user}} blinds {{target}} by sucking {{target}}'s focus out of alignment.",
          :effects=>%{
            :stun=>3.0
          }
        },
        %{
          :user_message=>"You slam {{target}} to the ground with a powerful sucking noise.",
          :target_message=>"{{user}} slams you to the ground with a powerful sucking noise.",
          :spectator_message=>"{{user}} slams {{target}} to the ground with a powerful sucking noise.",
          :effects=>%{
            :damage=>0.7
          }
        },
        %{
          :user_message=>"With a loud FWOOSH {{target}} get slammed to the ground.  SLAM!.",
          :target_message=>"With a loud FWOOSH you gets slammed to the ground.  SLAM!.",
          :spectator_message=>"With a loud FWOOSH {{target}} gets slammed to the ground.  SLAM!.",
          :effects=>%{
            :damage=>0.08
          }
        },
        %{
          :user_message=>"{{target}} pants loudly as you creates a vacuum that {{target:he/she/it}} must resist. {{target}} is exhausted.",
          :target_message=>"You pant loudly as {{user}} creates a vacuum that you must resist. You is exhausted.",
          :spectator_message=>"{{target}} pants loudly as {{user}} creates a vacuum that {{target:he/she/it}} must resist. {{target}} is exhausted.",
          :effects=>%{
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
            ],
            :damage=>0.01
          }
        },
        %{
          :user_message=>"You suck {{target}}'s hand into {{target:his/her/its}} own face rather roughly.",
          :target_message=>"{{user}} sucks your hand into your own face rather roughly.",
          :spectator_message=>"{{user}} sucks {{target}}'s hand into {{target:his/her/its}} own face rather roughly.",
          :effects=>%{
            :damage=>0.12
          }
        },
        %{
          :user_message=>"You suck open a small wound on {{target}}'s chest.",
          :target_message=>"{{user}} sucks open a small wound on your chest.",
          :spectator_message=>"{{user}} sucks open a small wound on {{target}}'s chest.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.2,
              :duration=>14.0
            }
          }
        },
        %{
          :user_message=>"You suck open a small wound on {{target}}'s chest.",
          :target_message=>"{{user}} sucks open a small wound on your chest.",
          :spectator_message=>"{{user}} sucks open a small wound on {{target}}'s chest.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.4,
              :duration=>9.0
            }
          }
        },
        %{
          :user_message=>"You suck open a small wound on {{target}}'s chest.",
          :target_message=>"{{user}} sucks open a small wound on your chest.",
          :spectator_message=>"{{user}} sucks open a small wound on {{target}}'s chest.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.5,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"You use a powerful vacuum to call on the ground to attack {{target}}. {{target}} is temporarily buried.",
          :target_message=>"{{user}} uses a powerful vacuum to call on the ground to attack you. You is temporarily buried.",
          :spectator_message=>"{{user}} uses a powerful vacuum to call on the ground to attack {{target}}. {{target}} is temporarily buried.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>3.0
            },
            :damage=>0.03,
            :stun=>2.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-5,
                :duration=>6.0
              },
              %{
                :skill=>"parry",
                :amount=>-4,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} suddenly slirps up a mouthful of dirt, no thanks to you.",
          :target_message=>"You suddenly slirp up a mouthful of dirt, no thanks to {{user}}.",
          :spectator_message=>"{{target}} suddenly slirps up a mouthful of dirt, no thanks to {{user}}.",
          :effects=>%{
            :stun=>1.0,
            :damage=>0.05,
            :stat_mod=>[
              %{
                :stat=>"charm",
                :amount=>-15,
                :duration=>40.0
              }
            ]
          }
        },
        %{
          :user_message=>"You cause gale force winds to violently throw {{target}} against all pain-inflicting objects present, inflicting massive damage.",
          :target_message=>"{{user}} causes gale force winds to violently throw you against all pain-inflicting objects present, inflicting massive damage.",
          :spectator_message=>"{{user}} causes gale force winds to violently throw {{target}} against all pain-inflicting objects present, inflicting massive damage.",
          :effects=>%{
            :damage=>0.54
          }
        },
        %{
          :user_message=>"FLOP!  {{target}}'s right arm is suddenly sucked into {{target:his/her/its}} body.  Ouch!",
          :target_message=>"FLOP!  your right arm is suddenly sucked into your body.  Ouch!",
          :spectator_message=>"FLOP!  {{target}}'s right arm is suddenly sucked into {{target:his/her/its}} body.  Ouch!",
          :effects=>%{
            :damage=>0.11
          }
        },
        %{
          :user_message=>"FLOP!  {{target}}'s left arm is suddenly sucked powerfully to the ground, and {{target:his/her/its}} head is bashed along with it!",
          :target_message=>"FLOP!  your left arm is suddenly sucked powerfully to the ground, and your head is bashed along with it!",
          :spectator_message=>"FLOP!  {{target}}'s left arm is suddenly sucked powerfully to the ground, and {{target:his/her/its}} head is bashed along with it!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"left arm"
              }
            ],
            :damage=>0.03,
            :damage_over_time=>%{
              :damage=>0.3,
              :duration=>4.0
            }
          }
        },
        %{
          :user_message=>"WHOOPS!  you STUN 4 the suction holding {{target}}'s body together to weaken dramatically, causing {{target}} great pain.",
          :target_message=>"WHOOPS!  {{user}} STUN 4s the suction holding your body together to weaken dramatically, causing you great pain.",
          :spectator_message=>"WHOOPS!  {{user}} STUN 4s the suction holding {{target}}'s body together to weaken dramatically, causing {{target}} great pain.",
          :effects=>%{
            :damage=>0.55
          }
        }
      ],
      "B"=>[
        %{
          :user_message=>"Your attempt to create a vacuum fails miseriably.  Sigh.",
          :target_message=>"{{user}}'s attempt to create a vacuum fails miseriably.  Sigh.",
          :spectator_message=>"{{user}}'s attempt to create a vacuum fails miseriably.  Sigh.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You pull {{target}}'s jaw straight to the ground with a magical force.",
          :target_message=>"{{user}} pulls your jaw straight to the ground with a magical force.",
          :spectator_message=>"{{user}} pulls {{target}}'s jaw straight to the ground with a magical force.",
          :effects=>%{
            :damage=>0.11,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"You set your vacuum to reverse!  Tons of debris and filth are blown at {{target}},  severly impairing {{target:he/she/it}} sight!",
          :target_message=>"{{user}} sets {{user:his/her/its}} vacuum to reverse!  Tons of debris and filth are blown at you,  severly impairing you sight!",
          :spectator_message=>"{{user}} sets {{user:his/her/its}} vacuum to reverse!  Tons of debris and filth are blown at {{target}},  severly impairing {{target:he/she/it}} sight!",
          :effects=>%{
            :damage=>0.01,
            :stun=>1.0,
            :stat_mod=>[
              %{
                :stat=>"charm",
                :amount=>-20,
                :duration=>40.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-10,
                :duration=>10.0
              },
              %{
                :skill=>"melee",
                :amount=>-10,
                :duration=>10.0
              },
              %{
                :skill=>"parry",
                :amount=>-10,
                :duration=>10.0
              },
              %{
                :skill=>"block",
                :amount=>-10,
                :duration=>10.0
              },
              %{
                :skill=>"dodge",
                :amount=>-10,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"You think, 'WOW!  What a neat attack!'  But no damage is done.",
          :target_message=>"{{user}} thinks, 'WOW!  What a neat attack!'  But no damage is done.",
          :spectator_message=>"{{user}} thinks, 'WOW!  What a neat attack!'  But no damage is done.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You open a wound in {{target}}'s side without even touching {{target:him/her/it}}.",
          :target_message=>"{{user}} opens a wound in your side without even touching you.",
          :spectator_message=>"{{user}} opens a wound in {{target}}'s side without even touching {{target:him/her/it}}.",
          :effects=>%{
            :damage=>0.06,
            :damage_over_time=>%{
              :damage=>0.5,
              :duration=>4.0
            }
          }
        },
        %{
          :user_message=>"{{target}} feels like his brain is being sucked into nothing!  AHHH!  {{target:he/she/it}} can't do anything!",
          :target_message=>"You feel like his brain is being sucked into nothing!  AHHH!  you can't do anything!",
          :spectator_message=>"{{target}} feels like his brain is being sucked into nothing!  AHHH!  {{target:he/she/it}} can't do anything!",
          :effects=>%{
            :stun=>5.0,
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-5,
                :duration=>12.0
              },
              %{
                :stat=>"willpower",
                :amount=>-5,
                :duration=>12.0
              }
            ],
            :damage=>0.03
          }
        },
        %{
          :user_message=>"{{target}} screams as he is pulled into the ground by a magical force, only to be quickly spat out.",
          :target_message=>"You scream as he is pulled into the ground by a magical force, only to be quickly spat out.",
          :spectator_message=>"{{target}} screams as he is pulled into the ground by a magical force, only to be quickly spat out.",
          :effects=>%{
            :damage=>0.12,
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
          :user_message=>"You cause the earth around {{target}} to fly up around {{target:him/her/it}}, effectively stopping any spells {{target}} might have wished to cast.",
          :target_message=>"{{user}} causes the earth around you to fly up around you, effectively stopping any spells you might have wished to cast.",
          :spectator_message=>"{{user}} causes the earth around {{target}} to fly up around {{target:him/her/it}}, effectively stopping any spells {{target}} might have wished to cast.",
          :effects=>%{
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-20,
                :duration=>6.0
              },
              %{
                :stat=>"willpower",
                :amount=>-20,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}}'s stomach is SQUEEEZED by your attack, causing {{target:him/her/it}} to puke all over.",
          :target_message=>"Your stomach is SQUEEEZED by {{user}}'s attack, causing you to puke all over.",
          :spectator_message=>"{{target}}'s stomach is SQUEEEZED by {{user}}'s attack, causing {{target:him/her/it}} to puke all over.",
          :effects=>%{
            :damage=>0.09,
            :stun=>3.0,
            :stat_mod=>[
              %{
                :stat=>"charm",
                :amount=>-5,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"You make {{target}} spin around and around, causing {{target:him/her/it}} to become quite dazed.",
          :target_message=>"{{user}} makes you spin around and around, causing you to become quite dazed.",
          :spectator_message=>"{{user}} makes {{target}} spin around and around, causing {{target:him/her/it}} to become quite dazed.",
          :effects=>%{
            :damage=>0.03,
            :stun=>2.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-5,
                :duration=>nil
              },
              %{
                :stat=>"agility",
                :amount=>-5,
                :duration=>nil
              },
              %{
                :stat=>"intellect",
                :amount=>-5,
                :duration=>nil
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-16,
                :duration=>6.0
              },
              %{
                :skill=>"block",
                :amount=>-3,
                :duration=>6.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>2.0
            }
          }
        },
        %{
          :user_message=>"You suck {{target}}'s hair towards the sky VERY hard, causing {{target:him/her/it}} to whimper in anguish.",
          :target_message=>"{{user}} sucks your hair towards the sky VERY hard, causing you to whimper in anguish.",
          :spectator_message=>"{{user}} sucks {{target}}'s hair towards the sky VERY hard, causing {{target:him/her/it}} to whimper in anguish.",
          :effects=>%{
            :damage=>0.36,
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>3.0
            }
          }
        },
        %{
          :user_message=>"Suddenly one of {{target}}'s limbs flies off into the air and into your hand. You hold it up in the air triumphantly.  Hurrah!",
          :target_message=>"Suddenly one of your limbs flies off into the air and into {{user}}'s hand. {{user:he/she/it}} holds it up in the air triumphantly.  Hurrah!",
          :spectator_message=>"Suddenly one of {{target}}'s limbs flies off into the air and into {{user}}'s hand. {{user:he/she/it}} holds it up in the air triumphantly.  Hurrah!",
          :effects=>%{
            :damage=>0.11,
            :damage_over_time=>%{
              :damage=>0.2,
              :duration=>4.0
            },
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} feels the bones in {{target:his/her/its}} right hand crush.  This was your doing!.",
          :target_message=>"You feel the bones in your right hand crush.  This was {{user}}'s doing!.",
          :spectator_message=>"{{target}} feels the bones in {{target:his/her/its}} right hand crush.  This was {{user}}'s doing!.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.2,
              :duration=>3.0
            },
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"right hand"
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-10,
                :duration=>20.0
              },
              %{
                :skill=>"melee",
                :amount=>-30,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} feels the bones in {{target:his/her/its}} left hand crush.  This was your doing!.",
          :target_message=>"You feel the bones in your left hand crush.  This was {{user}}'s doing!.",
          :spectator_message=>"{{target}} feels the bones in {{target:his/her/its}} left hand crush.  This was {{user}}'s doing!.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.2,
              :duration=>3.0
            },
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"left hand"
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-10,
                :duration=>20.0
              },
              %{
                :skill=>"melee",
                :amount=>-30,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} feels the bones in BOTH of {{target:his/her/its}} hands crush.  This was your evil doing, no doubt!",
          :target_message=>"You feel the bones in BOTH of your hands crush.  This was {{user}}'s evil doing, no doubt!",
          :spectator_message=>"{{target}} feels the bones in BOTH of {{target:his/her/its}} hands crush.  This was {{user}}'s evil doing, no doubt!",
          :effects=>%{
            :damage=>0.07,
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"left hand"
              },
              %{
                :kind=>"cripple",
                :limb=>"right hand"
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-13,
                :duration=>10.0
              },
              %{
                :skill=>"melee",
                :amount=>-40,
                :duration=>40.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} is slowly and painfully bent into exotic and painful positions.",
          :target_message=>"You is slowly and painfully bent into exotic and painful positions.",
          :spectator_message=>"{{target}} is slowly and painfully bent into exotic and painful positions.",
          :effects=>%{
            :stun=>5.0,
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"leg"
              }
            ],
            :damage=>0.75
          }
        }
      ],
      "C"=>[
        %{
          :user_message=>"Your attempt to create a vacuum fails miseriably.  Sigh.",
          :target_message=>"{{user}}'s attempt to create a vacuum fails miseriably.  Sigh.",
          :spectator_message=>"{{user}}'s attempt to create a vacuum fails miseriably.  Sigh.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"{{target}} feels pain in his spine as {{target:he/she/it}} is squashed like the bug that {{target:he/she/it}} is.",
          :target_message=>"You feel pain in his spine as you is squashed like the bug that you is.",
          :spectator_message=>"{{target}} feels pain in his spine as {{target:he/she/it}} is squashed like the bug that {{target:he/she/it}} is.",
          :effects=>%{
            :damage=>0.16,
            :skill_mod=>[
              %{
                :skill=>"strength",
                :amount=>-40,
                :duration=>7.0
              }
            ]
          }
        },
        %{
          :user_message=>"You manage to rip open a hole in {{target}}'s thigh.",
          :target_message=>"{{user}} manages to rip open a hole in your thigh.",
          :spectator_message=>"{{user}} manages to rip open a hole in {{target}}'s thigh.",
          :effects=>%{
            :damage=>0.09,
            :damage_over_time=>%{
              :damage=>0.3,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"You manage to rip open a hole in {{target}}'s arm.",
          :target_message=>"{{user}} manages to rip open a hole in your arm.",
          :spectator_message=>"{{user}} manages to rip open a hole in {{target}}'s arm.",
          :effects=>%{
            :damage=>0.09,
            :damage_over_time=>%{
              :damage=>0.3,
              :duration=>5.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-10,
                :duration=>12.0
              },
              %{
                :skill=>"block",
                :amount=>-18,
                :duration=>12.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}}'s skin pores suddenly suck up millions of nearby germs.",
          :target_message=>"Your skin pores suddenly suck up millions of nearby germs.",
          :spectator_message=>"{{target}}'s skin pores suddenly suck up millions of nearby germs.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.8,
              :duration=>8.0
            }
          }
        },
        %{
          :user_message=>"You manage to rip open {{target}}'s thigh.",
          :target_message=>"{{user}} manages to rip open your thigh.",
          :spectator_message=>"{{user}} manages to rip open {{target}}'s thigh.",
          :effects=>%{
            :damage=>0.13,
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>4.0
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
          :user_message=>"You rip {{target}}'s brain cell connections, dumbfounding {{target:him/her/it}}.",
          :target_message=>"{{user}} rips your brain cell connections, dumbfounding you.",
          :spectator_message=>"{{user}} rips {{target}}'s brain cell connections, dumbfounding {{target:him/her/it}}.",
          :effects=>%{
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-10,
                :duration=>20.0
              },
              %{
                :skill=>"dodge",
                :amount=>-10,
                :duration=>20.0
              },
              %{
                :skill=>nil,
                :amount=>-20,
                :duration=>28.0
              }
            ],
            :stun=>5.0,
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-20,
                :duration=>16.0
              }
            ]
          }
        },
        %{
          :user_message=>"You manage to rip open {{target}}'s arm.",
          :target_message=>"{{user}} manages to rip open your arm.",
          :spectator_message=>"{{user}} manages to rip open {{target}}'s arm.",
          :effects=>%{
            :damage=>0.12,
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>5.0
            },
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-15,
                :duration=>6.0
              },
              %{
                :skill=>"block",
                :amount=>-15,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You cause the air around {{target}} to become a vacuum, which quickly relieves {{target}} of {{target:his/her/its}} skin.",
          :target_message=>"{{user}} causes the air around you to become a vacuum, which quickly relieves you of your skin.",
          :spectator_message=>"{{user}} causes the air around {{target}} to become a vacuum, which quickly relieves {{target}} of {{target:his/her/its}} skin.",
          :effects=>%{
            :damage=>0.79,
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>7.0
            }
          }
        },
        %{
          :user_message=>"All sharp objects in the vicinity decide to move towards {{target}} at a very quick pace.",
          :target_message=>"All sharp objects in the vicinity decide to move towards you at a very quick pace.",
          :spectator_message=>"All sharp objects in the vicinity decide to move towards {{target}} at a very quick pace.",
          :effects=>%{
            :damage=>0.85
          }
        },
        %{
          :user_message=>"Where did {{target}}'s legs go?  Oh, you made them shoot into {{target:his/her/its}} own body.",
          :target_message=>"Where did your legs go?  Oh, {{user}} made them shoot into your own body.",
          :spectator_message=>"Where did {{target}}'s legs go?  Oh, {{user}} made them shoot into {{target:his/her/its}} own body.",
          :effects=>%{
            :damage=>0.1,
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>10.0
            },
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-10,
                :duration=>20.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-10,
                :duration=>20.0
              }
            ],
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"right leg"
              },
              %{
                :kind=>"sever",
                :limb=>"left leg"
              }
            ]
          }
        },
        %{
          :user_message=>"Before anyone can react {{target}}'s head shoots into the air!",
          :target_message=>"Before anyone can react your head shoots into the air!",
          :spectator_message=>"Before anyone can react {{target}}'s head shoots into the air!",
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
      "D"=>[
        %{
          :user_message=>"Everyone laughs when you attempt to create a powerful vacuum.",
          :target_message=>"Everyone laughs when {{user}} attempts to create a powerful vacuum.",
          :spectator_message=>"Everyone laughs when {{user}} attempts to create a powerful vacuum.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"{{target}}'s brain is messed up by you, and {{target:he/she/it}} forgets what to do!",
          :target_message=>"Your brain is messed up by {{user}}, and you forget what to do!",
          :spectator_message=>"{{target}}'s brain is messed up by {{user}}, and {{target:he/she/it}} forgets what to do!",
          :effects=>%{
            :stun=>7.0
          }
        },
        %{
          :user_message=>"{{target}}'s foot is sucked into {{target:his/her/its}} mouth by you!",
          :target_message=>"Your foot is sucked into your mouth by {{user}}!",
          :spectator_message=>"{{target}}'s foot is sucked into {{target:his/her/its}} mouth by {{user}}!",
          :effects=>%{
            :stun=>2.0,
            :damage=>0.07,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-16,
                :duration=>8.0
              },
              %{
                :skill=>"parry",
                :amount=>-20,
                :duration=>10.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-15,
                :duration=>10.0
              }
            ],
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"foot"
              }
            ]
          }
        },
        %{
          :user_message=>"You make the ground suck up one of {{target}}'s limbs!  Oh no!",
          :target_message=>"{{user}} makes the ground suck up one of your limbs!  Oh no!",
          :spectator_message=>"{{user}} makes the ground suck up one of {{target}}'s limbs!  Oh no!",
          :effects=>%{
            :damage=>0.16,
            :damage_over_time=>%{
              :damage=>0.2,
              :duration=>3.0
            },
            :stun=>1.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-5,
                :duration=>26.0
              }
            ],
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"non_fatal"
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} breaths in more air than is natural,  but doesn't quite explode. {{target:he/she/it}} is severly hurt.",
          :target_message=>"You breath in more air than is natural,  but doesn't quite explode. You is severly hurt.",
          :spectator_message=>"{{target}} breaths in more air than is natural,  but doesn't quite explode. {{target:he/she/it}} is severly hurt.",
          :effects=>%{
            :damage=>0.66,
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>15.0
            },
            :stun=>2.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-4,
                :duration=>14.0
              }
            ],
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
          :user_message=>"You create a powerful vacuum which sucks you, fists first, right into {{target}}'s jaw.",
          :target_message=>"{{user}} creates a powerful vacuum which sucks {{user:him/her/it}}, fists first, right into your jaw.",
          :spectator_message=>"{{user}} creates a powerful vacuum which sucks {{user:him/her/it}}, fists first, right into {{target}}'s jaw.",
          :effects=>%{
            :damage=>0.35,
            :stun=>2.0,
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-15,
                :duration=>14.0
              }
            ]
          }
        },
        %{
          :user_message=>"You create a powerful vacuum which sucks you, feet first, right into {{target}}'s jaw.",
          :target_message=>"{{user}} creates a powerful vacuum which sucks {{user:him/her/it}}, feet first, right into your jaw.",
          :spectator_message=>"{{user}} creates a powerful vacuum which sucks {{user:him/her/it}}, feet first, right into {{target}}'s jaw.",
          :effects=>%{
            :damage=>0.45,
            :stun=>3.0,
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-15,
                :duration=>14.0
              }
            ]
          }
        },
        %{
          :user_message=>"You create a powerful vacuum which sucks you, feet first, right through {{target}}'s stomach.",
          :target_message=>"{{user}} creates a powerful vacuum which sucks {{user:him/her/it}}, feet first, right through your stomach.",
          :spectator_message=>"{{user}} creates a powerful vacuum which sucks {{user:him/her/it}}, feet first, right through {{target}}'s stomach.",
          :effects=>%{
            :damage=>0.55,
            :stun=>4.0,
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
          :user_message=>"Two of {{target}}'s limbs are sucked into nothingness.",
          :target_message=>"Two of your limbs are sucked into nothingness.",
          :spectator_message=>"Two of {{target}}'s limbs are sucked into nothingness.",
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
            ],
            :damage=>0.04
          }
        },
        %{
          :user_message=>"You open a wound in {{target}}'s chest that looks like it could never heal!",
          :target_message=>"{{user}} opens a wound in your chest that looks like it could never heal!",
          :spectator_message=>"{{user}} opens a wound in {{target}}'s chest that looks like it could never heal!",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.5,
              :duration=>50.0
            }
          }
        },
        %{
          :user_message=>"You open up a vacuum rip in the space-time continuum.  Aliens that get sucked through make quick work of {{target}}.",
          :target_message=>"{{user}} opens up a vacuum rip in the space-time continuum.  Aliens that get sucked through make quick work of you.",
          :spectator_message=>"{{user}} opens up a vacuum rip in the space-time continuum.  Aliens that get sucked through make quick work of {{target}}.",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"You cause four of {{target}}'s limbs to be sucked into nothingness!",
          :target_message=>"{{user}} causes four of your limbs to be sucked into nothingness!",
          :spectator_message=>"{{user}} causes four of {{target}}'s limbs to be sucked into nothingness!",
          :effects=>%{
            :damage=>0.05,
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
          :user_message=>"{{target}}'s head is sucked into {{target:his/her/its}} body only to be shot out of {{target:his/her/its}} chest!",
          :target_message=>"Your head is sucked into your body only to be shot out of your chest!",
          :spectator_message=>"{{target}}'s head is sucked into {{target:his/her/its}} body only to be shot out of {{target:his/her/its}} chest!",
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
      "E"=>[
        %{
          :user_message=>"'I sure want some toast,' you think, forgetting to attack.",
          :target_message=>"'I sure want some toast,' {{user}} thinks, forgetting to attack.",
          :spectator_message=>"'I sure want some toast,' {{user}} thinks, forgetting to attack.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"Some nearby loose objects fly into {{target}}'s mouth, causing severe digestive track damage.",
          :target_message=>"Some nearby loose objects fly into your mouth, causing severe digestive track damage.",
          :spectator_message=>"Some nearby loose objects fly into {{target}}'s mouth, causing severe digestive track damage.",
          :effects=>%{
            :stun=>2.0,
            :damage=>0.22,
            :damage_over_time=>%{
              :damage=>0.4,
              :duration=>9.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-20,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"You cackle as {{target}}'s liver flies out through {{target:his/her/its}} chest and into your hand!",
          :target_message=>"{{user}} cackles as your liver flies out through your chest and into {{user}}'s hand!",
          :spectator_message=>"{{user}} cackles as {{target}}'s liver flies out through {{target:his/her/its}} chest and into {{user}}'s hand!",
          :effects=>%{
            :damage=>0.23,
            :damage_over_time=>%{
              :damage=>0.6,
              :duration=>6.0
            },
            :skill_mod=>[
              %{
                :skill=>"dodge",
                :amount=>-20,
                :duration=>12.0
              }
            ]
          }
        },
        %{
          :user_message=>"You suck out {{target}}'s eyes!",
          :target_message=>"{{user}} sucks out your eyes!",
          :spectator_message=>"{{user}} sucks out {{target}}'s eyes!",
          :effects=>%{
            :damage=>0.2,
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>10.0
            },
            :stun=>10.0
          }
        },
        %{
          :user_message=>"Despite {{target:his/her/its}} struggles, {{target}} is pulled violently towards your fist, which goes right through {{target:him/her/it}}.",
          :target_message=>"Despite your struggles, you is pulled violently towards {{user}}'s fist, which goes right through you.",
          :spectator_message=>"Despite {{target:his/her/its}} struggles, {{target}} is pulled violently towards {{user}}'s fist, which goes right through {{target:him/her/it}}.",
          :effects=>%{
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-46,
                :duration=>15.0
              }
            ]
          }
        },
        %{
          :user_message=>"You magically pull {{target}} in two directions, ripping off one of {{target:his/her/its}} arms.",
          :target_message=>"{{user}} magically pulls you in two directions, ripping off one of your arms.",
          :spectator_message=>"{{user}} magically pulls {{target}} in two directions, ripping off one of {{target:his/her/its}} arms.",
          :effects=>%{
            :damage=>0.13,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-20,
                :duration=>24.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-25,
                :duration=>24.0
              }
            ]
          }
        },
        %{
          :user_message=>"You suck {{target}}'s will to cast spells right out of {{target:him/her/it}}!",
          :target_message=>"{{user}} sucks your will to cast spells right out of you!",
          :spectator_message=>"{{user}} sucks {{target}}'s will to cast spells right out of {{target:him/her/it}}!",
          :effects=>%{
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-40,
                :duration=>40.0
              },
              %{
                :stat=>"willpower",
                :amount=>-40,
                :duration=>40.0
              }
            ]
          }
        },
        %{
          :user_message=>"You suck {{target}}'s will to fight right out of {{target:him/her/it}}!",
          :target_message=>"{{user}} sucks your will to fight right out of you!",
          :spectator_message=>"{{user}} sucks {{target}}'s will to fight right out of {{target:him/her/it}}!",
          :effects=>%{
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-87,
                :duration=>40.0
              }
            ]
          }
        },
        %{
          :user_message=>"With a FWOOSH, {{target}}'s left lung flies out through {{target:his/her/its}} mouth!",
          :target_message=>"With a FWOOSH, your left lung flies out through your mouth!",
          :spectator_message=>"With a FWOOSH, {{target}}'s left lung flies out through {{target:his/her/its}} mouth!",
          :effects=>%{
            :damage=>1.07
          }
        },
        %{
          :user_message=>"You magically tear apart every bone in {{target}}'s body.",
          :target_message=>"{{user}} magically tears apart every bone in your body.",
          :spectator_message=>"{{user}} magically tears apart every bone in {{target}}'s body.",
          :effects=>%{
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-35,
                :duration=>40.0
              },
              %{
                :stat=>"agility",
                :amount=>-40,
                :duration=>40.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-79,
                :duration=>40.0
              },
              %{
                :skill=>"dodge",
                :amount=>-80,
                :duration=>40.0
              },
              %{
                :skill=>"parry",
                :amount=>-92,
                :duration=>40.0
              },
              %{
                :skill=>"block",
                :amount=>-93,
                :duration=>40.0
              }
            ]
          }
        },
        %{
          :user_message=>"You cause {{target}}'s stomach to suck up all of {{target:his/her/its}} vital organs.  Stomach acid is released within!",
          :target_message=>"{{user}} causes your stomach to suck up all of your vital organs.  Stomach acid is released within!",
          :spectator_message=>"{{user}} causes {{target}}'s stomach to suck up all of {{target:his/her/its}} vital organs.  Stomach acid is released within!",
          :effects=>%{
            :damage=>2.56
          }
        },
        %{
          :user_message=>"Without warning, {{target}} breaths in MUCH more air then he can handle.  The explosion splatters a bit of intestine on your eye.",
          :target_message=>"Without warning, you breath in MUCH more air then he can handle.  The explosion splatters a bit of intestine on {{user}}'s eye.",
          :spectator_message=>"Without warning, {{target}} breaths in MUCH more air then he can handle.  The explosion splatters a bit of intestine on {{user}}'s eye.",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"You open a vacuum rip in the space-time continuum.  Aliens proceed to totally mutilate {{target}}.",
          :target_message=>"{{user}} opens a vacuum rip in the space-time continuum.  Aliens proceed to totally mutilate you.",
          :spectator_message=>"{{user}} opens a vacuum rip in the space-time continuum.  Aliens proceed to totally mutilate {{target}}.",
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
              }
            ]
          }
        },
        %{
          :user_message=>"You magically create a GIANT vacuum cleaner that will gradually suck {{target}} to his doom.",
          :target_message=>"{{user}} magically creates a GIANT vacuum cleaner that will gradually suck you to his doom.",
          :spectator_message=>"{{user}} magically creates a GIANT vacuum cleaner that will gradually suck {{target}} to his doom.",
          :effects=>%{
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-40,
                :duration=>40.0
              },
              %{
                :stat=>"agility",
                :amount=>-40,
                :duration=>40.0
              },
              %{
                :stat=>"intellect",
                :amount=>-43,
                :duration=>40.0
              },
              %{
                :stat=>"willpower",
                :amount=>-45,
                :duration=>40.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-79,
                :duration=>40.0
              },
              %{
                :skill=>"dodge",
                :amount=>-80,
                :duration=>40.0
              },
              %{
                :skill=>"parry",
                :amount=>-92,
                :duration=>40.0
              },
              %{
                :skill=>"block",
                :amount=>-93,
                :duration=>40.0
              }
            ],
            :stun=>20.0,
            :damage_over_time=>%{
              :damage=>3.0,
              :duration=>50.0
            }
          }
        }
      ]
    }
  end

end
