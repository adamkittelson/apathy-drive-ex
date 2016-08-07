defmodule ApathyDrive.CritTables.Cutting do

  def name do
    "cutting"
  end

  def damage_type do
    "physical"
  end

  def crits do
    %{
      "A"=>[
        %{
          :user_message=>"You lightly tap {{target}}.",
          :target_message=>"{{user}} lightly taps you.",
          :spectator_message=>"{{user}} lightly taps {{target}}.",
          :effects=>%{
            :damage=>0.01
          }
        },
        %{
          :user_message=>"You fail to do anything at all to {{target}}.",
          :target_message=>"{{user}} fails to do anything at all to you.",
          :spectator_message=>"{{user}} fails to do anything at all to {{target}}.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You get a good one in on {{target}}.",
          :target_message=>"{{user}} gets a good one in on you.",
          :spectator_message=>"{{user}} gets a good one in on {{target}}.",
          :effects=>%{
            :damage=>0.02,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-5,
                :duration=>40.0
              }
            ]
          }
        },
        %{
          :user_message=>"You inflict a minor wound on {{target}}'s calf.",
          :target_message=>"{{user}} inflicts a minor wound on your calf.",
          :spectator_message=>"{{user}} inflicts a minor wound on {{target}}'s calf.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.01,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"You deliver a blow to {{target}}'s back.",
          :target_message=>"{{user}} delivers a blow to your back.",
          :spectator_message=>"{{user}} delivers a blow to {{target}}'s back.",
          :effects=>%{
            :damage=>0.02,
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
          :user_message=>"You deftly deliver a blow to {{target}}'s chest, drawing blood.",
          :target_message=>"{{user}} deftly delivers a blow to your chest, drawing blood.",
          :spectator_message=>"{{user}} deftly delivers a blow to {{target}}'s chest, drawing blood.",
          :effects=>%{
            :damage=>0.02,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-25,
                :duration=>4.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"You inflict a wound on {{target}}'s thigh.",
          :target_message=>"{{user}} inflicts a wound on your thigh.",
          :spectator_message=>"{{user}} inflicts a wound on {{target}}'s thigh.",
          :effects=>%{
            :damage=>0.03,
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>3.0
            }
          }
        },
        %{
          :user_message=>"You whack {{target}}'s forearm rather hard.",
          :target_message=>"{{user}} whacks your forearm rather hard.",
          :spectator_message=>"{{user}} whacks {{target}}'s forearm rather hard.",
          :effects=>%{
            :damage=>0.03,
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>3.0
            }
          }
        },
        %{
          :user_message=>"You shatter {{target}}'s arm!",
          :target_message=>"{{user}} shatters your arm!",
          :spectator_message=>"{{user}} shatters {{target}}'s arm!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"arm"
              }
            ],
            :damage=>0.09,
            :stun=>3.0
          }
        },
        %{
          :user_message=>"You slash {{target}}'s neck, nearly severing an artery!",
          :target_message=>"{{user}} slashes your neck, nearly severing an artery!",
          :spectator_message=>"{{user}} slashes {{target}}'s neck, nearly severing an artery!",
          :effects=>%{
            :damage=>0.06,
            :stun=>3.0,
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
          :user_message=>"You sever the tendons in {{target}}'s lower leg, causing {{target:him/her/it}} to scream in pain.",
          :target_message=>"{{user}} severs the tendons in your lower leg, causing you to scream in pain.",
          :spectator_message=>"{{user}} severs the tendons in {{target}}'s lower leg, causing {{target:him/her/it}} to scream in pain.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>6.0
            },
            :damage=>0.04,
            :stun=>2.0
          }
        },
        %{
          :user_message=>"You hit your foe in the back...not pleasing to {{target}}.",
          :target_message=>"{{user}} hits {{user:his/her/its}} foe in the back...not pleasing to you.",
          :spectator_message=>"{{user}} hits {{user:his/her/its}} foe in the back...not pleasing to {{target}}.",
          :effects=>%{
            :damage=>0.08,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-200,
                :duration=>4.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>6.0
            }
          }
        },
        %{
          :user_message=>"You place a neat cut along {{target}}'s nose.",
          :target_message=>"{{user}} places a neat cut along your nose.",
          :spectator_message=>"{{user}} places a neat cut along {{target}}'s nose.",
          :effects=>%{
            :damage=>0.02,
            :stun=>6.0,
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>4.0
            },
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
          :user_message=>"You slash {{target}}'s jugular vein, causing blood to spurt everywhere!",
          :target_message=>"{{user}} slashes your jugular vein, causing blood to spurt everywhere!",
          :spectator_message=>"{{user}} slashes {{target}}'s jugular vein, causing blood to spurt everywhere!",
          :effects=>%{
            :damage=>0.3,
            :stun=>4.0,
            :damage_over_time=>%{
              :damage=>0.05,
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
        }
      ],
      "B"=>[
        %{
          :user_message=>"You deliver a weak strike.",
          :target_message=>"{{user}} delivers a weak strike.",
          :spectator_message=>"{{user}} delivers a weak strike.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You deliver a glancing blow to {{target}}'s side.",
          :target_message=>"{{user}} delivers a glancing blow to your side.",
          :spectator_message=>"{{user}} delivers a glancing blow to {{target}}'s side.",
          :effects=>%{
            :damage=>0.03
          }
        },
        %{
          :user_message=>"You break one of {{target}}'s ribs.",
          :target_message=>"{{user}} breaks one of your ribs.",
          :spectator_message=>"{{user}} breaks one of {{target}}'s ribs.",
          :effects=>%{
            :damage=>0.03,
            :stun=>3.0
          }
        },
        %{
          :user_message=>"You hit {{target}} with a minor calf wound.",
          :target_message=>"{{user}} hits you with a minor calf wound.",
          :spectator_message=>"{{user}} hits {{target}} with a minor calf wound.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.01,
              :duration=>5.0
            },
            :damage=>0.02
          }
        },
        %{
          :user_message=>"You hit {{target}} square on the back.",
          :target_message=>"{{user}} hits you square on the back.",
          :spectator_message=>"{{user}} hits {{target}} square on the back.",
          :effects=>%{
            :damage=>0.04,
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
          :user_message=>"You hit {{target}}'s chest lightly, inflicting a minor wound",
          :target_message=>"{{user}} hits your chest lightly, inflicting a minor wound",
          :spectator_message=>"{{user}} hits {{target}}'s chest lightly, inflicting a minor wound",
          :effects=>%{
            :damage=>0.03,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-30,
                :duration=>4.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.01,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"You cut {{target}}'s forearm...it'll probably scar.",
          :target_message=>"{{user}} cuts your forearm...it'll probably scar.",
          :spectator_message=>"{{user}} cuts {{target}}'s forearm...it'll probably scar.",
          :effects=>%{
            :damage=>0.04,
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>4.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-30,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"You pound {{target}} squarely in the elbow, and a loud crack is heard.",
          :target_message=>"{{user}} pounds you squarely in the elbow, and a loud crack is heard.",
          :spectator_message=>"{{user}} pounds {{target}} squarely in the elbow, and a loud crack is heard.",
          :effects=>%{
            :damage=>0.08,
            :stun=>4.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-200,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"You slash into {{target}}'s calf, causing {{target:him/her/it}} to fall to the ground in pain.",
          :target_message=>"{{user}} slashes into your calf, causing you to fall to the ground in pain.",
          :spectator_message=>"{{user}} slashes into {{target}}'s calf, causing {{target:him/her/it}} to fall to the ground in pain.",
          :effects=>%{
            :stun=>3.0,
            :damage=>0.06,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-30,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} cries out as {{target:his/her/its}} shoulder is cut deeply by you.",
          :target_message=>"You cry out as your shoulder is cut deeply by {{user}}.",
          :spectator_message=>"{{target}} cries out as {{target:his/her/its}} shoulder is cut deeply by {{user}}.",
          :effects=>%{
            :damage=>0.06,
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
              :duration=>4.0
            }
          }
        },
        %{
          :user_message=>"You slash your foe in {{target:his/her/its}} side, causing a major wound.",
          :target_message=>"{{user}} slashes {{user:his/her/its}} foe in your side, causing a major wound.",
          :spectator_message=>"{{user}} slashes {{user:his/her/its}} foe in {{target:his/her/its}} side, causing a major wound.",
          :effects=>%{
            :damage=>0.07,
            :damage_over_time=>%{
              :damage=>0.06,
              :duration=>4.0
            },
            :stun=>2.0
          }
        },
        %{
          :user_message=>"You visciously pound {{target}} in the head, cracking {{target:his/her/its}} skull!",
          :target_message=>"{{user}} visciously pounds you in the head, cracking your skull!",
          :spectator_message=>"{{user}} visciously pounds {{target}} in the head, cracking {{target:his/her/its}} skull!",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.2,
              :duration=>4.0
            },
            :damage=>0.2
          }
        },
        %{
          :user_message=>"You completely disembowel {{target}}, spilling {{target:his/her/its}} entrails on the ground.",
          :target_message=>"{{user}} completely disembowels you, spilling your entrails on the ground.",
          :spectator_message=>"{{user}} completely disembowels {{target}}, spilling {{target:his/her/its}} entrails on the ground.",
          :effects=>%{
            :damage=>0.3,
            :damage_over_time=>%{
              :damage=>0.3,
              :duration=>2.0
            },
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
      "C"=>[
        %{
          :user_message=>"You lightly cut your foe.",
          :target_message=>"{{user}} lightly cuts {{user:his/her/its}} foe.",
          :spectator_message=>"{{user}} lightly cuts {{user:his/her/its}} foe.",
          :effects=>%{
            :damage=>0.01
          }
        },
        %{
          :user_message=>"You deliver a nice cut to {{target}}'s arm.",
          :target_message=>"{{user}} delivers a nice cut to your arm.",
          :spectator_message=>"{{user}} delivers a nice cut to {{target}}'s arm.",
          :effects=>%{
            :damage=>0.03
          }
        },
        %{
          :user_message=>"You deliver a clean blow to {{target}}'s side.",
          :target_message=>"{{user}} delivers a clean blow to your side.",
          :spectator_message=>"{{user}} delivers a clean blow to {{target}}'s side.",
          :effects=>%{
            :damage=>0.04,
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
          :user_message=>"You cut {{target}}'s leg deeply.",
          :target_message=>"{{user}} cuts your leg deeply.",
          :spectator_message=>"{{user}} cuts {{target}}'s leg deeply.",
          :effects=>%{
            :damage=>0.02,
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>4.0
            }
          }
        },
        %{
          :user_message=>"You hit {{target}} on the back quite hard.",
          :target_message=>"{{user}} hits you on the back quite hard.",
          :spectator_message=>"{{user}} hits {{target}} on the back quite hard.",
          :effects=>%{
            :stun=>1.0,
            :damage=>0.03,
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
          :user_message=>"You inflict a minor wound on {{target}}'s thigh.",
          :target_message=>"{{user}} inflicts a minor wound on your thigh.",
          :spectator_message=>"{{user}} inflicts a minor wound on {{target}}'s thigh.",
          :effects=>%{
            :damage=>0.05,
            :stun=>1.0,
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"You carve a rather nasty wound on {{target}}'s forearm.",
          :target_message=>"{{user}} carves a rather nasty wound on your forearm.",
          :spectator_message=>"{{user}} carves a rather nasty wound on {{target}}'s forearm.",
          :effects=>%{
            :damage=>0.04,
            :stun=>1.0,
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>4.0
            },
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
          :user_message=>"You shatter {{target}}'s knee!",
          :target_message=>"{{user}} shatters your knee!",
          :spectator_message=>"{{user}} shatters {{target}}'s knee!",
          :effects=>%{
            :damage=>0.06,
            :stun=>3.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-90,
                :duration=>20.0
              },
              %{
                :skill=>"parry",
                :amount=>-200,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You slash the muscles and tendons in {{target}}'s leg.",
          :target_message=>"{{user}} slashes the muscles and tendons in your leg.",
          :spectator_message=>"{{user}} slashes the muscles and tendons in {{target}}'s leg.",
          :effects=>%{
            :stun=>2.0,
            :damage=>0.07,
            :damage_over_time=>%{
              :damage=>0.04,
              :duration=>4.0
            }
          }
        },
        %{
          :user_message=>"You puree the muscles and tendons in {{target}}'s shoulder.",
          :target_message=>"{{user}} purees the muscles and tendons in your shoulder.",
          :spectator_message=>"{{user}} purees the muscles and tendons in {{target}}'s shoulder.",
          :effects=>%{
            :damage=>0.09,
            :damage_over_time=>%{
              :damage=>0.04,
              :duration=>4.0
            },
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"arm"
              }
            ]
          }
        },
        %{
          :user_message=>"You deliver a mighty whack to {{target}}'s back, causing nasty cracking sounds and shit.",
          :target_message=>"{{user}} delivers a mighty whack to your back, causing nasty cracking sounds and shit.",
          :spectator_message=>"{{user}} delivers a mighty whack to {{target}}'s back, causing nasty cracking sounds and shit.",
          :effects=>%{
            :stun=>4.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-200,
                :duration=>8.0
              }
            ],
            :damage=>0.09
          }
        },
        %{
          :user_message=>"You completely sever {{target}}'s arm!!!",
          :target_message=>"{{user}} completely severs your arm!!!",
          :spectator_message=>"{{user}} completely severs {{target}}'s arm!!!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"arm"
              }
            ],
            :damage=>0.18
          }
        },
        %{
          :user_message=>"You stab {{target}} right in the eyes...ewwwwwww.",
          :target_message=>"{{user}} stabs you right in the eyes...ewwwwwww.",
          :spectator_message=>"{{user}} stabs {{target}} right in the eyes...ewwwwwww.",
          :effects=>%{
            :damage=>0.05,
            :stun=>30.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-200,
                :duration=>30.0
              }
            ]
          }
        }
      ],
      "D"=>[
        %{
          :user_message=>"You lightly cut your foe.",
          :target_message=>"{{user}} lightly cuts {{user:his/her/its}} foe.",
          :spectator_message=>"{{user}} lightly cuts {{user:his/her/its}} foe.",
          :effects=>%{
            :damage=>0.02
          }
        },
        %{
          :user_message=>"You deliver a nice cut to {{target}}'s arm.",
          :target_message=>"{{user}} delivers a nice cut to your arm.",
          :spectator_message=>"{{user}} delivers a nice cut to {{target}}'s arm.",
          :effects=>%{
            :damage=>0.04
          }
        },
        %{
          :user_message=>"You stab {{target}} in the side.",
          :target_message=>"{{user}} stabs you in the side.",
          :spectator_message=>"{{user}} stabs {{target}} in the side.",
          :effects=>%{
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-10,
                :duration=>2.0
              }
            ],
            :damage=>0.02
          }
        },
        %{
          :user_message=>"You slash {{target}}'s upper leg, smiling.",
          :target_message=>"{{user}} slashes your upper leg, smiling.",
          :spectator_message=>"{{user}} slashes {{target}}'s upper leg, smiling.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>5.0
            },
            :damage=>0.03
          }
        },
        %{
          :user_message=>"You slash {{target}} in the lower back, opening a sizeable wound.",
          :target_message=>"{{user}} slashes you in the lower back, opening a sizeable wound.",
          :spectator_message=>"{{user}} slashes {{target}} in the lower back, opening a sizeable wound.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>5.0
            },
            :damage=>0.03
          }
        },
        %{
          :user_message=>"You inflict a moderate thigh wound on {{target}}.",
          :target_message=>"{{user}} inflicts a moderate thigh wound on you.",
          :spectator_message=>"{{user}} inflicts a moderate thigh wound on {{target}}.",
          :effects=>%{
            :damage=>0.06,
            :stun=>2.0,
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"{{target}} receives a nasy wound on his forearm courtesy of you.",
          :target_message=>"You receive a nasy wound on his forearm courtesy of {{user}}.",
          :spectator_message=>"{{target}} receives a nasy wound on his forearm courtesy of {{user}}.",
          :effects=>%{
            :damage=>0.04,
            :stun=>2.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-10,
                :duration=>4.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"You whack {{target}} harshly over the head.",
          :target_message=>"{{user}} whacks you harshly over the head.",
          :spectator_message=>"{{user}} whacks {{target}} harshly over the head.",
          :effects=>%{
            :damage=>0.15,
            :stun=>10.0
          }
        },
        %{
          :user_message=>"You snap the tendons in {{target}}'s leg.",
          :target_message=>"{{user}} snaps the tendons in your leg.",
          :spectator_message=>"{{user}} snaps the tendons in {{target}}'s leg.",
          :effects=>%{
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-200,
                :duration=>4.0
              },
              %{
                :skill=>"attack",
                :amount=>-50,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"You drive his attack deep into {{target}}'s arm.",
          :target_message=>"{{user}} drives his attack deep into your arm.",
          :spectator_message=>"{{user}} drives his attack deep into {{target}}'s arm.",
          :effects=>%{
            :stun=>4.0,
            :damage=>0.1,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-200,
                :duration=>4.0
              }
            ]
          }
        },
        %{
          :user_message=>"You lop off {{target}}'s hand, blood squirting out of the stump.",
          :target_message=>"{{user}} lops off your hand, blood squirting out of the stump.",
          :spectator_message=>"{{user}} lops off {{target}}'s hand, blood squirting out of the stump.",
          :effects=>%{
            :damage=>0.06,
            :stun=>6.0,
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"hand"
              }
            ]
          }
        },
        %{
          :user_message=>"You slice almost completely through {{target}}, your attack buried deeply in {{target:his/her/its}} side.",
          :target_message=>"{{user}} slice almost completely through you, {{user:his/her/its}} attack buried deeply in your side.",
          :spectator_message=>"{{user}} slice almost completely through {{target}}, {{user:his/her/its}} attack buried deeply in {{target:his/her/its}} side.",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"You impale {{target}} straight through the heart!!!",
          :target_message=>"{{user}} impales you straight through the heart!!!",
          :spectator_message=>"{{user}} impales {{target}} straight through the heart!!!",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "E"=>[
        %{
          :user_message=>"You lightly cut your foe.",
          :target_message=>"{{user}} lightly cuts {{user:his/her/its}} foe.",
          :spectator_message=>"{{user}} lightly cuts {{user:his/her/its}} foe.",
          :effects=>%{
            :damage=>0.03
          }
        },
        %{
          :user_message=>"You deliver a nice cut to {{target}}'s arm.",
          :target_message=>"{{user}} delivers a nice cut to your arm.",
          :spectator_message=>"{{user}} delivers a nice cut to {{target}}'s arm.",
          :effects=>%{
            :damage=>0.05
          }
        },
        %{
          :user_message=>"{{target}} gets stunned by a nice whack from you",
          :target_message=>"You get stunned by a nice whack from {{user}}",
          :spectator_message=>"{{target}} gets stunned by a nice whack from {{user}}",
          :effects=>%{
            :stun=>1.0
          }
        },
        %{
          :user_message=>"You deliver a blow to {{target}}'s upper leg.",
          :target_message=>"{{user}} delivers a blow to your upper leg.",
          :spectator_message=>"{{user}} delivers a blow to {{target}}'s upper leg.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>6.0
            },
            :damage=>0.3
          }
        },
        %{
          :user_message=>"You deliver a mighty blow to {{target}}'s lower back.",
          :target_message=>"{{user}} delivers a mighty blow to your lower back.",
          :spectator_message=>"{{user}} delivers a mighty blow to {{target}}'s lower back.",
          :effects=>%{
            :damage=>0.04,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>5.0
            },
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
          :user_message=>"You cut deeply into {{target}}'s thigh.",
          :target_message=>"{{user}} cuts deeply into your thigh.",
          :spectator_message=>"{{user}} cuts deeply into {{target}}'s thigh.",
          :effects=>%{
            :damage=>0.08,
            :stun=>2.0,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>4.0
            }
          }
        },
        %{
          :user_message=>"You slice {{target}}'s forearm.",
          :target_message=>"{{user}} slice your forearm.",
          :spectator_message=>"{{user}} slice {{target}}'s forearm.",
          :effects=>%{
            :damage=>0.06,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>6.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-15,
                :duration=>2.0
              }
            ]
          }
        },
        %{
          :user_message=>"You sever your foe's arm mercilessly!",
          :target_message=>"{{user}} severs {{user:his/her/its}} foe's arm mercilessly!",
          :spectator_message=>"{{user}} severs {{user:his/her/its}} foe's arm mercilessly!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"arm"
              }
            ],
            :damage=>0.12
          }
        },
        %{
          :user_message=>"You cut {{target}}'s leg, snapping muscles and tendons o'plenty.",
          :target_message=>"{{user}} cuts your leg, snapping muscles and tendons o'plenty.",
          :spectator_message=>"{{user}} cuts {{target}}'s leg, snapping muscles and tendons o'plenty.",
          :effects=>%{
            :damage=>0.12,
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-200,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You sever {{target}}'s hand, leaving {{target:him/her/it}} screaming in pain.",
          :target_message=>"{{user}} severs your hand, leaving you screaming in pain.",
          :spectator_message=>"{{user}} severs {{target}}'s hand, leaving {{target:him/her/it}} screaming in pain.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"hand"
              }
            ],
            :damage=>0.05
          }
        },
        %{
          :user_message=>"You lop off {{target}}'s leg in a fluid motion!",
          :target_message=>"{{user}} lops off your leg in a fluid motion!",
          :spectator_message=>"{{user}} lops off {{target}}'s leg in a fluid motion!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"leg"
              }
            ],
            :damage=>0.15,
            :stun=>9.0
          }
        },
        %{
          :user_message=>"Your attack slash through {{target}}'s side, snapping a wide variety of tendons and muscles.",
          :target_message=>"{{user}}'s attack slashes through your side, snapping a wide variety of tendons and muscles.",
          :spectator_message=>"{{user}}'s attack slashes through {{target}}'s side, snapping a wide variety of tendons and muscles.",
          :effects=>%{
            :damage=>0.3,
            :stun=>10.0,
            :damage_over_time=>%{
              :damage=>0.08,
              :duration=>4.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-50,
                :duration=>16.0
              }
            ]
          }
        },
        %{
          :user_message=>"You swing your attack through the air, sending {{target}}'s head flying!",
          :target_message=>"{{user}} swings {{user:his/her/its}} attack through the air, sending your head flying!",
          :spectator_message=>"{{user}} swings {{user:his/her/its}} attack through the air, sending {{target}}'s head flying!",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"You violently dismember {{target}}!  Nasty!",
          :target_message=>"{{user}} violently dismembers you!  Nasty!",
          :spectator_message=>"{{user}} violently dismembers {{target}}!  Nasty!",
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
      ]
    }
  end

end
