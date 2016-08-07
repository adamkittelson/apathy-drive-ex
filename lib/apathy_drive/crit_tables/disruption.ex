defmodule ApathyDrive.CritTables.Disruption do

  def name do
    "disruption"
  end

  def damage_type do
    "magical"
  end

  def crits do
    %{
      "A"=>[
        %{
          :user_message=>"A near miss.",
          :target_message=>"A near miss.",
          :spectator_message=>"A near miss.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"You disrupt {{target}} mildly witth your attack.",
          :target_message=>"{{user}} disrupts you mildly witth {{user:his/her/its}} attack.",
          :spectator_message=>"{{user}} disrupts {{target}} mildly witth {{user:his/her/its}} attack.",
          :effects=>%{
            :damage=>0.03
          }
        },
        %{
          :user_message=>"Your disrupting blast stuns {{target}}, forcing {{target:him/her/it}} to go on the defensive.",
          :target_message=>"{{user}}'s disrupting blast stuns you, forcing you to go on the defensive.",
          :spectator_message=>"{{user}}'s disrupting blast stuns {{target}}, forcing {{target:him/her/it}} to go on the defensive.",
          :effects=>%{
            :damage=>0.06,
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
          :user_message=>"Your disrupting blast knocks {{target}} down.",
          :target_message=>"{{user}}'s disrupting blast knocks you down.",
          :spectator_message=>"{{user}}'s disrupting blast knocks {{target}} down.",
          :effects=>%{
            :damage=>0.12,
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
          :user_message=>"Your disrupting plast bursts the muscle in {{target}}'s leg!",
          :target_message=>"{{user}}'s disrupting plast bursts the muscle in your leg!",
          :spectator_message=>"{{user}}'s disrupting plast bursts the muscle in {{target}}'s leg!",
          :effects=>%{
            :stun=>1.0,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>10.0
            },
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>10.0
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
          :user_message=>"You inflict a minor wound on {{target}}'s back with a disrupting attack.",
          :target_message=>"{{user}} inflicts a minor wound on your back with a disrupting attack.",
          :spectator_message=>"{{user}} inflicts a minor wound on {{target}}'s back with a disrupting attack.",
          :effects=>%{
            :stun=>2.0,
            :damage=>0.15
          }
        },
        %{
          :user_message=>"You disrupt {{target}}'s thigh with your attack.",
          :target_message=>"{{user}} disrupts your thigh with {{user:his/her/its}} attack.",
          :spectator_message=>"{{user}} disrupts {{target}}'s thigh with {{user:his/her/its}} attack.",
          :effects=>%{
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>6.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.09,
              :duration=>4.0
            }
          }
        },
        %{
          :user_message=>"You blast {{target}}'s forearm, causing blood vessels to rupture.",
          :target_message=>"{{user}} blasts your forearm, causing blood vessels to rupture.",
          :spectator_message=>"{{user}} blasts {{target}}'s forearm, causing blood vessels to rupture.",
          :effects=>%{
            :damage=>0.15,
            :damage_over_time=>%{
              :damage=>0.09,
              :duration=>4.0
            },
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
          :user_message=>"{{target}}'s arm biursts from the inside as a result of your disrupting attack. It is useless!",
          :target_message=>"Your arm biursts from the inside as a result of {{user}}'s disrupting attack. It is useless!",
          :spectator_message=>"{{target}}'s arm biursts from the inside as a result of {{user}}'s disrupting attack. It is useless!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :damage=>0.3,
            :damage_over_time=>%{
              :damage=>0.15,
              :duration=>3.0
            },
            :stun=>6.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>12.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your disrupting attack shatters {{target}}'s collar bone!",
          :target_message=>"{{user}}'s disrupting attack shatters your collar bone!",
          :spectator_message=>"{{user}}'s disrupting attack shatters {{target}}'s collar bone!",
          :effects=>%{
            :damage=>0.18,
            :stun=>4.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>4.0
              },
              %{
                :skill=>"attack",
                :amount=>-40,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}}'s bicep is disrupted by your attack.",
          :target_message=>"Your bicep is disrupted by {{user}}'s attack.",
          :spectator_message=>"{{target}}'s bicep is disrupted by {{user}}'s attack.",
          :effects=>%{
            :damage=>0.21,
            :stun=>4.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-60,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your disrupting blast rips into {{target}}'s back.",
          :target_message=>"{{user}}'s disrupting blast rips into your back.",
          :spectator_message=>"{{user}}'s disrupting blast rips into {{target}}'s back.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.08,
              :duration=>4.0
            },
            :stun=>5.0,
            :damage=>0.24
          }
        },
        %{
          :user_message=>"{{target}}'s ear is blown completely off by your attack!",
          :target_message=>"Your ear is blown completely off by {{user}}'s attack!",
          :spectator_message=>"{{target}}'s ear is blown completely off by {{user}}'s attack!",
          :effects=>%{
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-25,
                :duration=>20.0
              }
            ],
            :stun=>10.0,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"Your disrupting attack strikes {{target}} squarely in the middle of the face!",
          :target_message=>"{{user}}'s disrupting attack strikes you squarely in the middle of the face!",
          :spectator_message=>"{{user}}'s disrupting attack strikes {{target}} squarely in the middle of the face!",
          :effects=>%{
            :stun=>1.2,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>24.0
              },
              %{
                :skill=>"block",
                :amount=>-100,
                :duration=>24.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"You disrupt {{target}}'s neck with your attack, severely wounding {{target:him/her/it}}.",
          :target_message=>"{{user}} disrupts your neck with {{user:his/her/its}} attack, severely wounding you.",
          :spectator_message=>"{{user}} disrupts {{target}}'s neck with {{user:his/her/its}} attack, severely wounding {{target:him/her/it}}.",
          :effects=>%{
            :damage=>0.5,
            :stun=>10.0
          }
        }
      ],
      "B"=>[
        %{
          :user_message=>"A glancing blow.",
          :target_message=>"A glancing blow.",
          :spectator_message=>"A glancing blow.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"Your disrupting blast opens a minor wound on {{target}}'s torso.",
          :target_message=>"{{user}}'s disrupting blast opens a minor wound on your torso.",
          :spectator_message=>"{{user}}'s disrupting blast opens a minor wound on {{target}}'s torso.",
          :effects=>%{
            :damage=>0.05
          }
        },
        %{
          :user_message=>"Your disrupting attack frimly strikes {{target}}'s side.",
          :target_message=>"{{user}}'s disrupting attack frimly strikes your side.",
          :spectator_message=>"{{user}}'s disrupting attack frimly strikes {{target}}'s side.",
          :effects=>%{
            :damage=>0.12,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"{{target}} is stunned and disoriented by your disrupting attack.",
          :target_message=>"You are stunned and disoriented by {{user}}'s disrupting attack.",
          :spectator_message=>"{{target}} is stunned and disoriented by {{user}}'s disrupting attack.",
          :effects=>%{
            :stun=>3.0,
            :damage=>0.15,
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"You disrupt {{target}}'s lower leg severely.",
          :target_message=>"{{user}} disrupts your lower leg severely.",
          :spectator_message=>"{{user}} disrupts {{target}}'s lower leg severely.",
          :effects=>%{
            :damage=>0.18,
            :damage_over_time=>%{
              :damage=>0.08,
              :duration=>3.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-80,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your disrupting blast opens wounds on {{target}}'s back.",
          :target_message=>"{{user}}'s disrupting blast opens wounds on your back.",
          :spectator_message=>"{{user}}'s disrupting blast opens wounds on {{target}}'s back.",
          :effects=>%{
            :stun=>4.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>8.0
              },
              %{
                :skill=>"block",
                :amount=>-100,
                :duration=>8.0
              }
            ],
            :damage=>0.21,
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>4.0
            }
          }
        },
        %{
          :user_message=>"Energy dissipation from your attack shreds {{target}}'s thigh muscle!",
          :target_message=>"Energy dissipation from {{user}}'s attack shreds your thigh muscle!",
          :spectator_message=>"Energy dissipation from {{user}}'s attack shreds {{target}}'s thigh muscle!",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.08,
              :duration=>4.0
            },
            :damage=>0.27,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-80,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your blast to {{target}}'s forearm renders {{target:his/her/its}} hand useless.",
          :target_message=>"{{user}}'s blast to your forearm renders your hand useless.",
          :spectator_message=>"{{user}}'s blast to {{target}}'s forearm renders {{target:his/her/its}} hand useless.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"hand"
              }
            ],
            :damage=>0.27,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"The joint in {{target}}'s elbow is severely disrupted by your attack. It is rendered useless!",
          :target_message=>"The joint in your elbow is severely disrupted by {{user}}'s attack. It is rendered useless!",
          :spectator_message=>"The joint in {{target}}'s elbow is severely disrupted by {{user}}'s attack. It is rendered useless!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :stun=>10.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>20.0
              },
              %{
                :skill=>"block",
                :amount=>-100,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"You deliver a severe blow to {{target}}'s neck.",
          :target_message=>"{{user}} delivers a severe blow to your neck.",
          :spectator_message=>"{{user}} delivers a severe blow to {{target}}'s neck.",
          :effects=>%{
            :stun=>6.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>6.0
              },
              %{
                :skill=>"block",
                :amount=>-100,
                :duration=>6.0
              },
              %{
                :skill=>"attack",
                :amount=>-30,
                :duration=>6.0
              }
            ],
            :damage=>0.3
          }
        },
        %{
          :user_message=>"{{target}}'s arm is severely disrupted by your attack. The bone shatters and it is useless.",
          :target_message=>"Your arm is severely disrupted by {{user}}'s attack. The bone shatters and it is useless.",
          :spectator_message=>"{{target}}'s arm is severely disrupted by {{user}}'s attack. The bone shatters and it is useless.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :stun=>7.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>14.0
              },
              %{
                :skill=>"block",
                :amount=>-100,
                :duration=>14.0
              },
              %{
                :skill=>"attack",
                :amount=>-70,
                :duration=>14.0
              }
            ]
          }
        },
        %{
          :user_message=>"You deliver a viscious strike to the back of {{target}}'s head, knocking {{target:him/her/it}} flat!",
          :target_message=>"{{user}} delivers a viscious strike to the back of your head, knocking you flat!",
          :spectator_message=>"{{user}} delivers a viscious strike to the back of {{target}}'s head, knocking {{target:him/her/it}} flat!",
          :effects=>%{
            :stun=>15.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>30.0
              }
            ],
            :damage=>0.35,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"A blast from your attack disrupts {{target}}'s hip, shattering the bone.",
          :target_message=>"A blast from {{user}}'s attack disrupts your hip, shattering the bone.",
          :spectator_message=>"A blast from {{user}}'s attack disrupts {{target}}'s hip, shattering the bone.",
          :effects=>%{
            :stun=>10.0,
            :damage=>0.45,
            :damage_over_time=>%{
              :damage=>0.04,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"A blast from your to {{target}}'s head causes {{target:his/her/its}} eyes to shoot speedily from their sockets!",
          :target_message=>"A blast from {{user}}'s to your head causes your eyes to shoot speedily from their sockets!",
          :spectator_message=>"A blast from {{user}}'s to {{target}}'s head causes {{target:his/her/its}} eyes to shoot speedily from their sockets!",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>4.0
            }
          }
        },
        %{
          :user_message=>"Your disrupting blast strikes {{target}} in the head, liquefying {{target}}'s brain in an instant.",
          :target_message=>"{{user}}'s disrupting blast strikes you in the head, liquefying your brain in an instant.",
          :spectator_message=>"{{user}}'s disrupting blast strikes {{target}} in the head, liquefying {{target}}'s brain in an instant.",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "C"=>[
        %{
          :user_message=>"A minor wound appears on {{target}}.",
          :target_message=>"A minor wound appears on you.",
          :spectator_message=>"A minor wound appears on {{target}}.",
          :effects=>%{
            :damage=>0.03
          }
        },
        %{
          :user_message=>"Your disrupting attack wounds {{target}}.",
          :target_message=>"{{user}}'s disrupting attack wounds you.",
          :spectator_message=>"{{user}}'s disrupting attack wounds {{target}}.",
          :effects=>%{
            :damage=>0.06
          }
        },
        %{
          :user_message=>"You deliver a fierce blow to {{target}}'s side, stunning {{target:him/her/it}}.",
          :target_message=>"{{user}} delivers a fierce blow to your side, stunning you.",
          :spectator_message=>"{{user}} delivers a fierce blow to {{target}}'s side, stunning {{target:him/her/it}}.",
          :effects=>%{
            :damage=>0.08,
            :damage_over_time=>%{
              :damage=>0.04,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"Your disrupting strike hits {{target}} along the chest.",
          :target_message=>"{{user}}'s disrupting strike hits you along the chest.",
          :spectator_message=>"{{user}}'s disrupting strike hits {{target}} along the chest.",
          :effects=>%{
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>4.0
              },
              %{
                :skill=>"block",
                :amount=>-100,
                :duration=>4.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.06,
              :duration=>4.0
            },
            :damage=>0.18
          }
        },
        %{
          :user_message=>"Your disrupting attack inflicts a minor wound on {{target}}'s leg.",
          :target_message=>"{{user}}'s disrupting attack inflicts a minor wound on your leg.",
          :spectator_message=>"{{user}}'s disrupting attack inflicts a minor wound on {{target}}'s leg.",
          :effects=>%{
            :stun=>4.0,
            :damage_over_time=>%{
              :damage=>0.08,
              :duration=>4.0
            }
          }
        },
        %{
          :user_message=>"Your disrupting blast tears across {{target}}'s back, tearing the skin!",
          :target_message=>"{{user}}'s disrupting blast tears across your back, tearing the skin!",
          :spectator_message=>"{{user}}'s disrupting blast tears across {{target}}'s back, tearing the skin!",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.06,
              :duration=>5.0
            },
            :stun=>5.0
          }
        },
        %{
          :user_message=>"You scream a fierce pulverizing blast to {{target}}'s upper thigh. {{target}} screams in pain.",
          :target_message=>"{{user}} screams a fierce pulverizing blast to your upper thigh. You scream in pain.",
          :spectator_message=>"{{user}} screams a fierce pulverizing blast to {{target}}'s upper thigh. {{target}} screams in pain.",
          :effects=>%{
            :damage=>0.22,
            :stun=>5.0,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-35,
                :duration=>10.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>4.0
            }
          }
        },
        %{
          :user_message=>"Your strike rips into {{target}}'s forearm, rendering the arm useless!",
          :target_message=>"{{user}}'s strike rips into your forearm, rendering the arm useless!",
          :spectator_message=>"{{user}}'s strike rips into {{target}}'s forearm, rendering the arm useless!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :damage=>0.24,
            :damage_over_time=>%{
              :damage=>0.08,
              :duration=>5.0
            },
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-20,
                :duration=>14.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your disrupting blast destroys {{target}}'s knee. {{target}} collapses to the ground!",
          :target_message=>"{{user}}'s disrupting blast destroys your knee. You collapse to the ground!",
          :spectator_message=>"{{user}}'s disrupting blast destroys {{target}}'s knee. {{target}} collapses to the ground!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"leg"
              }
            ],
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>5.0
            },
            :damage=>0.5,
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
          :user_message=>"You deliver a disrupting strike to {{target}}'s collar, choking {{target:him/her/it}}!",
          :target_message=>"{{user}} delivers a disrupting strike to your collar, choking you!",
          :spectator_message=>"{{user}} delivers a disrupting strike to {{target}}'s collar, choking {{target:him/her/it}}!",
          :effects=>%{
            :stun=>12.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>24.0
              }
            ],
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-40,
                :duration=>48.0
              }
            ],
            :damage=>0.26
          }
        },
        %{
          :user_message=>"You disrupt {{target}}'s arm, fracturing bone and destroying muscle!",
          :target_message=>"{{user}} disrupts your arm, fracturing bone and destroying muscle!",
          :spectator_message=>"{{user}} disrupts {{target}}'s arm, fracturing bone and destroying muscle!",
          :effects=>%{
            :stun=>12.0,
            :damage_over_time=>%{
              :damage=>0.08,
              :duration=>4.0
            },
            :damage=>0.28
          }
        },
        %{
          :user_message=>"{{target}}'s brain is severely wracked by your disrupting attack.",
          :target_message=>"Your brain is severely wracked by {{user}}'s disrupting attack.",
          :spectator_message=>"{{target}}'s brain is severely wracked by {{user}}'s disrupting attack.",
          :effects=>%{
            :stun=>12.0,
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>4.0
            },
            :damage=>0.3,
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-35,
                :duration=>24.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} gets all of the wind knocked out of {{target:him/her/it}} by your attack, and several of {{target:his/her/its}} ribs are cracked in the process",
          :target_message=>"You get all of the wind knocked out of you by {{user}}'s attack, and several of your ribs are cracked in the process",
          :spectator_message=>"{{target}} gets all of the wind knocked out of {{target:him/her/it}} by {{user}}'s attack, and several of {{target:his/her/its}} ribs are cracked in the process",
          :effects=>%{
            :damage=>0.33,
            :damage_over_time=>%{
              :damage=>0.06,
              :duration=>8.0
            },
            :stun=>3.0
          }
        },
        %{
          :user_message=>"Your disruptive attack strikes {{target}} in the torso, inflicting massive damage!",
          :target_message=>"{{user}}'s disruptive attack strikes you in the torso, inflicting massive damage!",
          :spectator_message=>"{{user}}'s disruptive attack strikes {{target}} in the torso, inflicting massive damage!",
          :effects=>%{
            :damage=>0.6,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>10.0
            },
            :stun=>4.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>8.0
              },
              %{
                :skill=>"dodge",
                :amount=>-60,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}}'s head explodes. {{target:he/she/it}} falls to the ground, a lifeless husk.",
          :target_message=>"Your head explodes. You fall to the ground, a lifeless husk.",
          :spectator_message=>"{{target}}'s head explodes. {{target:he/she/it}} falls to the ground, a lifeless husk.",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "D"=>[
        %{
          :user_message=>"Your blast disrupts {{target}}.",
          :target_message=>"{{user}}'s blast disrupts you.",
          :spectator_message=>"{{user}}'s blast disrupts {{target}}.",
          :effects=>%{
            :damage=>0.05
          }
        },
        %{
          :user_message=>"Your blast severely disrupts {{target}}",
          :target_message=>"{{user}}'s blast severely disrupts you",
          :spectator_message=>"{{user}}'s blast severely disrupts {{target}}",
          :effects=>%{
            :damage=>0.09
          }
        },
        %{
          :user_message=>"Your disrupting attack unbalanced {{target}}.",
          :target_message=>"{{user}}'s disrupting attack unbalanced you.",
          :spectator_message=>"{{user}}'s disrupting attack unbalanced {{target}}.",
          :effects=>%{
            :stun=>1.0,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>3.0
            }
          }
        },
        %{
          :user_message=>"Your attack disrupts {{target}}'s upper leg.",
          :target_message=>"{{user}}'s attack disrupts your upper leg.",
          :spectator_message=>"{{user}}'s attack disrupts {{target}}'s upper leg.",
          :effects=>%{
            :damage=>0.15,
            :stun=>3.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"You inflict a wound to {{target}}'s groin!  I hate it when that happens!",
          :target_message=>"{{user}} inflicts a wound to your groin!  I hate it when that happens!",
          :spectator_message=>"{{user}} inflicts a wound to {{target}}'s groin!  I hate it when that happens!",
          :effects=>%{
            :stun=>7.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>8.0
              },
              %{
                :skill=>"block",
                :amount=>-100,
                :duration=>8.0
              }
            ],
            :damage=>0.15
          }
        },
        %{
          :user_message=>"You disrupt {{target}}'s lower back, temporarily paralyzing {{target:him/her/it}}.",
          :target_message=>"{{user}} disrupts your lower back, temporarily paralyzing you.",
          :spectator_message=>"{{user}} disrupts {{target}}'s lower back, temporarily paralyzing {{target:him/her/it}}.",
          :effects=>%{
            :stun=>20.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>40.0
              },
              %{
                :skill=>"dodge",
                :amount=>-100,
                :duration=>40.0
              },
              %{
                :skill=>"block",
                :amount=>-100,
                :duration=>40.0
              }
            ],
            :damage=>0.2
          }
        },
        %{
          :user_message=>"Your disrupting attack causes a blast in {{target}}'s upper leg.",
          :target_message=>"{{user}}'s disrupting attack causes a blast in your upper leg.",
          :spectator_message=>"{{user}}'s disrupting attack causes a blast in {{target}}'s upper leg.",
          :effects=>%{
            :stat_mod=>[
              %{
                :stat=>"dexterity",
                :amount=>-45,
                :duration=>12.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.07,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"{{target}}'s forearm is destroyed!",
          :target_message=>"Your forearm is destroyed!",
          :spectator_message=>"{{target}}'s forearm is destroyed!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"arm"
              }
            ],
            :damage_over_time=>%{
              :damage=>0.07,
              :duration=>5.0
            },
            :stun=>5.0
          }
        },
        %{
          :user_message=>"You deliver a mighty disruptive blast to {{target}}'s face, destroying {{target:his/her/its}} eyes, ears, and throat.",
          :target_message=>"{{user}} delivers a mighty disruptive blast to your face, destroying your eyes, ears, and throat.",
          :spectator_message=>"{{user}} delivers a mighty disruptive blast to {{target}}'s face, destroying {{target:his/her/its}} eyes, ears, and throat.",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"Your blast causes major cellular disruption in {{target}}'s shoulder.",
          :target_message=>"{{user}}'s blast causes major cellular disruption in your shoulder.",
          :spectator_message=>"{{user}}'s blast causes major cellular disruption in {{target}}'s shoulder.",
          :effects=>%{
            :damage=>0.2,
            :stun=>6.0,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-30,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}}'s arm explodes messily, leaving blood and flesh strewn about!",
          :target_message=>"Your arm explodes messily, leaving blood and flesh strewn about!",
          :spectator_message=>"{{target}}'s arm explodes messily, leaving blood and flesh strewn about!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"arm"
              }
            ],
            :stun=>18.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>36.0
              }
            ],
            :damage=>0.25,
            :damage_over_time=>%{
              :damage=>0.06,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"Your blast slams into {{target}}'s side, destroying a kidney!",
          :target_message=>"{{user}}'s blast slams into your side, destroying a kidney!",
          :spectator_message=>"{{user}}'s blast slams into {{target}}'s side, destroying a kidney!",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>9.0
            },
            :stun=>2.0
          }
        },
        %{
          :user_message=>"Your blast compresses {{target}}'s chest effortlessly, causing {{target:him/her/it}} to spit up copious amounts of blood.",
          :target_message=>"{{user}}'s blast compresses your chest effortlessly, causing you to spit up copious amounts of blood.",
          :spectator_message=>"{{user}}'s blast compresses {{target}}'s chest effortlessly, causing {{target:him/her/it}} to spit up copious amounts of blood.",
          :effects=>%{
            :stun=>6.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>12.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>10.0
            }
          }
        },
        %{
          :user_message=>"Your strike guts through {{target}}, disembowling {{target:him/her/it}}!",
          :target_message=>"{{user}}'s strike guts through you, disembowling you!",
          :spectator_message=>"{{user}}'s strike guts through {{target}}, disembowling {{target:him/her/it}}!",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"Your attack strikes {{target}} firmly in the head. {{target:he/she/it}} hesitates momentarily, then {{target:his/her/its}} head explodes in a spectacular display!",
          :target_message=>"{{user}}'s attack strikes you firmly in the head. You hesitate momentarily, then your head explodes in a spectacular display!",
          :spectator_message=>"{{user}}'s attack strikes {{target}} firmly in the head. {{target:he/she/it}} hesitates momentarily, then {{target:his/her/its}} head explodes in a spectacular display!",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "E"=>[
        %{
          :user_message=>"Your attack slightly disrupts {{target}}.",
          :target_message=>"{{user}}'s attack slightly disrupts you.",
          :spectator_message=>"{{user}}'s attack slightly disrupts {{target}}.",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.02,
              :duration=>5.0
            },
            :stun=>3.0
          }
        },
        %{
          :user_message=>"Your attack disrupts {{target}}'s side.",
          :target_message=>"{{user}}'s attack disrupts your side.",
          :spectator_message=>"{{user}}'s attack disrupts {{target}}'s side.",
          :effects=>%{
            :damage=>0.1,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>5.0
            },
            :stun=>4.0
          }
        },
        %{
          :user_message=>"Your blow to {{target}}'s leg shatters bone and muscle.",
          :target_message=>"{{user}}'s blow to your leg shatters bone and muscle.",
          :spectator_message=>"{{user}}'s blow to {{target}}'s leg shatters bone and muscle.",
          :effects=>%{
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-35,
                :duration=>10.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.04,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"You disrupt {{target}}'s lower back.",
          :target_message=>"{{user}} disrupts your lower back.",
          :spectator_message=>"{{user}} disrupts {{target}}'s lower back.",
          :effects=>%{
            :stun=>5.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>5.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.04,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"You inflict a minor groin injury on {{target}}. OOOOOUCH.",
          :target_message=>"{{user}} inflicts a minor groin injury on you. OOOOOUCH.",
          :spectator_message=>"{{user}} inflicts a minor groin injury on {{target}}. OOOOOUCH.",
          :effects=>%{
            :stun=>5.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>5.0
              }
            ],
            :damage=>0.1,
            :damage_over_time=>%{
              :damage=>0.04,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"Your disrupting strike severs {{target}}'s arm at the elbow!",
          :target_message=>"{{user}}'s disrupting strike severs your arm at the elbow!",
          :spectator_message=>"{{user}}'s disrupting strike severs {{target}}'s arm at the elbow!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"arm"
              }
            ],
            :stun=>8.0,
            :stat_mod=>[
              %{
                :stat=>"agility",
                :amount=>-25,
                :duration=>20.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>6.0
            }
          }
        },
        %{
          :user_message=>"{{target}}'s lungs explode violently, spewing bits of ragged flesh from {{target:his/her/its}} mouth!",
          :target_message=>"Your lungs explode violently, spewing bits of ragged flesh from your mouth!",
          :spectator_message=>"{{target}}'s lungs explode violently, spewing bits of ragged flesh from {{target:his/her/its}} mouth!",
          :effects=>%{
            :damage=>2.0,
            :damage_over_time=>%{
              :damage=>0.5,
              :duration=>20.0
            }
          }
        },
        %{
          :user_message=>"You cause a blast in {{target}}'s shoulder that sends {{target:his/her/its}} arm flying into the distance!",
          :target_message=>"{{user}} causes a blast in your shoulder that sends your arm flying into the distance!",
          :spectator_message=>"{{user}} causes a blast in {{target}}'s shoulder that sends {{target:his/her/its}} arm flying into the distance!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"arm"
              }
            ],
            :stun=>12.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>24.0
              }
            ],
            :damage=>0.25,
            :damage_over_time=>%{
              :damage=>0.06,
              :duration=>6.0
            }
          }
        },
        %{
          :user_message=>"{{target}}'s arm bursts open like a balloon. Well, more like a balloon filled with red Jell-O (tm).",
          :target_message=>"Your arm bursts open like a balloon. Well, more like a balloon filled with red Jell-O (tm).",
          :spectator_message=>"{{target}}'s arm bursts open like a balloon. Well, more like a balloon filled with red Jell-O (tm).",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"arm"
              }
            ],
            :damage_over_time=>%{
              :damage=>0.06,
              :duration=>6.0
            },
            :stun=>36.0,
            :damage=>0.3
          }
        },
        %{
          :user_message=>"You disrupt {{target}}'s hip joint, causing {{target:his/her/its}} leg to be violently ripped from {{target:his/her/its}} body!",
          :target_message=>"{{user}} disrupts your hip joint, causing your leg to be violently ripped from your body!",
          :spectator_message=>"{{user}} disrupts {{target}}'s hip joint, causing {{target:his/her/its}} leg to be violently ripped from {{target:his/her/its}} body!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"leg"
              }
            ],
            :stun=>20.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>40.0
              }
            ],
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>10.0
            }
          }
        },
        %{
          :user_message=>"Your disrupting blast causes pressure to build in {{target}}'s body. Suddenly, {{target:his/her/its}} arms pop off and fly in opposing directions!",
          :target_message=>"{{user}}'s disrupting blast causes pressure to build in your body. Suddenly, your arms pop off and fly in opposing directions!",
          :spectator_message=>"{{user}}'s disrupting blast causes pressure to build in {{target}}'s body. Suddenly, {{target:his/her/its}} arms pop off and fly in opposing directions!",
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
            :stun=>10.0
          }
        },
        %{
          :user_message=>"Your blast strikes {{target}}'s chest, exploding it violently, and causing rib fragments to rain upon all bystanders.",
          :target_message=>"{{user}}'s blast strikes your chest, exploding it violently, and causing rib fragments to rain upon all bystanders.",
          :spectator_message=>"{{user}}'s blast strikes {{target}}'s chest, exploding it violently, and causing rib fragments to rain upon all bystanders.",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"You mercilessly disrupt {{target}}, rending his body into innumerable small pieces which float to the ground gently.",
          :target_message=>"{{user}} mercilessly disrupts you, rending his body into innumerable small pieces which float to the ground gently.",
          :spectator_message=>"{{user}} mercilessly disrupts {{target}}, rending his body into innumerable small pieces which float to the ground gently.",
          :effects=>%{
            :kill=>true
          }
        }
      ]
    }
  end

end
