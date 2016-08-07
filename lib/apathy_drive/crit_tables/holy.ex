defmodule ApathyDrive.CritTables.Holy do
  use ApathyDrive.Crits

  def name do
    "holy"
  end

  def damage_type do
    "magical"
  end

  def crits do
    %{
      "A"=>[
        %{
          :user_message=>"You glow slightly with divine energy.",
          :target_message=>"{{user}} glows slightly with divine energy.",
          :spectator_message=>"{{user}} glows slightly with divine energy.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"{{target}} swears that angel song can be heard as your attack hits {{target:him/her/it}}.",
          :target_message=>"You swear that angel song can be heard as {{user}}'s attack hits you.",
          :spectator_message=>"{{target}} swears that angel song can be heard as {{user}}'s attack hits {{target:him/her/it}}.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"A holy aura is visible around you as you attack {{target}} with great force.",
          :target_message=>"A holy aura is visible around {{user}} as {{user:he/she/it}} attacks you with great force.",
          :spectator_message=>"A holy aura is visible around {{user}} as {{user:he/she/it}} attacks {{target}} with great force.",
          :effects=>%{
            :damage=>0.1
          }
        },
        %{
          :user_message=>"{{target}} is thrown back by the extreme might of your attack.",
          :target_message=>"You are thrown back by the extreme might of {{user}}'s attack.",
          :spectator_message=>"{{target}} is thrown back by the extreme might of {{user}}'s attack.",
          :effects=>%{
            :damage=>0.05
          }
        },
        %{
          :user_message=>"The divine power of your attack causes you to be enshrouded in a protective aura.",
          :target_message=>"The divine power of {{user}}'s attack causes {{user:him/her/it}} to be enshrouded in a protective aura.",
          :spectator_message=>"The divine power of {{user}}'s attack causes {{user:him/her/it}} to be enshrouded in a protective aura.",
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
          :user_message=>"{{target}} is disoriented by your holy attack.",
          :target_message=>"You are disoriented by {{user}}'s holy attack.",
          :spectator_message=>"{{target}} is disoriented by {{user}}'s holy attack.",
          :effects=>%{
            :stun=>5.0,
            :damage=>0.1
          }
        },
        %{
          :user_message=>"Your attack summons a divine entity to distract {{target}}, so that you may attack more vigorously.",
          :target_message=>"{{user}}'s attack summons a divine entity to distract you, so that {{user}} may attack more vigorously.",
          :spectator_message=>"{{user}}'s attack summons a divine entity to distract {{target}}, so that {{user}} may attack more vigorously.",
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
          :user_message=>"Your attack is guided by a divine force, striking {{target}} with extreme force.",
          :target_message=>"{{user}}'s attack is guided by a divine force, striking you with extreme force.",
          :spectator_message=>"{{user}}'s attack is guided by a divine force, striking {{target}} with extreme force.",
          :effects=>%{
            :damage=>0.1,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"Divine forces resulting from your attack envelop {{target}}, raise {{target:him/her/it}} to a great height, and drop {{target:him/her/it}}!",
          :target_message=>"Divine forces resulting from {{user}}'s attack envelop you, raise you to a great height, and drop you!",
          :spectator_message=>"Divine forces resulting from {{user}}'s attack envelop {{target}}, raise {{target:him/her/it}} to a great height, and drop {{target:him/her/it}}!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"Your divine attack summons forces which serve to improve further attacks.",
          :target_message=>"{{user}}'s divine attack summons forces which serve to improve further attacks.",
          :spectator_message=>"{{user}}'s divine attack summons forces which serve to improve further attacks.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"{{target}} is sticken violently by your holy attack.",
          :target_message=>"You are sticken violently by {{user}}'s holy attack.",
          :spectator_message=>"{{target}} is sticken violently by {{user}}'s holy attack.",
          :effects=>%{
            :damage=>0.25,
            :stun=>2.0
          }
        },
        %{
          :user_message=>"Your attack causes {{target}} to find religion as holy forces strike {{target:him/her/it}}.",
          :target_message=>"{{user}}'s attack causes you to find religion as holy forces strike you.",
          :spectator_message=>"{{user}}'s attack causes {{target}} to find religion as holy forces strike {{target:him/her/it}}.",
          :effects=>%{
            :damage=>0.3,
            :stat_mod=>[
              %{
                :stat=>"intellect",
                :amount=>-20,
                :duration=>10.0
              }
            ],
            :stun=>2.0
          }
        },
        %{
          :user_message=>"You are driven to strike {{target}} again by holy forces.",
          :target_message=>"{{user}} ares driven to strike you again by holy forces.",
          :spectator_message=>"{{user}} ares driven to strike {{target}} again by holy forces.",
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
          :user_message=>"Holy forces summoned by your attack throw {{target}} a great distance backward.",
          :target_message=>"Holy forces summoned by {{user}}'s attack throw you a great distance backward.",
          :spectator_message=>"Holy forces summoned by {{user}}'s attack throw {{target}} a great distance backward.",
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
          :user_message=>"Your conjures a dazzling beam of holy light striking off the hand that offends your.",
          :target_message=>"{{user}}'s conjures a dazzling beam of holy light striking off the hand that offends {{user}}'s.",
          :spectator_message=>"{{user}}'s conjures a dazzling beam of holy light striking off the hand that offends {{user}}'s.",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"arm"
              }
            ],
            :damage=>0.3,
            :stun=>6.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-50,
                :duration=>10.0
              },
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>12.0
              }
            ]
          }
        }
      ],
      "B"=>[
        %{
          :user_message=>"You are imbued with divine force momentarily.",
          :target_message=>"{{user}} ares imbued with divine force momentarily.",
          :spectator_message=>"{{user}} ares imbued with divine force momentarily.",
          :effects=>%{

          }
        },
        %{
          :user_message=>"Your attack, aided by holy force, unbalances {{target}}.",
          :target_message=>"{{user}}'s attack, aided by holy force, unbalances you.",
          :spectator_message=>"{{user}}'s attack, aided by holy force, unbalances {{target}}.",
          :effects=>%{
            :stun=>2.0
          }
        },
        %{
          :user_message=>"Your attack is guided by divine forces, much to {{target}}'s consternation!",
          :target_message=>"{{user}}'s attack is guided by divine forces, much to your consternation!",
          :spectator_message=>"{{user}}'s attack is guided by divine forces, much to {{target}}'s consternation!",
          :effects=>%{
            :damage=>0.1,
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
          :user_message=>"Rays of divine energy from your attack strike {{target}} with great force.",
          :target_message=>"Rays of divine energy from {{user}}'s attack strike you with great force.",
          :spectator_message=>"Rays of divine energy from {{user}}'s attack strike {{target}} with great force.",
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
          :user_message=>"You strike {{target}} with great force, as your attack is guided by divine influence.",
          :target_message=>"{{user}} strikes you with great force, as {{user:his/her/its}} attack is guided by divine influence.",
          :spectator_message=>"{{user}} strikes {{target}} with great force, as {{user:his/her/its}} attack is guided by divine influence.",
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
          :user_message=>"Your attack is imbued with heavenly force, and it knocks {{target}} flat.",
          :target_message=>"{{user}}'s attack is imbued with heavenly force, and it knocks you flat.",
          :spectator_message=>"{{user}}'s attack is imbued with heavenly force, and it knocks {{target}} flat.",
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
          :user_message=>"Your holy attack calls a small bolt of lightning from the sky to strike {{target}}.",
          :target_message=>"{{user}}'s holy attack calls a small bolt of lightning from the sky to strike you.",
          :spectator_message=>"{{user}}'s holy attack calls a small bolt of lightning from the sky to strike {{target}}.",
          :effects=>%{
            :damage=>0.4
          }
        },
        %{
          :user_message=>"Your attack is guided with divine force, and {{target}} is thrown violently to the ground, cracking a few ribs.",
          :target_message=>"{{user}}'s attack is guided with divine force, and you are thrown violently to the ground, cracking a few ribs.",
          :spectator_message=>"{{user}}'s attack is guided with divine force, and {{target}} is thrown violently to the ground, cracking a few ribs.",
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
          :user_message=>"A plague of sores break out on {{target}} as holy forces come to aid your",
          :target_message=>"A plague of sores break out on you as holy forces come to aid {{user}}'s",
          :spectator_message=>"A plague of sores break out on {{target}} as holy forces come to aid {{user}}'s",
          :effects=>%{
            :damage=>0.5,
            :stun=>3.0,
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>10.0
            },
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-50,
                :duration=>20.0
              },
              %{
                :skill=>"dodge",
                :amount=>-15,
                :duration=>10.0
              },
              %{
                :skill=>"parry",
                :amount=>-20,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"Your attack is instilled with divine might, and you crush {{target}}!",
          :target_message=>"{{user}}'s attack is instilled with divine might, and {{user:he/she/it}} crushes you!",
          :spectator_message=>"{{user}}'s attack is instilled with divine might, and {{user:he/she/it}} crushes {{target}}!",
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
            ]
          }
        },
        %{
          :user_message=>"Your holy efforts seen in heaven, you are instilled with strength!",
          :target_message=>"{{user:his/her/its}} holy efforts seen in heaven, {{user}} ares instilled with strength!",
          :spectator_message=>"{{user:his/her/its}} holy efforts seen in heaven, {{user}} ares instilled with strength!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"Your smites {{target}} with Inhuman prowess.",
          :target_message=>"{{user}}'s smites you with Inhuman prowess.",
          :spectator_message=>"{{user}}'s smites {{target}} with Inhuman prowess.",
          :effects=>%{
            :damage=>0.7,
            :stun=>4.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-30,
                :duration=>10.0
              },
              %{
                :skill=>"dodge",
                :amount=>-30,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"You strike {{target}}'s arm with holy force, smashing it and rendering it useless!",
          :target_message=>"{{user}} strikes your arm with holy force, smashing it and rendering it useless!",
          :spectator_message=>"{{user}} strikes {{target}}'s arm with holy force, smashing it and rendering it useless!",
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
          :user_message=>"With a strength that {{target}} thought impossible, you strike {{target:him/her/it}}, throwing {{target:him/her/it}} effortlessly to the ground in a bloody mess.",
          :target_message=>"With a strength that you thought impossible, {{user}} strikes you, throwing you effortlessly to the ground in a bloody mess.",
          :spectator_message=>"With a strength that {{target}} thought impossible, {{user}} strikes {{target:him/her/it}}, throwing {{target:him/her/it}} effortlessly to the ground in a bloody mess.",
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
          :user_message=>"Roiling thunder splits {{target}} like a kindling at your's request.",
          :target_message=>"Roiling thunder splits you like a kindling at {{user}}'s's request.",
          :spectator_message=>"Roiling thunder splits {{target}} like a kindling at {{user}}'s's request.",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "C"=>[
        %{
          :user_message=>"You glow with heavenly force.",
          :target_message=>"{{user}} glows with heavenly force.",
          :spectator_message=>"{{user}} glows with heavenly force.",
          :effects=>%{
            :damage=>0.05
          }
        },
        %{
          :user_message=>"You summon holy energies, allowing you to attack with greater power",
          :target_message=>"{{user}} summons holy energies, allowing {{user:him/her/it}} to attack with greater power",
          :spectator_message=>"{{user}} summons holy energies, allowing {{user:him/her/it}} to attack with greater power",
          :effects=>%{
            :damage=>0.1
          }
        },
        %{
          :user_message=>"You seem to go into a flurry momentarily, attacking repeatedly!",
          :target_message=>"{{user}} seems to go into a flurry momentarily, attacking repeatedly!",
          :spectator_message=>"{{user}} seems to go into a flurry momentarily, attacking repeatedly!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"Your attack calls forth divine powers, which cause festering wounds to appear on {{target}}!",
          :target_message=>"{{user}}'s attack calls forth divine powers, which cause festering wounds to appear on you!",
          :spectator_message=>"{{user}}'s attack calls forth divine powers, which cause festering wounds to appear on {{target}}!",
          :effects=>%{
            :damage_over_time=>%{
              :damage=>0.2,
              :duration=>5.0
            },
            :stun=>3.0,
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
          :user_message=>"A fiery cross sears itself into {{target}}'s forehead!",
          :target_message=>"A fiery cross sears itself into you's forehead!",
          :spectator_message=>"A fiery cross sears itself into {{target}}'s forehead!",
          :effects=>%{
            :damage=>0.5,
            :stun=>6.0,
            :stat_mod=>[
              %{
                :stat=>"strength",
                :amount=>-100,
                :duration=>20.0
              }
            ]
          }
        },
        %{
          :user_message=>"Heavenly lights called forth by your attack dazzle and confuse {{target}}.",
          :target_message=>"Heavenly lights called forth by {{user}}'s attack dazzle and confuse you.",
          :spectator_message=>"Heavenly lights called forth by {{user}}'s attack dazzle and confuse {{target}}.",
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
          :user_message=>"The divine force of your attack sends {{target}} flying!",
          :target_message=>"The divine force of {{user}}'s attack sends you flying!",
          :spectator_message=>"The divine force of {{user}}'s attack sends {{target}} flying!",
          :effects=>%{
            :damage=>0.35,
            :stun=>2.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>4.0
              },
              %{
                :skill=>"attack",
                :amount=>-75,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}}'s eyes glow as holy force as your attack enshrouds {{target:him/her/it}}, buffetting {{target:him/her/it}} wildly.",
          :target_message=>"Your eyes glow as holy force as {{user}}'s attack enshrouds you, buffetting you wildly.",
          :spectator_message=>"{{target}}'s eyes glow as holy force as {{user}}'s attack enshrouds {{target:him/her/it}}, buffetting {{target:him/her/it}} wildly.",
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
          :user_message=>"{{target}} is torn asunder by holy forces!",
          :target_message=>"You is torn asunder by holy forces!",
          :spectator_message=>"{{target}} is torn asunder by holy forces!",
          :effects=>%{
            :damage=>1.0,
            :stun=>6.0,
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
          :user_message=>"Your attack strikes {{target}} in the leg, and the force is so great that {{target:his/her/its}} leg is crippled!",
          :target_message=>"{{user}}'s attack strikes you in the leg, and the force is so great that your leg is crippled!",
          :spectator_message=>"{{user}}'s attack strikes {{target}} in the leg, and the force is so great that {{target:his/her/its}} leg is crippled!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"cripple",
                :limb=>"leg"
              }
            ],
            :damage=>0.3,
            :stun=>2.0,
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"{{target}} is knocked violently back by your attack, hitting several inconveniently sharp objects while flying back.",
          :target_message=>"You are knocked violently back by {{user}}'s attack, hitting several inconveniently sharp objects while flying back.",
          :spectator_message=>"{{target}} is knocked violently back by {{user}}'s attack, hitting several inconveniently sharp objects while flying back.",
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
          :user_message=>"{{target}} swears that angel song can be heard as your attack strikes {{target:him/her/it}} with extreme violence!",
          :target_message=>"You swear that angel song can be heard as {{user}}'s attack strikes you with extreme violence!",
          :spectator_message=>"{{target}} swears that angel song can be heard as {{user}}'s attack strikes {{target:him/her/it}} with extreme violence!",
          :effects=>%{
            :damage=>0.4,
            :damage_over_time=>%{
              :damage=>0.06,
              :duration=>4.0
            },
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-60,
                :duration=>6.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} is struck by the stigmata by your attack, and {{target:his/her/its}} hands are rendered useless.",
          :target_message=>"You are struck by the stigmata by {{user}}'s attack, and your hands are rendered useless.",
          :spectator_message=>"{{target}} is struck by the stigmata by {{user}}'s attack, and {{target:his/her/its}} hands are rendered useless.",
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
          :user_message=>"{{target}} is transfixed by a brilliant ray of light from your attack, which shocks {{target:him/her/it}} violently.",
          :target_message=>"You are transfixed by a brilliant ray of light from {{user}}'s attack, which shocks you violently.",
          :spectator_message=>"{{target}} is transfixed by a brilliant ray of light from {{user}}'s attack, which shocks {{target:him/her/it}} violently.",
          :effects=>%{
            :damage=>0.6,
            :stun=>5.0,
            :damage_over_time=>%{
              :damage=>0.15,
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
          :user_message=>"A storm of divine flame shreds {{target}} like a rag doll",
          :target_message=>"A storm of divine flame shreds you like a rag doll",
          :spectator_message=>"A storm of divine flame shreds {{target}} like a rag doll",
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
            :damage=>1.0,
            :damage_over_time=>%{
              :damage=>0.5,
              :duration=>60.0
            },
            :stun=>6.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>10.0
              },
              %{
                :skill=>"dodge",
                :amount=>-50,
                :duration=>20.0
              }
            ]
          }
        }
      ],
      "D"=>[
        %{
          :user_message=>"You attack {{target}} with great strength.",
          :target_message=>"{{user}} attacks you with great strength.",
          :spectator_message=>"{{user}} attacks {{target}} with great strength.",
          :effects=>%{
            :damage=>0.08
          }
        },
        %{
          :user_message=>"Imbued with divine energy, you attack {{target}} with extreme force.",
          :target_message=>"Imbued with divine energy, {{user}} attacks you with extreme force.",
          :spectator_message=>"Imbued with divine energy, {{user}} attacks {{target}} with extreme force.",
          :effects=>%{
            :damage=>0.15,
            :stun=>1.0
          }
        },
        %{
          :user_message=>"Holy force from your attack causes spasms in {{target}}'s legs, and {{target:he/she/it}} collapses to the ground, momentarily helpless.",
          :target_message=>"Holy force from {{user}}'s attack causes spasms in your legs, and you collapse to the ground, momentarily helpless.",
          :spectator_message=>"Holy force from {{user}}'s attack causes spasms in {{target}}'s legs, and {{target:he/she/it}} collapses to the ground, momentarily helpless.",
          :effects=>%{
            :damage=>0.15,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>4.0
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
          :user_message=>"{{target}} gets all of {{target:him/her/it}} wind knocked out by the divine force of your attack.",
          :target_message=>"You get all of you wind knocked out by the divine force of {{user}}'s attack.",
          :spectator_message=>"{{target}} gets all of {{target:him/her/it}} wind knocked out by the divine force of {{user}}'s attack.",
          :effects=>%{
            :damage=>0.2,
            :damage_over_time=>%{
              :damage=>0.03,
              :duration=>5.0
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
          :user_message=>"By the power of the Gods, {{target}} is frozen in place.",
          :target_message=>"By the power of the Gods, you is frozen in place.",
          :spectator_message=>"By the power of the Gods, {{target}} is frozen in place.",
          :effects=>%{
            :damage=>0.2,
            :stun=>8.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-50,
                :duration=>16.0
              },
              %{
                :skill=>"dodge",
                :amount=>-50,
                :duration=>16.0
              },
              %{
                :skill=>"block",
                :amount=>-50,
                :duration=>16.0
              },
              %{
                :skill=>"attack",
                :amount=>-150,
                :duration=>16.0
              }
            ]
          }
        },
        %{
          :user_message=>"{{target}} is rendered unable to breathe by the extreme energy of your attack.",
          :target_message=>"You are rendered unable to breathe by the extreme energy of {{user}}'s attack.",
          :spectator_message=>"{{target}} is rendered unable to breathe by the extreme energy of {{user}}'s attack.",
          :effects=>%{
            :stun=>5.0,
            :damage_over_time=>%{
              :damage=>0.11,
              :duration=>5.0
            }
          }
        },
        %{
          :user_message=>"A fierce beam of light punches ragged holes in {{target}}, spilling vast amounts of blood!",
          :target_message=>"A fierce beam of light punches ragged holes in you, spilling vast amounts of blood!",
          :spectator_message=>"A fierce beam of light punches ragged holes in {{target}}, spilling vast amounts of blood!",
          :effects=>%{
            :damage=>0.8,
            :damage_over_time=>%{
              :damage=>0.2,
              :duration=>6.0
            },
            :stun=>6.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-30,
                :duration=>12.0
              },
              %{
                :skill=>"dodge",
                :amount=>-30,
                :duration=>12.0
              }
            ],
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
          :user_message=>"You attack {{target}}'s legs with great force, crippling them!",
          :target_message=>"{{user}} attacks your legs with great force, crippling them!",
          :spectator_message=>"{{user}} attacks {{target}}'s legs with great force, crippling them!",
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
            :stun=>5.0
          }
        },
        %{
          :user_message=>"HOLY WRATH severely smites {{target}}",
          :target_message=>"HOLY WRATH severely smites you",
          :spectator_message=>"HOLY WRATH severely smites {{target}}",
          :effects=>%{
            :damage=>0.5
          }
        },
        %{
          :user_message=>"{{target}} is thrown violently to the ground by your attack, shattering {{target:his/her/its}} hip.",
          :target_message=>"You are thrown violently to the ground by {{user}}'s attack, shattering your hip.",
          :spectator_message=>"{{target}} is thrown violently to the ground by {{user}}'s attack, shattering {{target:his/her/its}} hip.",
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
          :user_message=>"{{target}} falls on {{target:his/her/its}} head from your attack, rendering {{target:him/her/it}} momentarily unconscious",
          :target_message=>"You falls on your head from {{user}}'s attack, rendering you momentarily unconscious",
          :spectator_message=>"{{target}} falls on {{target:his/her/its}} head from {{user}}'s attack, rendering {{target:him/her/it}} momentarily unconscious",
          :effects=>%{
            :damage=>0.4,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>8.0
            },
            :stun=>8.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-200,
                :duration=>16.0
              }
            ]
          }
        },
        %{
          :user_message=>"You attack {{target}}'s arm with the force of the heavens, crippling it!",
          :target_message=>"{{user}} attacks your arm with the force of the heavens, crippling it!",
          :spectator_message=>"{{user}} attacks {{target}}'s arm with the force of the heavens, crippling it!",
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
          :user_message=>"Your attack, guided by heavenly influence, opens a gaping wound in {{target}}'s back, and {{target:he/she/it}} bleeds profusely.",
          :target_message=>"{{user}}'s attack, guided by heavenly influence, opens a gaping wound in your back, and you bleed profusely.",
          :spectator_message=>"{{user}}'s attack, guided by heavenly influence, opens a gaping wound in {{target}}'s back, and {{target:he/she/it}} bleeds profusely.",
          :effects=>%{
            :damage=>0.55,
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>5.0
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
          :user_message=>"Your attack strikes {{target}} violently in the ribs, cracking several of them with a sickening sound.",
          :target_message=>"{{user}}'s attack strikes you violently in the ribs, cracking several of them with a sickening sound.",
          :spectator_message=>"{{user}}'s attack strikes {{target}} violently in the ribs, cracking several of them with a sickening sound.",
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
          :user_message=>"In an awe-inspiring display, your attack throws {{target}} back with unearthly force. Upon impact, {{target:he/she/it}} is instantly crushed!",
          :target_message=>"In an awe-inspiring display, {{user}}'s attack throws you back with unearthly force. Upon impact, you are instantly crushed!",
          :spectator_message=>"In an awe-inspiring display, {{user}}'s attack throws {{target}} back with unearthly force. Upon impact, {{target:he/she/it}} is instantly crushed!",
          :effects=>%{
            :kill=>true
          }
        }
      ],
      "E"=>[
        %{
          :user_message=>"You attack {{target}} with great force.",
          :target_message=>"{{user}} attacks you with great force.",
          :spectator_message=>"{{user}} attacks {{target}} with great force.",
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
          :user_message=>"Holy forces shred {{target}} with light and fire!",
          :target_message=>"Holy forces shred you with light and fire!",
          :spectator_message=>"Holy forces shred {{target}} with light and fire!",
          :effects=>%{

          }
        },
        %{
          :user_message=>"A beam of holy light from your attack transfixes {{target}}, wounding {{target:him/her/it}} grievously.",
          :target_message=>"A beam of holy light from {{user}}'s attack transfixes you, wounding you grievously.",
          :spectator_message=>"A beam of holy light from {{user}}'s attack transfixes {{target}}, wounding {{target:him/her/it}} grievously.",
          :effects=>%{
            :damage=>0.25,
            :stun=>4.0,
            :damage_over_time=>%{
              :damage=>0.15,
              :duration=>3.0
            }
          }
        },
        %{
          :user_message=>"Your attack inflicts a major wound on {{target}}'s leg.",
          :target_message=>"{{user}}'s attack inflicts a major wound on your leg.",
          :spectator_message=>"{{user}}'s attack inflicts a major wound on {{target}}'s leg.",
          :effects=>%{
            :damage=>0.3,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>4.0
            },
            :stun=>1.0
          }
        },
        %{
          :user_message=>"{{target}} is upended by your attack, landing with a sickening THUD.",
          :target_message=>"You are upended by {{user}}'s attack, landing with a sickening THUD.",
          :spectator_message=>"{{target}} is upended by {{user}}'s attack, landing with a sickening THUD.",
          :effects=>%{
            :damage=>0.3,
            :damage_over_time=>%{
              :damage=>0.05,
              :duration=>5.0
            },
            :stun=>4.0,
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
          :user_message=>"A light from the heavens illuminates you, imbuing you with strength to attack!",
          :target_message=>"A light from the heavens illuminates {{user}}, imbuing {{user:him/her/it}} with strength to attack!",
          :spectator_message=>"A light from the heavens illuminates {{user}}, imbuing {{user:him/her/it}} with strength to attack!",
          :effects=>%{
            :damage=>0.5
          }
        },
        %{
          :user_message=>"Great balls of fire crush and sear {{target}} horribly!!",
          :target_message=>"Great balls of fire crush and sear you horribly!!",
          :spectator_message=>"Great balls of fire crush and sear {{target}} horribly!!",
          :effects=>%{
            :stun=>6.0
          }
        },
        %{
          :user_message=>"A beam of holy light from your attack impales {{target}}, causing {{target:him/her/it}} to twist and convulse violently. When the conflagration is over, all that remains of {{target}} is a smoking corpse.",
          :target_message=>"A beam of holy light from {{user}}'s attack impales you, causing you to twist and convulse violently. When the conflagration is over, all that remains of you is a smoking corpse.",
          :spectator_message=>"A beam of holy light from {{user}}'s attack impales {{target}}, causing {{target:him/her/it}} to twist and convulse violently. When the conflagration is over, all that remains of {{target}} is a smoking corpse.",
          :effects=>%{
            :kill=>true
          }
        },
        %{
          :user_message=>"!!!HALLELUJAH!!!",
          :target_message=>"!!!HALLELUJAH!!!",
          :spectator_message=>"!!!HALLELUJAH!!!",
          :effects=>%{
            :damage=>0.75,
            :stun=>7.0,
            :damage_over_time=>%{
              :damage=>0.25,
              :duration=>5.0
            },
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-25,
                :duration=>10.0
              },
              %{
                :skill=>"dodge",
                :amount=>-25,
                :duration=>10.0
              }
            ]
          }
        },
        %{
          :user_message=>"The impact of your attack is so severe, that an improbable amount of blood issues from {{target:his/her/its}} mouth!",
          :target_message=>"The impact of {{user}}'s attack is so severe, that an improbable amount of blood issues from your mouth!",
          :spectator_message=>"The impact of {{user}}'s attack is so severe, that an improbable amount of blood issues from {{target:his/her/its}} mouth!",
          :effects=>%{
            :damage=>0.6,
            :damage_over_time=>%{
              :damage=>0.1,
              :duration=>6.0
            }
          }
        },
        %{
          :user_message=>"Your holy attack slams into {{target}}'s arm, crushing it effortlessly!",
          :target_message=>"{{user}}'s holy attack slams into your arm, crushing it effortlessly!",
          :spectator_message=>"{{user}}'s holy attack slams into {{target}}'s arm, crushing it effortlessly!",
          :effects=>%{
            :limb_loss=>[
              %{
                :kind=>"sever",
                :limb=>"arm"
              }
            ],
            :damage=>0.4,
            :stun=>3.0
          }
        },
        %{
          :user_message=>"The extreme force of your attack causes {{target}} to spontaneously combust!",
          :target_message=>"The extreme force of {{user}}'s attack causes you to spontaneously combust!",
          :spectator_message=>"The extreme force of {{user}}'s attack causes {{target}} to spontaneously combust!",
          :effects=>%{
            :damage=>0.4,
            :stun=>4.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>8.0
              }
            ]
          }
        },
        %{
          :user_message=>"The Gods appear and proceed to beat the brown sauce out of {{target}}",
          :target_message=>"The Gods appear and proceed to beat the brown sauce out of you",
          :spectator_message=>"The Gods appear and proceed to beat the brown sauce out of {{target}}",
          :effects=>%{
            :stun=>10.0,
            :skill_mod=>[
              %{
                :skill=>"parry",
                :amount=>-100,
                :duration=>20.0
              },
              %{
                :skill=>"dodge",
                :amount=>-100,
                :duration=>20.0
              },
              %{
                :skill=>"attack",
                :amount=>-200,
                :duration=>20.0
              }
            ],
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
            :damage=>2.0,
            :damage_over_time=>%{
              :damage=>0.25,
              :duration=>20.0
            }
          }
        },
        %{
          :user_message=>"{{target}} goes to hell. Straight to hell. Without passing go.",
          :target_message=>"You goes to hell. Straight to hell. Without passing go.",
          :spectator_message=>"{{target}} goes to hell. Straight to hell. Without passing go.",
          :effects=>%{
            :kill=>true
          }
        }
      ]
    }
  end

end
