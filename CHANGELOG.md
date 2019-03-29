--------------------------------------------------------------------------------
                        ^ ADD NEW CHANGES ABOVE ^
--------------------------------------------------------------------------------

CHANGELOG
=========

---- 0.0.2 / 2014-07-12 / protection-command -----------------------------------
* added protection command

---- 0.0.3 / 2014-07-12 / elixir-0-14-3 ----------------------------------------
* update for elixir 0.14.3 and misc bug fixes

---- 0.0.4 / 2014-07-22 / combat -----------------------------------------------
* combat/attacks/dodge/parry

---- 0.0.5 / 2014-08-09 / elixir-0-15 ------------------------------------------
* update to elixir 0.15.0 and phoenix master

---- 0.0.6 / 2014-08-17 / effects ----------------------------------------------
* implement crit effects

---- 0.0.7 / 2014-08-17 / block_timer ------------------------------------------
* update timer -> block_timer

---- 0.0.8 / 2014-10-06 / posession --------------------------------------------
* possess monsters

---- 0.0.9 / 2014-10-12 / level-up ---------------------------------------------
* level up spirits and monsters when they get enough exp

---- 0.0.10 / 2014-10-30 / ability-update --------------------------------------
* ability updates

---- 0.0.11 / 2014-11-02 / item-re-export --------------------------------------
* skill requirements for armour/weapons,  power requirements for monster possession

---- 0.0.12 / 2014-11-08 / spawn-monsters-with-gear-equipped -------------------
* specify equipment monsters should spawn with

---- 0.0.13 / 2014-11-08 / greetings -------------------------------------------
* greet monsters for some flavor text

---- 0.0.14 / 2014-11-09 / ask -------------------------------------------------
* implement "ask" command / start on scripts

---- 0.0.15 / 2014-11-10 / alignment -------------------------------------------
* implement monster alignment

---- 0.0.16 / 2014-11-15 / ability-export --------------------------------------
* export abilities with scripts

---- 0.0.17 / 2014-11-22 / exits -----------------------------------------------
* begin implementing exit types

---- 0.0.18 / 2014-11-22 / upgrade-phoenix -------------------------------------
* upgrade phoenix

---- 0.0.19 / 2014-11-23 / keys ------------------------------------------------
* implement using keys on doors

---- 0.0.20 / 2014-11-29 / traps -----------------------------------------------
* implement trap exits / spawn placed items

---- 0.0.21 / 2014-11-30 / special-snowflakes ----------------------------------
* limited monsters (e.g. bosses)

---- 0.0.22 / 2014-11-30 / upgrade-phoenix -------------------------------------
* update phoenix

---- 0.0.23 / 2014-12-06 / get-drop-look-items ---------------------------------
* pick up / drop / look at items

---- 0.0.24 / 2014-12-12 / exp-dev-work ----------------------------------------
* monsters train with development points, spirits can give exp to monsters / buy and sell items for exp

---- 0.0.25 / 2014-12-13 / dynamic-skill-list ----------------------------------
* dynamic skill lists at trainers

---- 0.0.26 / 2014-12-20 / basic-monster-ai ------------------------------------
* basic monster ai

---- 0.0.27 / 2014-12-26 / ditch-corpses ---------------------------------------
* remove corpses / drop items to ground on death / create severed limbs when limbs are severed

---- 0.0.28 / 2014-12-27 / darkness --------------------------------------------
* room darkness, alignment affects visibility (evil can see better in the dark, good in the light)

---- 0.0.29 / 2014-12-28 / monster-adjectives ----------------------------------
* monster adjectives for name variety

---- 0.0.30 / 2014-12-28 / add-monsters-and-items-to-where-search --------------
* '\''where'\'' now searches for monsters and items

---- 0.0.31 / 2015-01-04 / timer-refactor --------------------------------------
* refactor timers to use :erlang.start_timer, light / extinguish light sources

---- 0.0.32 / 2015-01-04 / upgrade-phoenix -------------------------------------
* upgrade phoenix to 0.7.2

---- 0.0.33 / 2015-04-04 / big-crazy-refactor ----------------------------------
* switch from gen_event to gen_server, use separate tables instead of a giant entities table

---- 0.0.34 / 2015-04-05 / simplify --------------------------------------------
* remove items / equipment, monsters don'\''t level / gain exp / train, spirits train skills which can be used when possessing a monster

---- 0.0.35 / 2015-04-11 / facebook-login --------------------------------------
* use facebook for login

---- 0.0.36 / 2015-04-16 / possession-update -----------------------------------
* make monsters responsible for spirit struct while possessed, upgrade elixir / erlang / phoenix

---- 0.0.37 / 2015-04-26 / dockerize -------------------------------------------
* dockerize / make deployable

---- 0.0.38 / 2015-05-02 / command-refactor ------------------------------------
* finish updating a few exit types

---- 0.0.39 / 2015-05-02 / merge-permanent-npcs-into-lairs ---------------------
* remove permanent npcs, use lairs instead

---- 0.0.40 / 2015-05-02 / boss-monster-regen-times ----------------------------
* monsters can be set to not respawn for a period of time after being killed

---- 0.0.41 / 2015-05-05 / factions --------------------------------------------
* lairs can be captured, hourly exp bonuses rewarded based on lair control

---- 0.0.42 / 2015-05-06 / remove-disposition ----------------------------------
* revamp monster aggro

---- 0.0.43 / 2015-05-10 / nav -------------------------------------------------
* some work on the home page

---- 0.0.44 / 2015-05-11 / upgrade-phoenix -------------------------------------
* upgrade phoenix to 0.13

---- 0.0.45 / 2015-05-16 / faction-pages ---------------------------------------
* ability lists for factions on site

---- 0.0.47 / 2015-05-17 / say -------------------------------------------------
* add say command

---- 0.0.48 / 2015-05-17 / score-updates ---------------------------------------
* add skills / defense info to score

---- 0.0.49 / 2015-05-28 / admin -----------------------------------------------
* start admin section, room editing for admins

---- 0.0.50 / 2015-08-01 / players-characters-races-classes --------------------
* just kidding about the title, add username / password auth instead

---- 0.0.51 / 2015-08-04 / upgrade-phoenix -------------------------------------
* upgrade phoenix and friends

---- 0.0.52 / 2015-11-27 / character-revamp ------------------------------------
* various and sundry

---- 0.0.53 / 2015-12-01 / crafting --------------------------------------------
* learn how to create items by absorbing them for their essence, spend essence to create items

---- 0.0.54 / 2015-12-05 / lair-refactor ---------------------------------------
* use a join table for lair monsters

---- 0.0.55 / 2015-12-16 / script-stuff ----------------------------------------
* enough textblock/script stuff to handle grave digging

---- 0.0.56 / 2015-12-17 / upgrade-phoenix -------------------------------------
* update phoenix / ecto

---- 0.0.57 / 2015-12-17 / elixir-1-2 ------------------------------------------
* upgrade to elixir 1.2(ish)

---- 0.0.58 / 2016-01-03 / deploy-stuff ----------------------------------------
* notes etc regarding releases

---- 0.0.68 / 2016-04-11 / end-of-the-world ------------------------------------
* remove world module

---- 0.0.69 / 2016-04-13 / datadog ---------------------------------------------
* add datadog metrics

---- 0.0.70 / 2016-05-31 / frequent-essence-updates ----------------------------
* update essence more frequently for players

---- 0.0.71 / 2016-07-03 / ex_admin --------------------------------------------
* use ex_admin for admin pages

---- 0.0.72 / 2016-07-30 / remove-mobile-servers-redux -------------------------
* don'\''t have genservers for monsters, make monster data part o room genservers

---- 0.0.73 / 2016-09-02 / totally-rip-off-pillars-of-eternity -----------------
* move abilities to monster templates, decided not to do the POE thing

---- 0.0.74 / 2017-07-28 / another-revamp --------------------------------------
* monsters scale to players level and about a billion other things

---- 0.0.75 / 2017-08-05 / reputation ------------------------------------------
* area reputations

---- 0.0.76 / 2017-08-05 / distillery ------------------------------------------
* switch to distillery

---- 0.0.77 / 2017-08-06 / update-elixir-and-libraries -------------------------
* update erlang / elixir / phoenix etc

---- 0.0.78 / 2017-10-28 / skill-training --------------------------------------
* skill training

---- 0.0.79 / 2017-12-27 / bring-back-crits ------------------------------------
* crits / admin dashboard stuff

---- 0.0.80 / 2018-06-19 / exp-for-skill-usage ---------------------------------
* exp for skill usage

---- 0.0.81 / 2018-07-16 / classes ---------------------------------------------
* basic class stuff etc

---- 0.0.82 / 2018-07-29 / energy ----------------------------------------------
* energy

---- 0.0.83 / 2018-09-16 / item-overhaul ---------------------------------------
* overhaul items

---- 0.0.84 / 2018-09-17 / update-gossip-and-tweak-who-list --------------------
* who list tweaks

---- 0.0.85 / 2018-10-05 / monster-things --------------------------------------
* tweak monster hp / damage, make kills grant exp / currency

---- 0.0.86 / 2018-10-14 / party-and-mob-bars ----------------------------------
* add party / mob bars

---- 0.0.87 / 2018-10-22 / room-exit-revamp ------------------------------------
* move exits into their own tables, re-add doors etc

---- 0.0.88 / 2018-10-22 / monster-roaming -------------------------------------
* monster roaming and player automation

---- 0.0.89 / 2018-10-25 / chat-history ----------------------------------------
* chat history

---- 0.0.90 / 2018-10-27 / update-gossip ---------------------------------------
* udpate gossip to 1.0.0

---- 0.0.91 / 2018-10-29 / monster-tweaks --------------------------------------
* monsters cast spells

---- 0.0.92 / 2018-10-31 / monster-attack-chance -------------------------------
* monster attack chance

---- 0.0.93 / 2018-11-03 / monster-item-drops ----------------------------------
* monsters drop items, items are cleaned up from rooms periodically

---- 0.0.94 / 2018-11-11 / spell-help ------------------------------------------
* help for spells, more info when looking at spell scrolls

---- 0.0.95 / 2018-11-14 / enchantment -----------------------------------------
* enchant items with spells

---- 0.0.96 / 2019-02-16 / alignment -------------------------------------------
* alignment stuff

---- 0.0.97 / 2019-02-16 / scroll-changes --------------------------------------
* don'\''t learn spells via scrolls, just allow single use casting (including enchantment)

---- 0.0.98 / 2019-02-18 / placed-items ----------------------------------------
* spawn placed items

---- 0.0.99 / 2019-02-18 / greet-and-ask ---------------------------------------
* greet and ask commands

---- 0.0.100 / 2019-03-16 / kill-bonuses ---------------------------------------
* bonus exp for killing 10 / 100 / 1000 etc of a monster

---- 0.0.101 / 2019-03-18 / upgrades -------------------------------------------
* upgrade erlang / elixir / phoenix

---- 0.0.102 / 2019-03-27 / scroll-reading-redux -------------------------------
* read scrolls to learn spells, forget spells

---- 0.0.103 / 2019-03-29 / dns-things -----------------------------------------
* update dns settings to point at current ip periodically (prep for when i host at home with a dynamic ip)
