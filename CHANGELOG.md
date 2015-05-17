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
