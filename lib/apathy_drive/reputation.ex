defmodule ApathyDrive.Reputation do

  def word_for_value(value) when value >= 1000, do: "Saint"
  def word_for_value(value) when value >= 500,  do: "Good"
  def word_for_value(value) when value >= 200,  do: "Neutral"
  def word_for_value(value) when value >= 0,    do: "Seedy"
  def word_for_value(value) when value > -200,  do: "Outlaw"
  def word_for_value(value) when value > -500,  do: "Criminal"
  def word_for_value(value) when value > -1000, do: "Villian"
  def word_for_value(_),                        do: "FIEND"

  def color("Saint"),    do: "white"
  def color("Good"),     do: "white"
  def color("Neutral"),  do: "dark-cyan"
  def color("Seedy"),    do: "dark-grey"
  def color("Outlaw"),   do: "dark-red"
  def color("Criminal"), do: "dark-yellow"
  def color("Villian"),  do: "yellow"
  def color("FIEND"),    do: "red"

end
