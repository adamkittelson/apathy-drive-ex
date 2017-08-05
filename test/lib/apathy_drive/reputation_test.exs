defmodule ApathyDrive.ReputationTest do
  use ExUnit.Case, async: true

  alias ApathyDrive.Reputation

  describe "Reputation.word_for_value/1" do
    test "reputations above 1000 are saints" do
      assert "Saint" == Reputation.word_for_value(1000)
      assert "Saint" == Reputation.word_for_value(2500)
    end

    test "reputations above 500 are good" do
      assert "Good" == Reputation.word_for_value(500)
      assert "Good" == Reputation.word_for_value(750)
    end

    test "reputations above 200 are neutral" do
      assert "Neutral" == Reputation.word_for_value(200)
      assert "Neutral" == Reputation.word_for_value(425)
    end

    test "reputations above 0 are seedy" do
      assert "Seedy" == Reputation.word_for_value(0)
      assert "Seedy" == Reputation.word_for_value(150)
    end

    test "reputations below 0 are outlaws" do
      assert "Outlaw" == Reputation.word_for_value(-1)
      assert "Outlaw" == Reputation.word_for_value(-199)
    end

    test "reputations below -200 are criminals" do
      assert "Criminal" == Reputation.word_for_value(-200)
      assert "Criminal" == Reputation.word_for_value(-499)
    end

    test "reputations below -500 are villians" do
      assert "Villian" == Reputation.word_for_value(-500)
      assert "Villian" == Reputation.word_for_value(-999)
    end

    test "reputations below -1000 are fiends" do
      assert "FIEND" == Reputation.word_for_value(-1000)
      assert "FIEND" == Reputation.word_for_value(-2500)
    end
  end

  test "Reputation.color/1" do
    assert "white" == Reputation.color("Saint")
    assert "white" == Reputation.color("Good")
    assert "dark-cyan" == Reputation.color("Neutral")
    assert "dark-grey" == Reputation.color("Seedy")
    assert "dark-red" == Reputation.color("Outlaw")
    assert "dark-yellow" == Reputation.color("Criminal")
    assert "yellow" == Reputation.color("Villian")
    assert "red" == Reputation.color("FIEND")
  end

end
