defmodule ApathyDrive.SkillSpell do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Skill, Spell}

  schema "skills_spells" do
    belongs_to :skill, Skill
    belongs_to :spell, Spell
    field :level, :integer
  end

end
