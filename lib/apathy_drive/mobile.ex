defprotocol ApathyDrive.Mobile do
  def attribute_at_level(mobile, attribute, level)
  def max_hp_at_level(mobile, level)
  def max_mana_at_level(mobile, level)
end
