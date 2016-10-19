defprotocol ApathyDrive.Mobile do
  def attribute_at_level(mobile, attribute, level)
  def confused(mobile, room)
  def enter_message(mobile)
  def exit_message(mobile)
  def held(mobile)
  def look_name(mobile)
  def max_hp_at_level(mobile, level)
  def max_mana_at_level(mobile, level)
  def send_scroll(mobile, html)
  def set_room_id(mobile, room_id)
end
