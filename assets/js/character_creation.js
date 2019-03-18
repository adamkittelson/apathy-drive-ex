$("#race").on("change", function (event) {
  $(".races").hide()
  $("#race-" + event.target.value).show()
});

$("#class").on("change", function (event) {
  $(".classes").hide()
  $("#class-" + event.target.value).show()
});
