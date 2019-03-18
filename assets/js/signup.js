$(document).ready(function () {
  show_tab(tab);
  show_race($("#race").val())
  show_class($("#class").val())
});

$(document).on("click", "#signup_tab", function (event) {
  show_tab("signup_tab")
});

$(document).on("click", "#signin_tab", function (event) {
  show_tab("signin_tab")
});

$(document).on("change", "#race", function (event) {
  show_race(event.target.value)
});

$(document).on("change", "#class", function (event) {
  show_class(event.target.value)
});

var show_tab = function (tab) {
  if (tab === "signup_tab") {
    $("#signup_tab").addClass("selected")
    $("#signin_tab").removeClass("selected")
    $("#signup_form").show();
    $("#signin_form").hide();
  }
  else if (tab === "signin_tab") {
    $("#signin_tab").addClass("selected")
    $("#signup_tab").removeClass("selected")
    $("#signin_form").show();
    $("#signup_form").hide();
  }
}

var show_race = function (race_id) {
  $(".races").hide()
  $("#race-" + race_id).show()
}

var show_class = function (class_id) {
  $(".classes").hide()
  $("#class-" + class_id).show()
}
