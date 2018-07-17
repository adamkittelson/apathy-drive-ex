import $ from "js/jquery-1.10.2.min";

$(document).ready(function () {
  show_tab(tab);
  show_race($("#race").val())
  show_class($("#class").val())
});

$("#signup_tab").on("click", function (event) {
  show_tab("signup_tab")
});

$("#signin_tab").on("click", function (event) {
  show_tab("signin_tab")
});

$("#race").on("change", function (event) {
  show_race(event.target.value)
});

$("#class").on("change", function (event) {
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
