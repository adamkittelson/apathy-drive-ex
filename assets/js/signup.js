import $ from "js/jquery-1.10.2.min";

$("#signup_tab").on("click", function (event) {
  $("#signup_tab").addClass("selected")
  $("#signin_tab").removeClass("selected")
  $("#signup_form").show();
  $("#signin_form").hide();
});

$("#signin_tab").on("click", function (event) {
  $("#signin_tab").addClass("selected")
  $("#signup_tab").removeClass("selected")
  $("#signin_form").show();
  $("#signup_form").hide();
});
