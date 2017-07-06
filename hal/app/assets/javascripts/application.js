// This is a manifest file that'll be compiled into application.js, which will include all the files
// listed below.
//
// Any JavaScript/Coffee file within this directory, lib/assets/javascripts, or any plugin's
// vendor/assets/javascripts directory can be referenced here using a relative path.
//
// It's not advisable to add code directly here, but if you do, it'll appear at the bottom of the
// compiled file. JavaScript code in this file should be added after the last require_* statement.
//
// Read Sprockets README (https://github.com/rails/sprockets#sprockets-directives) for details
// about supported directives.
//
//= require rails-ujs
//= require turbolinks
//= require jquery
//= require_tree .

$(document).ready(function() {

  delay = 400;

  slider = $(".slider").slick({
    speed: delay,
    cssEase: 'linear'
  });

  moveTo = function(slider, index, changeCurrentSlide) {
    if (slider.slick("slickCurrentSlide") != index) {
      slider.slick("slickGoTo", index);
    }
    if (changeCurrentSlide) {
      window.currentSlide = index;
    }
  }

  $('#subtitle_mode_easy')  .on('click', function(e) { moveTo(slider, 0, true); });
  $('#subtitle_mode_normal').on('click', function(e) { moveTo(slider, 1, true); });
  $('#subtitle_mode_hard')  .on('click', function(e) { moveTo(slider, 2, true); });

  window.currentSlide = slider.slick("slickCurrentSlide");

  (function submitFormWhenFileSelected() {


    $("#fake-upload-button").mouseup(function () {
      // select file modal
      document.getElementById('subtitle_file').click()
    })

    $("#subtitle_file").change(function () {
      $("#fake-upload-button").addClass('is-loading');
      $("form#new_subtitle").submit();

      var doOnce = function() {
        var subtitreDownloaded = function() {
          return document.cookie.replace(/(?:(?:^|.*;\s*)subtitreDownloaded\s*\=\s*([^;]*).*$)|^.*$/, "$1");
        }
        if (subtitreDownloaded() === "true") {
          $("#fake-upload-button").removeClass('is-loading');
          document.cookie = 'subtitreDownloaded=; expires=Thu, 01 Jan 1970 00:00:01 GMT;';
          clearInterval(window.intervalId);
        }
      }
      window.intervalId = setInterval(doOnce, 1000);
    });
  }());

  (function changeMode() {
    $(".label-mode-selection").click(function () {
      $(".label-mode-selection").removeClass('is-active is-warning');
      $(this).addClass('is-active is-warning');
    });
  }());

});
