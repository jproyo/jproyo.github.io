---
layout: null
sitemap:
  exclude: 'yes'
---

$(document).ready(function () {
  if ($('.panel-cover').hasClass('panel-cover--collapsed')) return
  currentWidth = $('.panel-cover').width()
  if (currentWidth < 960) {
    $('.panel-cover').addClass('panel-cover--collapsed')
  } else {
    $('.panel-cover').css('max-width', '430px')
    $('.panel-cover').css('width', '40%')
  }

  if (window.location.hash && window.location.hash == '#blog') {
    $('.panel-cover').addClass('panel-cover--collapsed')
  }

  if (window.location.pathname !== '{{ site.baseurl }}/' && window.location.pathname !== '{{ site.baseurl }}/index.html') {
    $('.panel-cover').addClass('panel-cover--collapsed')
  }

  $('.btn-mobile-menu').click(function () {
    $('.navigation-wrapper').toggleClass('visible animated bounceInDown')
    $('.btn-mobile-menu__icon').toggleClass('icon-list icon-x-circle animated fadeIn')
  })

  $('.navigation-wrapper .blog-button').click(function () {
    $('.navigation-wrapper').toggleClass('visible')
    $('.btn-mobile-menu__icon').toggleClass('icon-list icon-x-circle animated fadeIn')
  })

})
