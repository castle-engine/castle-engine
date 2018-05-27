'use strict';

function cge_update_download_size()
{
  var thumbnails = jQuery('.thumbnails');
  if (thumbnails.length != 0) {
    var window_width = jQuery(window).width();
    var download = jQuery('.download');
    if (window_width > 600) {
      download.css('max-width', (window_width - 250) + 'px');
      //console.log('Max size of download: ', download.css('max-width'));
    } else {
      // in this case, CSS makes .thumbnails take full page width
      download.css('max-width', '');
      //console.log('Max size of download cleared');
    }
  }
}

jQuery(window).resize(function() {
  cge_update_download_size();
});

jQuery(document).ready(function() {
  cge_update_download_size();
});
