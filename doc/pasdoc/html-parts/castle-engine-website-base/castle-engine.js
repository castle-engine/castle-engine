'use strict';

/* Update .download width to fit alongside the column
   with .thumbnails. No longer needed.
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
*/

/* Simple JS to show/hide something, used by convert-output.php now. */
jQuery("#toggle-details").click(function() {
  jQuery("#details").toggle(50);
});
