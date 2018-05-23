/* Based on http://www.netlobo.com/javascript_get_element_id.html */
function kambi_get_element_by_id(id)
{
  if (document.getElementById)
    return document.getElementById(id); else
  if (document.all)
    return document.all[id]; else
  if (document.layers)
    return document.layers[id];
}

function kambi_toggle_display(id)
{
  var element = kambi_get_element_by_id(id);
  if (element.style.display == "none")
    element.style.display=""; else
    element.style.display="none";
}

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
