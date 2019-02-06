'use strict';

/* Change browser location to url, unless it doesn't exist,
   then jump to fallbackUrl.
   Detecting of url uses AJAX and will only work on the same domain. */
function cgeChangeLocation(url, fallbackUrl)
{
    var ajax = jQuery.ajax({
        type: "get",
        url: url,
        cache: false
    });
    ajax.done(function() {
        window.location = url;
    });
    ajax.fail(function() {
        window.location = fallbackUrl;
    });
}

/* Append a form to switch API docs between stable/unstable engine versions. */
jQuery(document).ready(function() {
    var versionsForm = jQuery(`<form class="cge-version-form form-inline">
        <div class="form-group">
            <label for="engineVersion" class="">Engine version:</label>
            <select class="form-control" id="engineVersion">
            </select>
        </div>
    </form>`);

    var versionSelect = versionsForm.find('#engineVersion');

    // add all existing versions to <select>
    cgeVersions.forEach(function (version) {
        var option = jQuery('<option>', {
            'selected': version.id == cgeCurrentVersion,
            'name': version.id,
            'html': version.id
        });
        versionSelect.append(option);
    });

    // when <select> changes, redirect to new version
    versionSelect.change(function() {
        var newVersion = jQuery(this).val();
        if (newVersion == cgeCurrentVersion) {
            return; // don't redirect to main page of current version
        }

        var newVersionUrl = '', oldVersionUrl = '';
        cgeVersions.forEach(function (version) {
            if (version.id == newVersion) {
                newVersionUrl = version.url;
            }
            if (version.id == cgeCurrentVersion) {
                oldVersionUrl = version.url;
            }
        });

        /* We want to jump to newVersionUrl now.
           Try to preserve a suffix in window.location (ignoring oldVersionUrl). */
        if (newVersionUrl !== '') {
            var completeNewVersionUrl = newVersionUrl;
            if (oldVersionUrl !== '' && window.location.href.startsWith(oldVersionUrl)) {
                completeNewVersionUrl =
                    window.location.href.replace(oldVersionUrl, newVersionUrl);
            }

            /* try to jump to completeNewVersionUrl, fallback on newVersionUrl */
            cgeChangeLocation(completeNewVersionUrl, newVersionUrl);
        }
    });

    jQuery('td.navigation').append(versionsForm);
});
