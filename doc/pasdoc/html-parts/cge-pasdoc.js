'use strict';

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
        cgeVersions.forEach(function (version) {
            if (newVersion == version.id) {
                window.location = version.url;
            }
        });
    });

    jQuery('td.navigation').append(versionsForm);
});
