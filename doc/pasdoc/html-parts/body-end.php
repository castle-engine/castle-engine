<?php

require_once 'cge-www-configure.php';

echo '</div> <!-- .pasdoc-container -->';

// end div, just like castle_footer when $castle_sidebar is empty.
// Note: This is not included in echo_shared_body_end,
// though it is required to match echo_shared_body_begin.
echo '</div>';

echo_shared_body_end();
