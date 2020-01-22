<?php

// Sends the GET parameter 'message' to syslog (usually /var/log/syslog).

if (!openlog("cge_logger", LOG_PID, LOG_LOCAL0)) {
  die('openlog failed');
}

$message = $_GET['message'];
if (!syslog(LOG_INFO, $message)) {
  die('syslog failed');
}

echo 'Received message, stored in syslog: <b>' . htmlspecialchars($message) . '</b>';
