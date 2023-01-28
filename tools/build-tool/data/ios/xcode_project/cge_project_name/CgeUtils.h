// Define this, if you use TestFairy on iOS,
// to send NSLog (all logs from our Objective-C code) to TestFairy.
// See https://docs.testfairy.com/SDK/Remote_Logging.html#ios .
// #define SEND_NSLOG_TO_TEST_FAIRY

#ifdef SEND_NSLOG_TO_TEST_FAIRY
  #import "TestFairy.h"
  #define NSLog(s, ...) do { NSLog(s, ##__VA_ARGS__); TFLog(s, ##__VA_ARGS__); } while (0)
#endif
