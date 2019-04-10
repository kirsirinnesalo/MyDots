// Download mail from all accounts on startup
user_pref("mail.check_all_imap_folders_for_new", true);

// Change the reply header
// 0 - No Reply-Text
// 1 - "[Author] wrote:"
// 2 - "On [date] [author] wrote:"
// 3 - User-defined reply header. Use the prefs below in conjunction with this:
user_pref("mailnews.reply_header_type", 2);

// If you set 3 for the pref above then you may set the following prefs.
//user_pref("mailnews.reply_header_authorwrote", "%s said the following");
//user_pref("mailnews.reply_header_ondate", "on %s");
//user_pref("mailnews.reply_header_separator", " ");
//user_pref("mailnews.reply_header_colon", ":");
// The end result will be [authorwrote][separator][ondate][colon]

// Date format
// 0    No date (10:23 AM)
// 1    Your system's long* date format (Friday, December 31 2003 10:23 AM)
// 2    Your system's short* date format (12/31/1999 10:23 AM)
// 3    Year and month, separated by a slash (1999/12 10:23 AM)
// 4    Abbreviated day name (Fri 10:23 AM)
user_pref("mail.ui.display.dateformat.default", 2);
//user_pref("mail.ui.display.dateformat.thisweek", 4);
user_pref("mail.ui.display.dateformat.today", 0);

// Sorting mails: http://superuser.com/questions/13518/change-the-default-sorting-order-in-thunderbird
// Sort order
// 1 = Ascending
// 2 = Descending
user_pref("mailnews.default_sort_order", 1);
// Sort type
// 22 = Thread
user_pref("mailnews.default_sort_type", 22);

//Show folder pane columns
user_pref("mail.folderpane.showColumns", true);

// Because of Outlook, don't show the invitation subject prefix
// https://bugzilla.mozilla.org/show_bug.cgi?id=1251484
user_pref("calendar.itip.useInvitationSubjectPrefixes", false);

