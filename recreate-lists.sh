#!/bin/sh

set -e

export PATH=/var/lib/mailman/bin:$PATH

while read list owner
do
    newlist -q --urlhost=common-lisp.net --emailhost=common-lisp.net $list $owner $(< /dev/urandom tr -dc _A-Z-a-z-0-9 | head -c${1:-6})
    psql -d user-migration -qtA -c "select email_address from subscription where mailing_list = '$list'" | add_members -r - -w n -a n $list
done < migrated-lists.txt
