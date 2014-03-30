#!/bin/sh

set -e

export PATH=/var/lib/mailman/bin:$PATH

while read list owner
do
    psql -d user-migration -qtA -c "select email_address from subscription where mailing_list = '$list'" > members.txt
    if [ -s members.txt ]
    then
        sudo -u list newlist -q --urlhost=common-lisp.net --emailhost=common-lisp.net $list $owner \
            $(< /dev/urandom tr -dc _A-Z-a-z-0-9 | head -c${1:-6})
        add_members -r members.txt -w n -a n $list
    else
        echo $list >> no-members.txt
    fi
done < migrated-lists.txt
