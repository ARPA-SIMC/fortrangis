#!/bin/sh

VERSION=`grep '^AC_INIT' ../configure.ac|sed -e 's/^.*, *\([^,]\+\),.*$/\1/g'`

echo "Updating version $VERSION"

scp README ../fortrangis-$VERSION.tar.gz \
    sf:/home/frs/project/fortrangis/fortrangis/
rsync -a --delete ../doc/html/ sf:/home/project-web/fortrangis/htdocs/doc

