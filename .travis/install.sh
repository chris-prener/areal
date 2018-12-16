#!/bin/bash

if [[ $TRAVIS_OS_NAME == 'linux' ]]; then

  # install postgis from source, to avoid dependency conflict with GDAL >= 2.0:
  - wget http://download.osgeo.org/postgis/source/postgis-2.4.3.tar.gz
  - (mv postgis* /tmp; cd /tmp; tar xzf postgis-2.4.3.tar.gz)
  - (cd /tmp/postgis-2.4.3 ; ./configure; make; sudo make install)

  # activate liblwgeom by:
  - sudo ldconfig
  # create postgis databases:
  - sudo service postgresql restart
  - createdb postgis
  - psql -d postgis -c "CREATE EXTENSION postgis;"
  - psql -d postgis -c "GRANT CREATE ON DATABASE postgis TO travis"
  - psql -d postgis -c "GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO travis"
  - createdb empty
  - psql -d empty -c "CREATE EXTENSION postgis;"

fi
