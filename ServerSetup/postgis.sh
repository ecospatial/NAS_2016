# http://stackoverflow.com/questions/27177327/setting-up-django-with-geodjango-support-in-aws-beanstalk-or-ec2-instance
# http://rpmfind.net/linux/rpm2html/search.php?query=lib64jpeg8
# http://rpmfind.net/linux/rpm2html/search.php?query=lib64poppler5

# install postgresql repo
sudo yum install https://download.postgresql.org/pub/repos/yum/9.5/redhat/rhel-6-x86_64/pgdg-ami201503-95-9.5-3.noarch.rpm

# add priority=1 to perfer this repo over amzn-main
#vim /etc/yum.repos.d/pgdg-95-ami201503.repo
sudo sed -i '5ipriority=1' /etc/yum.repos.d/pgdg-95-ami201503.repo

# ensure that pgdg95 repo is used
sudo yum install postgresql95-server postgresql95-contrib

# remove priority setting to perfer amzn-main
#vim /etc/yum.repos.d/pgdg-95-ami201503.repo
sudo sed -i -e '5d' /etc/yum.repos.d/pgdg-95-ami201503.repo


sudo yum --enablerepo=epel install postgis2_95 \
    ftp://rpmfind.net/linux/Mandriva/official/2010.1/x86_64/media/main/updates/lib64poppler5-0.12.4-2.1mdv2010.1.x86_64.rpm \
    ftp://rpmfind.net/linux/mageia/distrib/1/x86_64/media/core/updates/lib64jpeg8-8b-5.1.mga1.x86_64.rpm \
    https://yum.postgresql.org/9.5/redhat/rhel-6-x86_64/geos-3.5.0-1.rhel6.x86_64.rpm

sudo passwd postgres
sudo service postgresql-9.5 initdb

########### Modify pg_hbf.conf
# $ sudo vim /var/lib/pgsql9/data/pg_hba.conf
# Update the bottom of the file, which will read something like this, by default:

# # TYPE  DATABASE        USER            ADDRESS                 METHOD

# # "local" is for Unix domain socket connections only
# local   all             all                                     ident
# # IPv4 local connections:
# host    all             all             127.0.0.1/32            ident
# # IPv6 local connections:
# host    all             all             ::1/128                 ident
# To read this:

# # TYPE  DATABASE        USER            ADDRESS                 METHOD

# # "local" is for Unix domain socket connections only
# local   all             all                                     trust
# # IPv4 local connections:
# host    all             power_user      0.0.0.0/0               md5
# host    all             other_user      0.0.0.0/0               md5
# host    all             storageLoader   0.0.0.0/0               md5
# # IPv6 local connections:
# host    all             all             ::1/128                 md5
# Now that we've updated the authorization settings, we need to update PostgreSQL to enable remote connections to the database. At the command line enter:

# $ sudo vim /var/lib/pgsql9/data/postgresql.conf
# Uncomment line 59:

# #listen_addresses = 'localhost'          # what IP address(es) to listen on;
# And update the line to enable connections from any IP address:

# listen_addresses='*'
# And uncomment line 63:

# #port = 5432
# So it reads

# port = 5432

sudo service postgresql-9.5 start
sudo chkconfig postgresql-9.5 on

su postgres
/usr/pgsql-9.5/bin/psql -p 5432

# in psql
CREATE DATABASE gistest;
\connect gistest;
CREATE EXTENSION postgis;
CREATE EXTENSION postgis_topology;
SELECT postgis_full_version();
# POSTGIS="2.2.2 r14797" GEOS="3.5.0-CAPI-1.9.0 r4084" PROJ="Rel. 4.8.0, 6 March 2012" GDAL="GDAL 1.9.2, released 2012/10/08" LIBXML="2.7.6" LIBJSON="0.11" TOPOLOGY RASTER

CREATE USER power_user SUPERUSER;
ALTER USER power_user WITH PASSWORD 'powpass17';