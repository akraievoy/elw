#!/bin/bash
# I did not test these right now, but they're required for the whole thing to work, AFAICS
sudo apt-get install python-dev python-setuptools
#this is done as explained at this post
#   http://www.rossbates.com/2009/07/data-migration-for-couchdb/
sudo apt-get install python-httplib2
sudo easy_install -U simplejson
sudo easy_install -U CouchDB

/usr/local/lib/python2.7/dist-packages/CouchDB-0.8-py2.7.egg/couchdb/tools/dump.py http://localhost:5984/elw-data

# oh, BTW, in case you are playing around with couchapp, this is the tool for you
#sudo easy_install -U couchapp





