#!/bin/bash
/usr/local/lib/python2.7/dist-packages/CouchDB-0.8-py2.7.egg/couchdb/tools/dump.py http://localhost:5984/elw-data > elw-data.txt
/usr/local/lib/python2.7/dist-packages/CouchDB-0.8-py2.7.egg/couchdb/tools/dump.py http://localhost:5984/elw-user > elw-user.txt
/usr/local/lib/python2.7/dist-packages/CouchDB-0.8-py2.7.egg/couchdb/tools/dump.py http://localhost:5984/elw-attachment > elw-attachment.txt
/usr/local/lib/python2.7/dist-packages/CouchDB-0.8-py2.7.egg/couchdb/tools/dump.py http://localhost:5984/elw-solution > elw.solution.txt
# not really required
/usr/local/lib/python2.7/dist-packages/CouchDB-0.8-py2.7.egg/couchdb/tools/dump.py http://localhost:5984/elw-auth > elw.auth.txt





