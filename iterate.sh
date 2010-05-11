mkdir /usr/share/java/webapps
WA_DIR=/var/lib/jetty/webapps
while echo '------------->' && sleep 5; do {
  cd /home/anton/Documents/projects/mipsEmu;
  if sudo -u anton mvn clean package; then
    mkdir $WA_DIR;
    rm $WA_DIR/elw.war;
    rm -r /var/cache/jetty
    cp -v web/target/web-1.3.war $WA_DIR/elw.war;
    rm -r /usr/share/jetty/elw-data
    mkdir -p /usr/share/jetty/elw-data/courses;
    mkdir -p /usr/share/jetty/elw-data/groups;
    mkdir -p /usr/share/jetty/elw-data/enroll;
    cp -v elw-data/courses/aos-s10.json /usr/share/jetty/elw-data/courses;
    cp -v elw-data/groups/ka*.json /usr/share/jetty/elw-data/groups;
    cp -v elw-data/enroll/enroll*.json /usr/share/jetty/elw-data/enroll;
    chown -R jetty:adm /usr/share/jetty/elw-data;
    service jetty stop
    rm /var/log/jetty/*;
    service jetty start
    tail -f /var/log/jetty/*.stderrout.log
  else
    break;
  fi;
} done;
