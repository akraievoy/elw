while echo '------------->' && sleep 5; do {
  cd /home/anton/Documents/projects/mipsEmu;
  if sudo -u anton mvn clean package; then
    rm /var/lib/jetty/webapps/elw.war;
    cp -v web/target/web-1.3.war /var/lib/jetty/webapps/elw.war;
    mkdir -p /usr/share/jetty/elw-data/courses;
    mkdir -p /usr/share/jetty/elw-data/groups;
    chown -R jetty:adm /usr/share/jetty/elw-data;
    cp -v datapath/src/main/resources/aos-s10.json /usr/share/jetty/elw-data/courses;
    cp -v datapath/src/main/resources/group*.json /usr/share/jetty/elw-data/groups;
    service jetty stop
    rm /var/log/jetty/*;
    service jetty start
    tail -f /var/log/jetty/*.stderrout.log
  else
    break;
  fi;
} done;
