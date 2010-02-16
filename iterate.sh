while echo '------------->' && sleep 5; do {
  cd /home/anton/Documents/projects/mipsEmu;
  if sudo -u anton mvn clean package; then
    rm /var/lib/jetty/webapps/elw.war;
    cp -v web/target/web-1.3.war /var/lib/jetty/webapps/elw.war;
    cp -v datapath/src/main/resources/aos-s10.json /usr/share/jetty/elw-data/courses;
    service jetty stop
    service jetty start
    read;
  else
    break;
  fi;
} done;
