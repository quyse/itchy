FROM centos:7

RUN yum install -y docker-client
COPY itchyd /usr/bin/
CMD itchyd --config /var/itchy/itchy.yaml
