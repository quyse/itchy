FROM centos:7

RUN yum install -y docker-client
COPY itchyd /usr/bin/
ENV LANG=en_US.UTF-8
CMD itchyd --config /var/itchy/itchy.yaml
