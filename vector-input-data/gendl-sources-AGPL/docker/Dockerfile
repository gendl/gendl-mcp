FROM ubuntu:latest

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get -y update && apt-get -y upgrade && \
    apt-get -y install openssl libssl-dev iputils-ping telnet ghostscript wamerican && \
    rm -rf /var/cache/apt/* && apt-get clean

RUN deluser ubuntu && \
    groupadd -g 1000 gendl-user && \
    useradd -rm -s /bin/bash -u 1000 -g gendl-user -G sudo gendl-user && \
    ln -s /home /Users

RUN mkdir -p /opt/gendl && chown gendl-user:gendl-user /opt/gendl/

COPY --chown=gendl-user:gendl-user gendl-build/. /opt/gendl/
RUN rm /opt/gendl/run-gdl
COPY --chown=gendl-user:gendl-user gendl/docker/build-build/run-gendl-services /opt/gendl/run-gendl-services
COPY --chown=gendl-user:gendl-user gendl/docker/build-build/start-services.lisp /opt/gendl/gdl/start-services.lisp
COPY --chown=gendl-user:gendl-user gendl/docker/build-build/etc/rc.local /etc/rc.local
COPY --chown=gendl-user:gendl-user gendl/docker/pipeline-tests.lisp /opt/gendl/gdl/pipeline-tests.lisp
COPY --chown=gendl-user:gendl-user ./gendl/regression/ /opt/gendl-regression/

RUN ls -l /opt/ && ls -l /opt/gendl && ls -ld /home/gendl-user/ && ls -l /home/gendl-user/


USER gendl-user
WORKDIR /home/gendl-user


RUN test -e /opt/gendl/gdl/program/gdl-ccl && \
    (echo "CCL install detected, running gdl-ccl" && \
     /opt/gendl/gdl/program/gdl-ccl -n -b -e "(progn (gendl:load-quicklisp)(gdl:start-gdl\!)(load \"/opt/gendl/gdl/pipeline-tests.lisp\")) " < /dev/null ) || \
    (echo "CCL not detected, running gdl-sbcl" && \
    /opt/gendl/gdl/program/gdl-sbcl --no-sysinit --no-userinit --non-interactive --eval "(progn(gendl:load-quicklisp)(gdl:start-gdl!)(load \"/opt/gendl/gdl/pipeline-tests.lisp\")) " )



USER root
CMD /etc/rc.local







