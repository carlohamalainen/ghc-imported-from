FROM ubuntu
MAINTAINER Carlo Hamalainen <carlo@carlo-hamalainen.net>

RUN         apt-get -qq update
RUN         apt-get -qqy install cabal-install git wget happy

RUN         apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
RUN         echo 'deb http://download.fpcomplete.com/ubuntu wily main'|sudo tee /etc/apt/sources.list.d/fpco.list

RUN         apt-get -qq update
RUN         apt-get -qqy install stack

RUN         mkdir -p /opt
WORKDIR     /opt

ADD         ghc-imported-from-0.3.0.6.tar.gz /opt/ghc-imported-from/
WORKDIR     /opt/ghc-imported-from/ghc-imported-from-0.3.0.6

ADD         build_and_test.sh /opt/ghc-imported-from/ghc-imported-from-0.3.0.6/
RUN         bash build_and_test.sh

CMD /bin/bash
