FROM debian:testing
MAINTAINER Carlo Hamalainen <carlo@carlo-hamalainen.net>

RUN         apt-get -qq update
RUN         apt-get -qqy install cabal-install ghc git wget happy

RUN         apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
RUN         echo 'deb http://download.fpcomplete.com/debian jessie main' | tee /etc/apt/sources.list.d/fpco.list

RUN         apt-get -qq update
RUN         apt-get -qqy install stack

RUN         mkdir -p /opt
WORKDIR     /opt

RUN         wget https://hackage.haskell.org/package/ghc-imported-from-0.3.0.1/ghc-imported-from-0.3.0.1.tar.gz

RUN         tar zxf ghc-imported-from-0.3.0.1.tar.gz

WORKDIR     /opt/ghc-imported-from-0.3.0.1

ENV         PATH /.cabal/bin:/opt/ghc-imported-from-0.3.0.1/.cabal-sandbox/bin:$PATH

#RUN         stack build
#RUN         stack haddock
#RUN         stack haddock
#RUN         stack haddock
#RUN         stack test

CMD /bin/bash
