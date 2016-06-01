FROM ubuntu:15.10
MAINTAINER Carlo Hamalainen <carlo@carlo-hamalainen.net>

RUN         apt-get -qq update
RUN         apt-get -qqy install software-properties-common
RUN         add-apt-repository ppa:hvr/ghc
RUN         apt-get -qq update
RUN         apt-get -qqy install git wget happy zlib1g-dev
RUN         apt-get -qqy install ghc-7.10.3 ghc-7.10.3-prof ghc-7.10.3-dyn ghc-7.10.3-htmldocs cabal-install

ENV         PATH    /opt/ghc/7.10.3/bin:$PATH

RUN         mkdir -p /opt
WORKDIR     /opt

# RUN         wget https://hackage.haskell.org/package/ghc-imported-from-0.3.0.6/ghc-imported-from-0.3.0.6.tar.gz
# RUN         tar zxf ghc-imported-from-0.3.0.6.tar.gz

# RUN         git clone https://github.com/carlohamalainen/ghc-imported-from.git /opt/ghc-imported-from

ADD         ghc-imported-from-0.3.0.6.tar.gz /opt/ghc-imported-from/
WORKDIR     /opt/ghc-imported-from/ghc-imported-from-0.3.0.6

ENV         PATH /.cabal/bin:/opt/ghc-imported-from/.cabal-sandbox/bin:$PATH

ADD         build_and_test.sh /opt/ghc-imported-from/ghc-imported-from-0.3.0.6/

RUN         bash build_and_test.sh

CMD /bin/bash
