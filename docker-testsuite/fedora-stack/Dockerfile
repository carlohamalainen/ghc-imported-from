FROM fedora
MAINTAINER Carlo Hamalainen <carlo@carlo-hamalainen.net>

RUN curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/fedora/23/fpco.repo | tee /etc/yum.repos.d/fpco.repo

RUN dnf -y install stack wget git cabal-install zlib-devel

RUN         mkdir -p /opt
WORKDIR     /opt

# RUN wget https://hackage.haskell.org/package/ghc-imported-from-0.3.0.6/ghc-imported-from-0.3.0.6.tar.gz
# RUN tar zxf ghc-imported-from-0.3.0.6.tar.gz
# WORKDIR /opt/ghc-imported-from-0.3.0.6

# RUN         git clone https://github.com/carlohamalainen/ghc-imported-from.git /opt/ghc-imported-from
# WORKDIR     /opt/ghc-imported-from

ADD         ghc-imported-from-0.3.0.6.tar.gz /opt/ghc-imported-from/
WORKDIR     /opt/ghc-imported-from/ghc-imported-from-0.3.0.6

RUN     stack setup
RUN     stack build
RUN     stack haddock
RUN     stack haddock
RUN     stack haddock
RUN     stack test

CMD /bin/bash
