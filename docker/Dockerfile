FROM ubuntu:14.10

MAINTAINER Tobias Waaler <tobiasgwaaler+docker@gmail.com>

# Install cabal and ghc
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F6F88286
RUN echo 'deb     http://ppa.launchpad.net/hvr/ghc/ubuntu trusty main' >> /etc/apt/sources.list.d/haskell.list
RUN echo 'deb-src http://ppa.launchpad.net/hvr/ghc/ubuntu trusty main' >> /etc/apt/sources.list.d/haskell.list
RUN apt-get update

RUN apt-get install -y ghc-7.8.3 cabal-install-1.20 

# symlink ghc and cabal into /usr/local/bin
RUN ln -s /opt/ghc/7.8.3/bin/* /usr/local/bin
RUN ln -s /opt/cabal/1.20/bin/* /usr/local/bin

RUN apt-get -y install emacs vim
RUN apt-get -y install git

# Create a potentially shared folder 
VOLUME /haskell/shared

RUN cabal update

WORKDIR /haskell
RUN git clone https://github.com/haskell/haskell-mode.git
WORKDIR /haskell/haskell-mode
RUN apt-get -y install make texinfo install-info
RUN make EMACS=/usr/bin/emacs

ADD dotemacs /root/.emacs