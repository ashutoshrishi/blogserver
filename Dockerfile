FROM haskell

RUN cabal update

ADD ./blogserver.cabal /opt/server/blogserver.cabal
RUN cd /opt/server && cabal install --only-dependencies -j4


# Add and Install Application Code
ADD . /opt/server
RUN cd /opt/server && cabal install

# Add installed cabal executables to PATH
ENV PATH /root/.cabal/bin:$PATH

# Default Command for Container
WORKDIR /opt/server



