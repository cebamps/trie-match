# also tested: 9.8.4
ARG GHC_VERSION=9.10.1

from haskell:$GHC_VERSION-slim

WORKDIR /opt/trie-match

RUN cabal update

COPY ./trie-match.cabal /opt/trie-match/trie-match.cabal
RUN cabal build --only-dependencies -j4

COPY . /opt/trie-match
RUN cabal install

CMD ["trie-match"]
