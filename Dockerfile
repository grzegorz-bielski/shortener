FROM haskell:8.4 as builder

WORKDIR /app

# env and dependencies
RUN stack upgrade
RUN stack update
RUN stack setup --resolver lts-13.19

COPY stack.yaml .
COPY package.yaml .
RUN stack build --dependencies-only

# app
COPY . .
RUN stack install --ghc-options -DDOCKERIZED

## runtime
FROM fpco/stack-run:lts

WORKDIR /app

COPY --from=builder /root/.local/bin/shortener-exe /app/shortener-exe

CMD ["/app/shortener-exe"]