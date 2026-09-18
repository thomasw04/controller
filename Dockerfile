FROM nixos/nix:2.35.2 AS build
WORKDIR /src
ENV NIX_CONFIG="experimental-features = nix-command flakes"

COPY flake.nix flake.lock controller.cabal ./
COPY app ./app
RUN nix build .#default --option sandbox false --out-link /result \
    && nix copy --no-check-sigs --to /runtime /result

FROM scratch AS runtime
COPY --from=build /runtime/nix/store /nix/store
COPY --from=build /result/bin/controller /bin/controller
ENTRYPOINT ["/bin/controller"]
