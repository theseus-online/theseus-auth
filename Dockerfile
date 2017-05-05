FROM frolvlad/alpine-glibc

RUN apk update && apk add gmp-dev ca-certificates
ADD .stack-work/dist/x86_64-linux*/Cabal-*/build/theseus-auth/theseus-auth /usr/local/bin/theseus-auth

CMD /usr/local/bin/theseus-auth --config /config/theseus-config.yaml
