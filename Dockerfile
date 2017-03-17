FROM frolvlad/alpine-glibc

RUN apk update && apk add gmp-dev ca-certificates
ADD .stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/theseus-auth/theseus-auth /usr/local/bin/theseus-auth

CMD /usr/local/bin/theseus-auth
