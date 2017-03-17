FROM ubuntu

ADD .stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/theseus-auth/theseus-auth /usr/local/bin/theseus-auth

CMD /usr/local/bin/theseus-auth
