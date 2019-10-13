FROM rocker/verse

RUN apt-get update && apt-get install -y \
    ranger \
    git \
    tig \ 
    && install2.r --error \
       --deps TRUE \
       data.table \
       futile.logger \
       badgecreatr \
    && ln -sf /usr/share/zoneinfo/Europe/Berlin /etc/localtime \
    && git config --global user.mail "scheer@freescience.de" \
    && git config --global user.name "m"
