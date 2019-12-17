#!/bin/bash
echo "FROM inbobmk/rn2k:0.6
ARG ssh_prv_key
ARG ssh_pub_key

RUN apt-get update  \
&&  apt-get install -y --no-install-recommends \
        openssh-server

# Authorize SSH Host
RUN mkdir -p /root/.ssh \
&&  chmod 0700 /root/.ssh \
&&  ssh-keyscan gitlab.com > /root/.ssh/known_hosts

# Add the keys and set permissions
RUN echo \"\$ssh_prv_key\" > /root/.ssh/id_rsa \
&&  echo \"\$ssh_pub_key\" > /root/.ssh/id_rsa.pub \
&&  chmod 600 /root/.ssh/id_rsa \
&&  chmod 600 /root/.ssh/id_rsa.pub

RUN Rscript -e 'remotes::install_github(\"inbo/n2khelper@v0.4.3\", dependencies = FALSE, upgrade = \"never\", keep_source = FALSE)' \
&&  Rscript -e 'remotes::install_github(\"inbo/n2kanalysis@v0.2.8\", dependencies = FALSE, upgrade = \"never\", keep_source = FALSE)' \
&&  Rscript -e 'remotes::install_github(\"inbo/n2kupdate@v0.1.1\", dependencies = FALSE, upgrade = \"never\", keep_source = FALSE)' \
&&  Rscript -e 'remotes::install_github(\"inbo/abvanalysis@meetnetten\", dependencies = FALSE, upgrade = \"never\", keep_source = FALSE)'" > Dockerfile
cat Dockerfile
docker build --pull --tag prepareabv --build-arg ssh_prv_key="$(cat ~/.ssh/id_rsa)" --build-arg ssh_pub_key="$(cat ~/.ssh/id_rsa.pub)" .
rm Dockerfile

docker run --rm -v ~/n2kdocker:/root/n2k -it prepareabv
cd /root/n2k
git clone git@gitlab.com:inbo-n2k/abv.git
mkdir analysis
R
library(abvanalysis)
repo <- git2rdata::repository("/root/n2k/abv")
base <- "/root/n2k/analysis"
script <- prepare_analysis(repo = repo, base = base, project = "abv", volume = "~/n2kdocker:/root/n2k")
writeLines(c(script$init, script$model), "/root/n2k/analysis/abv_model.sh")
writeLines(c(script$init, script$manifest), "/root/n2k/analysis/abv.sh")
system("chmod 755 /root/n2k/analysis/*.sh")
q(save = "no")
exit
docker image rm prepareabv
