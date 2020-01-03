#!/bin/bash
mkdir docker
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
&&  Rscript -e 'remotes::install_github(\"inbo/n2kanalysis@inla_arguments\", dependencies = FALSE, upgrade = \"never\", keep_source = FALSE)' \
&&  Rscript -e 'remotes::install_github(\"inbo/n2kupdate@v0.1.1\", dependencies = FALSE, upgrade = \"never\", keep_source = FALSE)' \
&&  Rscript -e 'remotes::install_github(\"inbo/abvanalysis@meetnetten\", dependencies = FALSE, upgrade = \"never\", keep_source = FALSE)'" > docker/Dockerfile
cat docker/Dockerfile
docker build --pull --tag prepareabv --build-arg ssh_prv_key="$(cat ~/.ssh/id_rsa)" --build-arg ssh_pub_key="$(cat ~/.ssh/id_rsa.pub)" docker
rm dockerDockerfile

docker run --rm -v ~/n2kanalysis:/root/n2k -it prepareabv
FILE=/root/n2k/
cd $FILE
if [ -d "abv" ]; then
    echo "bestaat"
else
    git clone git@gitlab.com:inbo-n2k/abv.git
fi

mkdir analysis
R
library(abvanalysis)
repo <- git2rdata::repository("/root/n2k/abv")
base <- "/root/n2k/analysis"
script <- prepare_analysis(repo = repo, base = base, project = "abv", volume = "~/n2kanalysis:/root/n2k", dependencies = c(
    "inbo/n2khelper@v0.4.3", "inbo/n2kanalysis@inla_arguments",
    "inbo/n2kupdate@v0.1.1"
  ))
writeLines(c(script$init, script$model), "/root/n2k/analysis/abv_model.sh")
writeLines(c(script$init, script$manifest), "/root/n2k/analysis/abv.sh")
system("chmod 755 /root/n2k/analysis/*.sh")
q(save = "no")
exit
docker image rm prepareabv
