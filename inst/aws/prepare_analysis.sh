mkdir ~/n2k
git clone --depth=1 --branch=master --progress git@gitlab.com:inbo-n2k/abv.git ~/n2k/abv
docker pull inbobmk/rn2k:latest
docker run --rm -it --env-file ./env.list -v ~/n2k:/root/n2k inbobmk/rn2k:latest
Rscript -e 'source("https://raw.githubusercontent.com/inbo/abvanalysis/meetnetten/inst/aws/prepare_analysis.R")'
exit
rm --recursive --force ~/n2k/abv
