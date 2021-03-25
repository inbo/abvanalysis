docker pull inbobmk/rn2k:latest
docker run --rm --env-file ./env.list -v ~/n2k:/root/n2k inbobmk/rn2k:latest Rscript -e 'source("https://raw.githubusercontent.com/inbo/abvanalysis/meetnetten/inst/aws/store_parameters.R")'
