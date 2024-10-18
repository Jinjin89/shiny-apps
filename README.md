# shiny-apps

shiny applications


## build image
```
docker build -t shiny-apps .
docker run -ti --rm -p 8880:8880 shiny-apps # used for test
docker run -ti -d --rm -p 8880:8880 shiny-apps
```

## run apps

## 1. stats

`pairwise-comparison.R`: Pairwise comparison of data within the groups utilizing t-tests or Wilcoxon tests
