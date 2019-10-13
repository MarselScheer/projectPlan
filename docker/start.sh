#!/bin/bash

sudo docker run -d -p 8787:8787 -e DISABLE_AUTH=true -v /tmp:/home/rstudio projectplan_ide
