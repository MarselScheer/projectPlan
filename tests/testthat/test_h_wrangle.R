context("Wrangle")
library(projectPlan)

dt <- readxl::read_xlsx("../../kaggle/xlsx_2_gantt.rep/prjplan.xlsx", sheet = "RoughPlan")

futile.logger::flog.threshold(futile.logger::TRACE)

h.rd_wrangle(dt)
