# READ Airport data from Oracle DB

# DB params
usr <- Sys.getenv("PRU_APT_USR")
pwd <- Sys.getenv("PRU_APT_PWD")
dbn <- Sys.getenv("PRU_APT_DBNAME")


# NOTE: for dply/odbc see https://github.com/rstats-db/odbc#odbc
# once IT HelpDesk has fixed the Oracle drivers config.

# suppressMessages(library('DBI'))
# con <- dbConnect(odbc::odbc(),
#   driver = "SAMAD20",
#   database = dbn,
#   uid = usr,
#   pwd = pwd)

# ROracle way
suppressMessages(library("ROracle"))
suppressMessages(library("DBI"))
suppressMessages(library(dplyr))


# NOTE: to be set before you create your ROracle connection!
# See http://www.oralytics.com/2015/05/r-roracle-and-oracle-date-formats_27.html
tz <- "UTC"
Sys.setenv("TZ" = tz)
Sys.setenv("ORA_SDTZ" = "UTC")


drv <- dbDriver("Oracle")
con <- dbConnect(drv, usr, pwd, dbname = dbn)

wef <- "2017-04-03"
til <- "2017-04-04"

sqlq_flt <- "
(SELECT
  *
FROM
  SWH_FCT.FAC_APDS_FLIGHT_IR691
WHERE
  MVT_TIME_UTC >= TO_DATE('2017-02-05', 'YYYY-MM-DD')
  AND MVT_TIME_UTC < TO_DATE('2017-02-06', 'YYYY-MM-DD')
  AND AP_C_FLTID IN ('CSA190','RYR40WJ','CFE43J','AIC175','BEL3292','DLH356',
    'AFL1212','BMR1918','PGT7YA','THY10','FIN5AN','STK3832','AFR793L','PGT356',
    'SBI3355','AZI224','RYR18PB','EZY63HU','CFE1ZQ','OHY158')
)

UNION

(SELECT
  *
FROM
  SWH_FCT.FAC_APDS_FLIGHT_IR691
WHERE
  MVT_TIME_UTC >= TO_DATE('2017-02-06', 'YYYY-MM-DD')
  AND MVT_TIME_UTC < TO_DATE('2017-02-07', 'YYYY-MM-DD')
  AND AP_C_FLTID IN ('RAM984A','RYR24W','SAS342','DLH2UT','KLM70X','FIN9EM',
    'AUA308E','DLH5LF','AEA4125','CCM230P','THY2YX','DLH01W','KLM706','RYR19QK',
    'IBE3144','EZY68XR','IBK9HW','THA955','TFL684','EZY51UQ','FIN4PP','THY4BH')
);
"


query_apt <- sqlInterpolate(con, sqlq_flt, WEF = wef, TIL = til)
aptq <- dbSendQuery(con, query_flt)
apts <- fetch(fltq, n = -1)
apts <- as_tibble(flts)


dbDisconnect(con)
Sys.unsetenv("TZ")
Sys.unsetenv("ORA_SDTZ")
