{
  library(DBI)
  library(RPostgres)
  library(RPostgreSQL)
}

# ----- create connection to database -----
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "NAAED",
                      host = "localhost",
                      port = 5433,
                      user = "postgres",
                      password = rstudioapi::askForSecret("password"))
