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
                      user = Sys.getenv("POSTGRES_USER"),
                      password = Sys.getenv("POSTGRES_PASSWORD")
                      )
