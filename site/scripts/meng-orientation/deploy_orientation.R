#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Deploy the M.Eng. orientation deck to Posit Connect.
#
# The deck is a single self-contained HTML file. It is served two ways:
#   1. GitHub Pages  -> https://timothyfraser.com/meng/orientation.html
#      (automatic: site/public/** is copied verbatim by the site build)
#   2. Posit Connect -> a vanity path on connect.systems-apps.com
#      (this script; use it when you want the deck behind Connect, e.g. to
#       restrict access to enrolled students, or to keep a versioned history
#       with rollback via Connect -> Source Versions)
#
# Usage, from the repo root:
#   Rscript site/scripts/meng-orientation/deploy_orientation.R
#
# Requires two environment variables (never commit their values):
#   CONNECT_SERVER   e.g. https://connect.systems-apps.com
#   CONNECT_API_KEY  a key from a PUBLISHER-role Connect account
#
# Gotcha, learned the hard way on other repos in this lab: rsconnect's *stored*
# credentials and any profile-exported CONNECT_* vars go stale silently, and the
# symptom is a 403 that looks like a code problem. Source your .env explicitly
# before running this, and let the check below fail loudly if it is unset.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages(library(rsconnect))

server <- Sys.getenv("CONNECT_SERVER")
key    <- Sys.getenv("CONNECT_API_KEY")

if (!nzchar(server) || !nzchar(key)) {
  stop(
    "CONNECT_SERVER and CONNECT_API_KEY must both be set.\n",
    "  Set them in a gitignored .env and source it, or export them in this shell.\n",
    "  The key must come from a Publisher-role Connect account; a Viewer key 403s.",
    call. = FALSE
  )
}

# --- config ---------------------------------------------------------------
app_dir    <- "site/public/meng"        # bundle root
entry      <- "orientation.html"        # the deck
app_name   <- "meng-orientation"        # Connect content name / vanity path
app_title  <- "M.Eng. Orientation — Fall 2026"

stopifnot(file.exists(file.path(app_dir, entry)))

message("=== Deploying ", entry, " to ", server, " ===")

# Only ship the deck itself. Without this filter the whole /meng folder
# (syllabus, translations, the recruiting deck, PDFs) rides along.
files <- entry

rsconnect::addServer(url = paste0(sub("/$", "", server), "/__api__"),
                     name = "connect", quiet = TRUE)
rsconnect::connectApiUser(account = "publisher", server = "connect", apiKey = key)

rsconnect::deployApp(
  appDir       = app_dir,
  appFiles     = files,
  appPrimaryDoc = entry,
  appName      = app_name,
  appTitle     = app_title,
  account      = "publisher",
  server       = "connect",
  forceUpdate  = TRUE,
  launch.browser = FALSE
)

message("=== Done ===")
message("Set the vanity path to /", app_name, "/ in Connect -> Settings -> Content URL.")
message("Set access: 'Anyone - no login required' for a public deck, or restrict to")
message("the student group if you want it behind Connect auth.")
