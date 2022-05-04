# workflow to get and save access token in .secrets fodler

# set authentication token to be stored in a folder called `.secrets` ----------
options(gargle_oauth_cache = ".secrets")

# authenticate manually
gs4_auth()

# If successful, the previous step stores a token file.
# Check that a file has been created with:
list.files(".secrets/")

# Check that the non-interactive authentication works by first deauthorizing:
gs4_deauth()

# Authenticate using token. If no browser opens, the authentication works.
gs4_auth(cache = ".secrets", email = "rademacher.tim@gmail.com")
