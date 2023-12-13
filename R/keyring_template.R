# Keyring Set-Up
# If you need the keys to the database, get in touch with Liz Roten, Brandon Whited, or Sean Molloy

keyring::key_set("mts_planning_database_string")

# looks like: (DESCRIPTION=(ADDRESS_LIST ....
# paste this in, NO QUOTE MARKS around the string.

# If you are on Mac, you need these additional values

keyring::key_set("councilR.uid") # your MetC user id
keyring::key_set("councilR.pwd") # your MetC password
keyring::key_set("desktop") # path to your desktop
