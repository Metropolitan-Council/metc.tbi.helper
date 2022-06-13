# Keyring Set up
# If you need the keys to the database, get in touch with Ashley Asmus or Sean Molloy

keyring::key_set("mts_planning_database_string")
# looks like: (DESCRIPTION=(ADDRESS_LIST ....
# paste this in, NO QUOTE MARKS around the string.

keyring::key_set("mts_planning_data_pw")
# type this in, NO QUOTES!

# If you are on Mac, you need these additional values

keyring::key_set("MetC") # your MetC pwd

keyring::key_set("MetC_uid") # your MetC user id. should be "mc\\uid"
# if you are having difficulty getting the "\\" to behave, try
keyring::key_set_with_value(service = "MetC_uid", password = "mc\\{your_uid}")
