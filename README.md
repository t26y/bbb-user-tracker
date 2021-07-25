# bbb-user-tracker

A simple utility to keep track of users in a BBB room. It monitors the given directory for downloaded "save-users-list" files from BBB and informs the user of changes as well as writing them to a CSV file.

## Usage
```bash
stack build
stack exec bbb-user-tracker /path/to/download/folder
```

### Example output
```
detected userlist file. waiting 1s for download
Processed userlist from 25.7.2021:11:59:10
These users joined:
Guest a

These users reconnected:
Geust b

These users left:
Guest c

These users left previously:
Guest d, last seen 25.7.2021:11:56:07
```

## State
The tool tracks its state in the file `user-tracker-state.txt`. Do not edit this file manually. It will look for this file and re-use it if it is present, which means you can stop and re-start the tool to continue monitoring the same session.
If you want to monitor a new session, **it is very important to delete the state file** (`user-tracker-state.txt`), otherwise you will get incorrect results.

## CSV
Any changes in users are logged to `user-tracker-events.csv`. It is appended to if the file is already present at startup, but if you start a new session, it is still recommended to delete or move the old CSV file.

The timestamps are only accurate if userlists are immediately downloaded when the active users change.

## Tips
- use this Greasemonkey script to automatically download userlists when users join or leave: https://gist.github.com/t26y/2034aa3eb057e55d61e9df01763bbdf9
  - you can disable download confirmations in your browser to make this even simpler