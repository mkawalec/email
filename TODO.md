Let's do it! Here's what we have to do:

- [x] Fetch a list of all mailboxes
- [x] Get config from config.yaml
- [x] Download all messages in a mailbox
- [x] Save the messages in a DB
- [x] Create a message fetching API
- [x] Save more info from the messages to the DB (standard headers, if
      there's an attachment and such)
- [x] Save the contact info from the message to the DB
- [x] Serve only the parsed headers of messages on the main API
- [x] references field is not parsed with stripped <
- [x] Serve all of the contents when a message is requested
- [x] Save email threads info somewhere
- [x] Serve threads on the API
- [ ] Take database conn info from an env file as well
- [ ] Support updates of mailboxes
- [ ] Parallel fetches (arbitrary parallel fetches)
- [ ] Catch errors and recover from them where needed
- [ ] Support update of all the mailboxes for an account (also of a selection)
- [ ] Watch for message changes from the server (polling and watching)
- [ ] Deal with attachments
  - [ ] Save the info on whether there are attachments (and which) to the DB
  - [ ] Serve the attachments on a separate endpoint
- [ ] Support multiple accounts
- [ ] Integrate with GPG for automatic message decryption
- [ ] Add tests
- [ ] Check if postgres search is sufficient, if not, play with elastic
- [ ] Make an email-reading interface
