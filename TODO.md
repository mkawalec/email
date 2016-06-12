Let's do it! Here's what we have to do:

- [x] Fetch a list of all mailboxes
- [x] Get config from config.yaml
- [x] Download all messages in a mailbox
- [x] Save the messages in a DB
- [x] Create a message fetching API
- [ ] Save more info from the messages to the DB (standard headers, if
      there's an attachment and such)
- [ ] Save the contact info from the message to the DB
- [ ] Serve only the parsed headers of messages on the main API, all of
      the contents when a message is requested, an additional endpoint for
      getting a given attachment
- [ ] Integrate with GPG for automatic message decryption
- [ ] Check if postgres search is sufficient, if not, play with elastic
- [ ] Support updates of mailboxes
- [ ] Support automatic update of all the mailboxes for an account
- [ ] Support multiple accounts
- [ ] Support pipelined, streamlined updates
- [ ] Make an email-reading interface
