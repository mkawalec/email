# An email client
with a name yet to be invented

## TODO

Back-end:
- [ ] Email parsing
  - [x] Parse url-encoded emails properly into unicode Texts
  - [x] Boundary parsing is most probably comically broken (perf too)
  - [x] Improve header parsing perf
  - [x] Parse MIME
  - [x] Parse base64-encoded too
  - [ ] Use global content type or encoding if local not available
  - [ ] Parse attachements into separate places
  - [ ] Write docs and tests (include some degenerate cases too)
  - [ ] Publish an email-parse library on Hackage
- [ ] Download whole inboxes
- [ ] Persist parsed email and attachements to disk
- [ ] Be able to only download updates, in an efficient manner
- [ ] Create a configuration layer using JSON (for easy config
      management)
- [ ] Discover threads in emails
- [ ] create a REST API for reading mailboxes
  - [ ] The API shall include a websocket component for pushing updates
        to the client (data-binding style)
  - [ ] Push updates into the API while downloading/updating mailboxes
- [ ] Support email decryption
- [ ] Devise an operation method for offline mode ('offline first')
- [ ] Support email sending (in UTF-8, quoted-printable encoded only)
- [ ] Support email encryption
- [ ] Support draft saving
  - [ ] Straight to DB/server
  - [ ] Encrypted
- [ ] Add an address book
  - [ ] Automatically store emails of outgoing people
  - [ ] Add an address book API
- [ ] Add search
- [ ] GPG key management


Front-end:
- [ ] Display a list of threads
  - [ ] Infinite scroll with only a certain amount of objects rendered
        at any given moment
- [ ] Display a conversation
- [ ] Support attachement saving
- [ ] Show attached images
- [ ] Email sending
- [ ] Mailbox creation
- [ ] Email movement into different mailboxes
- [ ] Search results display
- [ ] Email address autocomplete
- [ ] GPG key management
- [ ] Display a gravatar :)
- [ ] Settings window
