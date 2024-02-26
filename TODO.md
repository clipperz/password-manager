# TODO
latest updated on February 18th, 2024 (synched with Linear.app)

## Scala
- [ ] code cleanup 
   - [ ] replace `Task` types with more specific ones
   - [ ] review option to use StateT to avoid `proxy`, `proxy'`, `proxy''` pattern
- [ ] review path handling, in order to be Open-API compatible
- [ ] review ZIO environment setting
- [ ] adopt ZIO config for configuration management
- [ ] collect cache stats via DataDog
- [ ] fix tests to avoid sequentiality requirement
- [ ] avoid failing altogether when not finding the DataDog side pod; useful mostly for development environments


## PureScript
- [ ] PRNG
- [ ] implement OfflineProxy for offline copy
- [ ] Edit card: when saving the card, for a little while the old card content is shown, before showing the updated content
- [ ] Adding a new card when in "narrow mode" show just a purple screen; resizing the page (growing and then shrinking it again) will end up showing the correct form. This may also be just a CSS issue in disguise.


## UI
- [ ] Review `Notes` section, both in 'read mode' (Title "Notes" too big) and in 'edit mode' (`edit`/`preview` button position and label/icon)
- [ ] Edit card: when the card has no changes, the "save" button is disabled, but the cursor still show the "pointer" mode when hovering. Also, the two tones of green for enabled/disabled are very close and not easy to spot.


## Documentation
- [ ] update documentation on how to setup developer environment
- [ ] how to start DataDog side pod to collect stats
- [ ] update DataDog dashboard to use new ZIO metrics names, and to add ZIO cache stats eventually


## Errors


## New Featurs (still to be designed)
- [ ] tag "indexing" and editing
- [ ] manage card groups


----------------

# DONE

2024-01-10
- [x] Scala - Session timeout
- [x] Scala - ZIO Cache: used to implement session handling, with timeout
- [x] Scala - add generic timeout to constraint all executions
- [x] PureScript - handled 401 with automatic reconnection
- [x] PureScript - handled html export and offline copy download. The offline copy is not working, as the OfflineProxy is not complet, yet.
- [x] Documentation - update `README.md`
- [x] Documentation - add `TODO.md`

2024-01-11
- [x] Errors - `sign-up` requests work only seldomly on Firefox; it does not work with other browsers
- [x] Errors - `login` requests seem to work reliably only on Firefox; on Safari and Chrome-Dev it works only sometime
