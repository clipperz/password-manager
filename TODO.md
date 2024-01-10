# TODO
latest updated on January 10th, 2024

## Scala
- [ ] code cleanup 
   - [ ] replace `Task` types with more specific ones
   - [ ] review option to use StateT to avoid `proxy`, `proxy'`, `proxy''` pattern
- [ ] review path handling, in order to be Open-API compatible
- [ ] review ZIO environment setting
- [ ] adopt ZIO config for configuration management
- [ ] collect cache stats via DataDog
- [ ] fix tests to avoid sequentiality requirement


## PureScript
- [ ] PRNG
- [ ] implement OfflineProxy for offline copy


## Documentation
- [ ] update documentation on how to setup developer environment
- [ ] update DataDog dashboard to use new ZIO metrics names, and to add ZIO cache stats eventually


## Errors
- [ ] `sign-up` requests work only seldomly on Firefox; it does not work with other browsers
- [ ] `login` requests seem to work reliably only on Firefox; on Safari and Chrome-Dev it works only sometime


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
