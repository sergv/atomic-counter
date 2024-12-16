# 0.1.2.3

- Remove internal ‘test-utils’ library which wasn’t supposed to be used by clients anyway but was confusing cabal dependency resolution

# 0.1.2.2

- Lower minimum supported GHC to 8.6 and base to 4.12
- Improve generated documentation
- Faster `stg_newCounterzh`

# 0.1.2.1

- Disable cmm on 32 bit x86 architecture

# 0.1.2

- Add compare and swap operation
- Add flag to force non-CMM implementation (`no-cmm`), not that users should use it normally

# 0.1.1

- On non-javascript platforms the counter is implemented directly in
  CMM instead of using singleton primitive array. Likely to occupy
  less memory and perform faster (https://github.com/sergv/atomic-counter/pull/3).

# 0.1

Initial release
