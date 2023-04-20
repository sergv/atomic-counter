# 0.1.2

- Add compare and swap operation
- Add flag to force non-CMM implementation (`no-cmm`), not that users should use it normally

# 0.1.1

- On non-javascript platforms the counter is implemented directly in
  CMM instead of using singleton primitive array. Likely to occupy
  less memory and perform faster (https://github.com/sergv/atomic-counter/pull/3).

# 0.1

Initial release
