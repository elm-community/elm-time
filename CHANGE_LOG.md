# 1.0.0

### Initial Release

Forked https://github.com/Bogdanp/elm-time/tree/1.4.0 and published.

# 1.0.1

### Converted to use "elm-community"

* Updated README.md
* Updated Travis configuration

# 1.0.2

### Workaround Timezone Names Anomaly

@witoldsz contributed an innovative idea to avoid a stack overflow issue
in the **core**.  Solves issue #13.

### Fixed Bug When Time Ends .000Z

This solves issue #21.

# 1.0.3

### Fixes Date.addMonths anomaly

There were failure cases when adding negative months that crossed year 0.

### Added this CHANGE_LOG

Make it easier to see what's happening.
