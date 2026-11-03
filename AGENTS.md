# AGENTS.md

softphone4s is a SIP softphone library for Scala 3, on cats-effect and fs2: SIP over
UDP, TCP and TLS, registration, calls, RTP and SRTP media, messaging, subscriptions,
presence and publication. It is open source (Apache 2.0).

Read [ARCHITECTURE.md](ARCHITECTURE.md) before you change the design, and
[README.md](README.md) for what the library does. The style rules are in
[`.claude/rules/`](.claude/rules): follow them.

## Rules

1. **Protocol conformance.** ALWAYS check protocol behaviour against the RFC. NEVER
   write it from memory. Fetch the text, and cite the section in the doc comment
   (`RFC 3261 §17.1.1`). If you cannot read the part that matters, say so.
2. **Pure packages stay pure.** `model`, `codec`, `fsm`, `sdp`, `srtp`, `messaging`,
   `subscription`, `publication` and `dialog` import no `cats.effect` and no fs2
   stream, pipe or I/O. `PuritySuite` fails if one does. Protocol machines are pure
   functions `(State, Event) => (State, List[Action])`.
3. **Types.** NEVER use `throw`, `null`, `return`, `???` or a Boolean mode flag.
   NEVER add a `var` outside a marked hot loop. Failures are values (`Either`, an
   enum of errors). Make invalid values unrepresentable: opaque types, enums and
   smart constructors. Parse a number a person sets into its own type (`Attempts`,
   `Secret`, `User`).
4. **Tests.** Every change ships with tests. ALWAYS plant the bug the test is for,
   see it fail, and restore the file: a green first run proves little. A mutation
   that leaves a value unused fails to compile (`-Werror`) and proves nothing, so
   keep the value in use. Test time with `TestControl`. A flaky virtual-time test
   is a race in the code: repeat it hundreds of times in one test and look at the
   odd result.
5. **Sockets in tests.** Suites bind fixed ports and run one at a time. Before you
   add one, search for the port. NEVER sleep for a fixed time to wait for a
   socket.
6. **Build.** Run `sbt` from the repository root only, and put settings in
   `build.sbt`. `-Wunused:all -Werror` makes an unused import a build error.
   scalafmt runs on compile and re-wraps lines: read a file again before you edit
   it twice. The full check takes about five minutes, so run it in the background:

   ```
   sbt "scalafmtSbtCheck; scalafmtCheckAll; softphone4sInterop/scalafmtCheckAll; \
        softphone4s/testFull; softphone4sOtel4s/testFull; softphone4sDemo/compile; \
        softphone4sBench/compile; softphone4sInterop/Test/compile; doc"
   ```

   One suite: `sbt "softphone4s/testOnly softphone4s.presence.*"`. One test:
   `... -- --tests=.*regex.*`. A test run that times out can leave the sbt server
   spinning: check `ps` and restart sbt before you measure anything.
7. **Performance.** ALWAYS measure. Run `uptime` first, and compare allocation and
   CPU time, not latency, on a busy machine. Benchmark the call that is made, not a
   neighbour of it. The load test is `softphone4s-bench` (`LoadTest`, with a
   `silent` option); the numbers and what they showed are in
   [`softphone4s-bench/LOAD.md`](softphone4s-bench/LOAD.md). Say what you did not
   measure.
8. **Security.** Bound everything that comes off the network: sizes, counts and
   time. Decoders take any input and return a value. A new decoder goes in the fuzz
   and hostile-input suites (`softphone4s/.../robustness`). NEVER log a secret:
   `Secret` prints `***`.
9. **Documentation.** Change `README.md` and `CHANGELOG.md` (`[Unreleased]`) with a
   user-visible change, and `MIGRATION.md` with a breaking one. Document the
   non-obvious: why, limits, and what is not done. NEVER keep a running log of
   work in the repository: git has it.
10. **Interop.** `softphone4sInterop` runs Asterisk in Docker and is not in the
    default build. NEVER pull or run an image the maintainer has not approved.
11. **Language.** Write short, plain sentences. State results with the numbers, and
    say what was not verified or not done. Name people with they/them unless their
    pronouns are known.
12. **Commits.** Commit only when asked. List the paths (`git commit -- path ...`):
    NEVER `git add -A` or `git commit -a`. A commit says what changed and why.
    NEVER touch files in `.claude/` that are staged but not part of your change.
