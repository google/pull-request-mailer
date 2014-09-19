# pull-request-emailer

Sends a GitHub pull request as a patch series via email

**Disclaimer:** This is not an official Google product.

---

Many open-source projects (such as the Linux kernel or the [Ganeti project](https://code.google.com/p/ganeti/)) accept patches only via their mailing lists.

This tool makes it easy to integrate contributors who prefer Github pull requests into mailing list review workflows.

## Features

* Sending a pull request to a mailing list takes **no more than 1 command**.
* Optional **notification** as a pull request comment that discussion shall be continued on the mailing list (instead of in the pull request).
* **Support for force-pushes and thread tracking.** After addressing review feedback, the improved patches can be force-pushed into the pull request, and the next invocation of this tool will send the next patch series as a reply to the previous email thread.
* Optionally it's possible to run the tool in daemon mode, let it **receive GitHub
  webhooks** and automatically process them.

## Usage

### Procesing a single pull request

```
Usage: pull-request-emailer USER REPO N --to EMAIL
                            [--post-checkout-hook PROGRAM]
                            [--github-oauth-token TOKEN] [--no-thread-tracking]
                            [--discussion-location STRING]
  Sends a GitHub pull request as a patch series via email

Available options:
  -h,--help                Show this help text
  USER                     GitHub user who owns the repo containing the pull
                           request
  REPO                     Repo containing the pull request
  N                        Number of the pull request
  --to EMAIL               Email recipient
  --post-checkout-hook PROGRAM
                           A program in the cloned direcotry just after checkout
  --github-oauth-token TOKEN
                           Auth token needed to post information to the pull
                           request. You can generate one at
                           https://github.com/settings/applications
  --no-thread-tracking     Disable posting thread message ID and patch iteration
                           count into the pull request. When active, future
                           versions of the PR can not be sent as reply to the
                           created email thread
  --discussion-location STRING
                           The place where the contents of the PR are discussed
                           (as opposed to the discussion being in PR comments.
                           Example: 'the mailing list project@example.com'.
```

### Automated server mode

```
Usage: pull-request-emailer-server --to EMAIL [--post-checkout-hook PROGRAM]
                                   [--github-oauth-token TOKEN]
                                   [--no-thread-tracking]
                                   [--discussion-location STRING]
  Receive GitHub pull request webbooks and send the patch series via email

Available options:
  -h,--help                Show this help text
  --to EMAIL               Email recipient
  --post-checkout-hook PROGRAM
                           A program in the cloned direcotry just after checkout
  --github-oauth-token TOKEN
                           Auth token needed to post information to the pull
                           request. You can generate one at
                           https://github.com/settings/applications
  --no-thread-tracking     Disable posting thread message ID and patch iteration
                           count into the pull request. When active, future
                           versions of the PR can not be sent as reply to the
                           created email thread
  --discussion-location STRING
                           The place where the contents of the PR are discussed
                           (as opposed to the discussion being in PR comments.
                           Example: 'the mailing list project@example.com'.
```

## Contact

The tool has been created and is maintained by the
[Ganeti](https://code.google.com/p/ganeti/) team. Please send any questions to
the ganeti-devel@googlegroups.com mailing list (also available
[online](https://groups.google.com/forum/#!forum/ganeti-devel)).
