# Arbiter

Arbiter is an HTTP reverse proxy that can sit in front of multiple services to
compare their responses and arbitrate between differences.

Arbiter can be used to compare new implementations against a legacy one. It can
also be used to provide a single front when your team can't agree on the
language and end up implementing a service in go, rust, node and haskell.

This repo only contains some very early experiments.
