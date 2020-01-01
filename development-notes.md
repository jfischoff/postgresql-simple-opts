# 12/31/2019

- I should get this working the latest stackage nightly and then readd it to stackage.
- The new version of `postgres-options` is really a partial options. The `PartialOptions` is unnecessary.
  I'm leaving it now but I'm probably going to remove it.
- On the otherhand I haven't revisited the decision to make the `Option` type have so
  many optional fields. It seems fine. This could just have orphans..idk...or stay the way it is.
