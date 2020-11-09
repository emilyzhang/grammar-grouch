# grammar-grouch

Reddit bot that scrapes latest reddit posts, finds comments that use "should/would/could of" instead of "should/would/could have", and replies to those comments correcting the comment author's grammar. Written mostly to learn Haskell!

## High Level Implementation

1. Get latest reddit posts from a specified subreddit.
2. Check top comments in the post for grammar errors.
3. Responds to those comments correcting the author's grammar.

## Development

Run with the following command:

```bash
cabal run grammar-grouch -- \
  --client_id "REDDIT_APP_CLIENT_ID"
  --client_secret "REDDIT_APP_CLIENT_SECRET"
```
