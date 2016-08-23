# Blogserver

This is a [Scotty](https://hackage.haskell.org/package/scotty) based REST API
server which interacts with a PostgreSQL database using
[Persistent](https://hackage.haskell.org/package/persistent), to present an
interface to read and push blog posts.

Blog posts can be provided in simple *Markdown* and other formats, as these
will be passed through different parsers to be stored in the database. The
origin of these posts can be a folder or a remote store. The idea is to keep
writing separate from the API server. 

