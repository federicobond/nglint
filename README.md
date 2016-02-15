nglint
======

A work-in-progress linter for nginx configuration files. Written in Haskell.


To Do
=====

 * Generate `nglint` executable on cabal install
 * Make nginx parser more resilient. Fix bugs
 * Test with QuickCheck (?)
 * Use a more flexible scheme than reverse function composition for defining rules
 * Add more rules
 * Move rule messages to an ADT
