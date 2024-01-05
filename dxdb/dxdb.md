# `(import (dxdb))`

## `(make-dxdb) → dxdb?`

Create an empty in-memory ordered key-value store.

## `(dxdb-empty? handle) dxdb-handle? → boolean?`

Returns `#t` if the database associated with `handle` is
empty. Otherwise, returns `#f`.

## `(dxdb? object) anything? → boolean?`

Returns `#t` if `object` is an ordered key-value store object such as
returned by `make-dxdb`. Otherwise, returns `#f`.

## `(dxdb-error? object) anything? → boolean?`

Returns `#t` if `object` is an error produced by the library
`(dxdb)`. Otherwise, returns `#f`.

## `(dxdb-close dxdb) dxdb?`

Close the database `dxdb`.

## `(dxdb-cusor? object) anything? → boolean?`

Returns `#t` if `object` is a cursor object produced by
`call-with-dxdb-cursor`.

## `(dxdb-transaction? object) anything? → boolean?`

Returns `#t` if `object` is a transaction object produced by
`call-with-dxdb-transaction` or
`call-with-dxdb-transaction-read-only`.

## `(dxdb-handle? object) anything? → boolean?`

Returns `#t` if `object` satisfy one of the following predicate:

- `dxdb?`
- `dxdb-transaction?`
- `dxdb-cursor?`

## `(dxdb-key-max-length handle) dxdb-handle? → integer`

Returns the maximum length of a key bytevector in the database
associated with `handle`.

## `(dxdb-value-max-length handle) dxdb-handle? → integer`

Returns the maximum length of a value bytevector in the database
associated with `handle`.

## `(make-dxdb-parameter default) anything? → procedure?`

Returns a procedure that is a parameter bound during the extent of
transactions. transaction parameters follow the protocol:

- When a transaction parameter is called with one argument that
  satisfies `dxdb-transaction?`, it returns the current value
  associated within the ongoing transaction, that is `default` at the
  beginning of the transaction, and until the transaction parameter is
  reset.

- When a transaction parameter is called with two arguments, the first
  must satisfy the predicate `dxdb-transaction?`, the second argument
  can be anything; that will reset the object associated with the
  transaction parameter within the ongoing transaction. An immediate
  call with the same transaction object as only argument.

Calling a transaction parameter outside a transaction is an error.

## `(dxdb-parameterize ((dxdb-parameter object) ...) body ...)`

Binds `dxdb-parameter ...`  to `object ...` while evaluating `body ...`

## `(dxdb-begin-hook dxdb)`

Returns the SRFI-172 hooks associated with `dxdb`. The begin hook's
procedures are called, when a transaction is started, to the beginning
of the transaction, inside the transaction.

## `(dxdb-pre-commit-hook dxdb)`

Returns the SRFI-172 hooks associated with `dxdb`. The pre-commit
hook's procedures are called, at the end of a transaction, before
`call-with-dxdb-transaction` or `call-with-dxdb-transaction-read-only`
returns, inside the transaction.

## `(dxdb-post-commit-hook dxdb)`

Returns the SRFI-172 hooks associated with `dxdb`. The post-commit
hook's procedures are called, at the end of a transaction, before
`call-with-dxdb-transaction` or `call-with-dxdb-transaction-read-only`
returns, outside the transaction, when the transaction succeed to
commit.

## `(dxdb-rollback-hook dxdb)`

Returns the SRFI-172 hooks associated with `dxdb`. The rollback hook's
procedures are called, at the end of a transaction, before
`call-with-dxdb-transaction` or `call-with-dxdb-transaction-read-only`
exit, outside the transaction, when the transaction failed to commit.

## `(call-with-dxdb-transaction dxdb proc success failure)`

## `(call-with-dxdb-transaction-read-only dxdb proc success failure)`

## `(call-with-dxdb-cursor handle key proc) handle? bytevector? procedure?`

## `(dxdb-approximate-key-count handle)`

Returns an approximate count of keys inside the dxdb associated with
`handle`.

## `(dxdb-approximate-byte-count handle)`

Returns an approximate count of bytes inside the dxdb associated with
`handle`.

## `(dxdb-set! handle key value)`

Set, or update the object associated with `key` to `value` inside the
dxdb associated with `handle`.

The modification is only visible to other transactions when the
current transaction returns successfully.

## `(dxdb-clear! handle key [value])`

Clear the pairing, if any, that `key` has with an object inside the
dxdb associated with `handle`.

The modification is only visible to other transactions when the
current transaction returns successfully.

## `(dxdb-next cursor)`

Try to move `cursor` to the next pairing. If there is a
lexicographically a bigger key in the dxdb, returns `#t`. Otherwise,
returns `#f`. It means the cursor is at the end of the key space, and
`dxdb-key` or `dxdb-value` will return `#f`.

## `(dxdb-previous cursor)`

Try to move `cursor` to the previous pairing. If there is a
lexicographically a smaller key in the dxdb, returns `#t`. Otherwise,
returns `#f`. It means the cursor is at the beginning of the key
space, and `dxdb-key` or `dxdb-value` will return `#f`.

## `(dxdb-key cursor)`

Returns the key associated with `cursor`. Returns `#f` when `cursor`
is at the beginning, or the end of the key space.

## `dxdb-value`

Returns the value associated with `cursor`. Returns `#f` when `cursor`
is at the beginning, or the end of the key space.

## `(dxdb-query handle key [other [offset [limit]]])`

If only `handle`, and `key` are provided, returns the value associated
with `key`, or `#f` if `key` is not paired with an object.

If `other` is provided, returns a generator producing all pairs
present in the dxdb associated with `handle` between `key`, and
`other` in lexicographic order. If `key` is smaller than `other`, the
generator produce keys in ascending order, if `key` is bigger than
`other` produce keys in descending order.
