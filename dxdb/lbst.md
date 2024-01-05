# `(import (letloop dxdb lbst))`

Log-balanced search tree is an immutable balanced binary tree that
stores bytevector key-value pairs. Key, and values can be found in all
nodes: root, internal, and leaf nodes. A node keep track of its
parent, weight, node count, and bytes count.
