# associated_list

An associated list based on a `Vec`, providing the usual map functionality.

The methods are purely based on the `PartialEq` implementation of the key types,
so most have a runtime characteristic of `O(n)`.

In general, you should prefer to use either a `HashMap`, or a `BTreeMap`.
The `AssocList` exists as a fallback if the key implements neither `Hash` nor `Ord`.

The crate is `#![no_std]`-compatible, but requires `alloc`.

## PartialEq-only key types

All methods only require `PartialEq` for the key, but there is a strong argument to only use key types
that are also (at least nearly) `Ord`. For example, elements associated with a `f32::NAN`
cannot be found or deleted (`PartialEq::eq` will alway return `false`).

## Features

### nightly

This feature is a collection, enabling all nightly-only features, requiring unstable features.
Currently, this activates the features [`allocator_api`](#allocator_api) and [`doc_auto_cfg`](#doc_auto_cfg).

### allocator_api

This feature enables the nightly-only feature to use a non-default allocator for the underlying `Vec`.

### doc_auto_cfg

This features enables the nightly-only feature `doc_auto_cfg` to improve the documentation
for types and methods hidden behind feature-flags.
